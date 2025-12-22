server <- function(input, output, session) {
  
  # ==== 0.1) END POINT =======================================
  send_ga_event <- function(client_id, event_name, params = list()) {
    
    secret <- Sys.getenv("ZKNkvKGbTV6504car3fmFw")
    mid    <- Sys.getenv("G-2D6B3PWVGG")
    
    url <- paste0(
      "https://www.google-analytics.com/mp/collect",
      "?measurement_id=", mid,
      "&api_secret=", secret
    )
    
    body <- list(
      client_id = client_id,
      events = list(
        list(name = event_name, params = params)
      )
    )
    
    request(url) %>%
      req_body_json(body) %>%
      req_perform()
  }
  
  
  
  pr <- plumber::pr()
  
  pr$handle("POST", "/ga", function(req, res) {
    body <- jsonlite::fromJSON(req$postBody)
    send_ga_event(body)
    return("ok")
  })
  
  session$registerDataObj("ga_proxy", pr, filter = "plumber")
  
  #print(session$getTestEndpointUrl("ga_proxy"))
  
  
  
  
  # ==== 0) CONSTANTS / INITIALIZATION =======================================
  current_tab <- reactiveVal("map_tab")
  
  observeEvent(input$tabs, {
    current_tab(input$tabs)
  })
  
  

  # Initialize Fancytree with default selection
  observeEvent(session, {
    fancytree_states_json <- get_fancytree_data_states(data)
    
    session$sendCustomMessage(
      "fancytree_states_data",
      list(
        # IMPORTANT: keep nested lists, NOT data.frame
        data = jsonlite::fromJSON(fancytree_states_json, simplifyVector = FALSE),
        default_selected = c(
          "ARGENTINA-CAPITAL FEDERAL",
          "BRAZIL-DISTRITO FEDERAL",
          "MEXICO-CDMX"
        )
      )
    )
  })
  
  
  ## fancy tree map
  
  observeEvent(session, {
    fancytree_json_vars <- get_fancytree_data_vars(
      dict %>% 
        filter(viewable_map == 1, variable != "chamber_sub_leg")
    )
    
    session$sendCustomMessage(
      "fancytree_vars_data",
      list(
        data = jsonlite::fromJSON(fancytree_json_vars, simplifyVector = FALSE),
        default_selected = list("Executive Elections-Valid Votes")
      )
    )
  })
  
  
  ## fancy tree graph
  observeEvent(session, {
    fancytree_json_vars_graph <- get_fancytree_data_vars(
      dict %>% 
        filter(viewable_graph == 1, variable != "chamber_sub_leg"),
      FALSE
    )
    
    session$sendCustomMessage(
      "fancytree_vars_data_graph",
      list(
        data = jsonlite::fromJSON(fancytree_json_vars_graph, simplifyVector = FALSE),
        default_selected = list("Executive Elections-Valid Votes")
      )
    )
  })
  
  
  
  

  # 
  
  # ==== 1) GLOBAL REACTIVES ==================================================
  # -- 1.0) states JSTree graph ----------------------

  selected_states_vector <- reactive({
    x <- input$selected_nodes_states
    if (is.null(x) || x == "" || x == "[]") return(character(0))
    
    # Fancytree sends: "COUNTRY-STATE"
    ids <- jsonlite::fromJSON(x)
    
    # Ensure vector
    if (!length(ids)) return(character(0))
    
    # Only state nodes contain "-" (countries do not)
    state_ids <- ids[grepl("-", ids)]
    
    # Extract the state part after "-"
    sapply(strsplit(state_ids, "-", fixed = TRUE), function(x) x[2])
  })
  
  
  
  
  selected_vars_vector <- reactive({
    x <- input$selected_nodes_vars2
    if (is.null(x) || x == "") return(NULL)
    
    # Fancytree sends a simple string (not a JSON array), so no fromJSON
    key <- x
    parts <- strsplit(key, "-", fixed = TRUE)[[1]]
    
    # Check SLED structure
    if (identical(parts[1], "Legislative Elections") &&
        length(parts) >= 3 &&
        parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      
      chamber <- parts[2]
      n_chamber <- ifelse(chamber == "Lower Chamber", 1, 2)
      
      pretty <- paste(parts[3:length(parts)], collapse = "-")
      
      dict %>%
        filter(pretty_name == pretty) %>%
        pull(variable) %>%
        paste0("_", n_chamber)
      
    } else {
      # Generic format: DATASET-Pretty Name
      pretty <- paste(parts[2:length(parts)], collapse = "-")
      
      dict %>%
        filter(pretty_name == pretty) %>%
        pull(variable)
    }
  })
  
  
  
  
  selected_vars_vector_graph <- reactive({
    key <- input$selected_nodes_vars_graph2
    if (is.null(key) || key == "") return(NULL)
    
    parts <- strsplit(key, "-", fixed = TRUE)[[1]]
    

    if (identical(parts[1], "Legislative Elections") &&
        length(parts) >= 3 &&
        parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      
      n_chamber <- dplyr::case_when(
        parts[2] == "Lower Chamber" ~ 1,
        parts[2] == "Upper Chamber" ~ 2
      )
      
      pretty <- paste(parts[3:length(parts)], collapse = "-")
      
      base_var <- dict %>%
        dplyr::filter(pretty_name == pretty) %>%
        dplyr::pull(variable)
      
      return(paste0(base_var, "_", n_chamber))
    }

    pretty <- paste(parts[2:length(parts)], collapse = "-")
    
    dict %>%
      dplyr::filter(pretty_name == pretty) %>%
      dplyr::pull(variable)
  })
  
  
  
  # -- 1.0) DATA MAP ---------------------
  data_map <- reactive({
    req(input$country_sel, input$year_sel)
    geom_filtered <- geom %>% dplyr::filter(country_name == input$country_sel)
    data_filtered <- data %>% dplyr::filter(country_name == input$country_sel, year == input$year_sel)
    dplyr::left_join(geom_filtered, data_filtered, by = "country_state_code")
  })
  
  
  
  # ==== 1.3) CAMERA TAB SELECTORS (SLED-driven) ==============================
  # UI renderers (camera-only)
  output$country_selector_camera <- renderUI({
    selectInput(
      "country_sel_camera", "Country",
      choices  = sort(unique(SLED$country_name)),
      selected = "BRAZIL"
    )
  })
  
  output$state_selector_camera <- renderUI({
    req(input$country_sel_camera)
    
    # Get original values (what you want returned)
    choices_vals <- SLED |>
      dplyr::filter(country_name == input$country_sel_camera) |>
      dplyr::pull(state_name) |>
      unique() |>
      sort()
    
    # Build labels (what you want displayed)
    choices_labs <- stringr::str_to_title(choices_vals)
    
    # Name the vector: names = labels, values = originals
    names(choices_vals) <- choices_labs
    
    selectInput(
      inputId  = "state_sel_camera",
      label    = "State",
      choices  = choices_vals,                 # shows Title Case, returns originals
      selected = if (length(choices_vals))
        choices_vals[[min(4, length(choices_vals))]]
      else NULL
    )
  })
  
  
  output$chamber_selector_camera <- renderUI({
    selectInput(
      "chamber_sel_camera", "Chamber",
      choices = c("Lower chamber" = 1, "Upper chamber" = 2),
      selected = 1
    )
  })
  
  
  # output$year_selector_camera <- renderUI({
  #   shinyWidgets::sliderTextInput(
  #     inputId  = "year_sel_camera", label = "Year",
  #     choices  = as.character(seq(1983, 2024, 1)),
  #     grid     = TRUE, width = "90%",
  #     selected = 2019,
  #     animate  = shiny::animationOptions(
  #       interval = 1000,
  #       loop = FALSE
  #     )
  #   )
  # })
  
  # Year scoping helpers (camera)
  sled_years_scoped_camera <- reactive({
    df <- SLED
    
    # country filter
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    
    # state filter
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }
    
    # chamber filter  ← THIS WAS MISSING
    # if (!is.null(input$chamber_sel_camera) && nzchar(input$chamber_sel_camera)) {
    #   df <- df[df$chamber_election_sub_leg == input$chamber_sel_camera, , drop = FALSE]
    # }
    
    
    sort(unique(df$year))
    
  })
  
  
  # Keep year slider in sync (camera)
  observeEvent(
    list(current_tab(), input$country_sel_camera, input$state_sel_camera, input$chamber_sel_camera),
    {
      req(current_tab() == "camera")
      if (is.null(input$year_sel_camera)) return()
      #shiny::invalidateLater(1, session)  # ← lets UI finish rebuilding first
      
      yrs <- sled_years_scoped_camera()
      
      # no available years → reset slider cleanly
      if (length(yrs) == 0 || all(is.na(yrs))) {
        shinyWidgets::updateSliderTextInput(
          session, "year_sel_camera",
          choices  = character(0),
          selected = NULL
        )
        return()
      }
      
      # ensure years are character for the slider
      yrs_chr <- as.character(sort(yrs))
      
      # preserve current selection *if still valid*
      current <- input$year_sel_camera
      if (!is.null(current) && current %in% yrs_chr) {
        selected_year <- current
      } else {
        # fallback to LAST valid year (as you were doing)
        selected_year <- tail(yrs_chr, 1)
      }
      
      shinyWidgets::updateSliderTextInput(
        session, "year_sel_camera",
        choices  = yrs_chr,
        selected = selected_year
      )
    },
    ignoreInit = TRUE
  )
  
  
  # output$chamber_selector_camera <- renderUI({
  #   selectInput(
  #     "chamber_sel_camera", "Chamber",
  #     choices = c("Lower chamber" = 1, "Upper chamber" = 2),
  #     selected = 1
  #   )
  # })
  
  
  # Available chambers (scoped)
  available_chambers_camera <- reactive({
    df <- SLED
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$year_sel_camera) && nzchar(input$year_sel_camera)) {
      df <- df[df$year == as.integer(input$year_sel_camera), , drop = FALSE]
    }
    ch <- sort(unique(suppressWarnings(as.integer(df$chamber_election_sub_leg))))
    ch <- ch[!is.na(ch) & ch %in% c(1L, 2L)]
    ch
  })
  
  .label_chambers <- function(v) {
    labs <- ifelse(v == 1L, "Lower chamber", ifelse(v == 2L, "Upper chamber", as.character(v)))
    stats::setNames(v, labs)
  }
  
  # Keep chamber selector in sync
  observeEvent(
    list(current_tab(), input$country_sel_camera, input$state_sel_camera, input$year_sel_camera),
    {
      req(current_tab() == "camera")
      if (is.null(input$chamber_sel_camera)) return()
      
      #shiny::invalidateLater(1, session)  # ← lets UI finish rebuilding first
      
      ch <- available_chambers_camera()
      
      if (!length(ch)) {
        shinyjs::disable("chamber_sel_camera")
        updateSelectInput(session, "chamber_sel_camera",
                          choices = setNames(numeric(0), character(0)),
                          selected = character(0))
        return(invisible(NULL))
      }
      
      shinyjs::enable("chamber_sel_camera")
      choices_named <- .label_chambers(ch)
      
      old_sel <- suppressWarnings(as.integer(isolate(input$chamber_sel_camera)))
      new_sel <- if (length(old_sel) && !is.na(old_sel) && old_sel %in% ch) old_sel else ch[1]
      
      updateSelectInput(session, "chamber_sel_camera",
                        choices = choices_named,
                        selected = new_sel)
    },
    ignoreInit = FALSE
  )
  
  

  
  # ==== 2) MODALS / MESSAGES ================================================
  observe({
    showModal(
      tags$div(
        id = "aboutSPPModal",
        modalDialog(
          title = HTML("
      <div style='display:flex; align-items:center; justify-content:center;'>
        <img src='spp_logo_v5.svg'/>
      </div>
    "),
          HTML("
          <div style='color:#111; font-size: 0.95em;'>
          <p style='text-align:justify;'>
          The Subnational Politics Project (SPP) is a collaborative initiative dedicated to compiling, generating, and disseminating systematic, transparent, and publicly accessible data on subnational political institutions, processes, and electoral outcomes across Latin America.
          </p>
          <p style='text-align:justify;'>
          The SPP’s central goal is to build a comprehensive and standardized data infrastructure that enables both detailed within-country analysis and robust cross-national comparisons of subnational political dynamics.
        </p>
        <p style='text-align:justify;'>
          By providing consistent, high-quality, and spatially disaggregated longitudinal data, the SPP seeks to advance scholarly and policy-oriented research on the political foundations and consequences of territorial inequality in Latin America.
        </p>
        <p style='text-align:justify;'>
          This data infrastructure will support empirical work on a wide range of topics, including federalism, decentralization, subnational democracy and authoritarianism, party competition, electoral accountability, territorial governance, among others.
        </p>

        <hr style='border:0; height:1px; margin:18px 0;
            background:linear-gradient(90deg, rgba(255,169,42,0), rgba(255,169,42,0.8), rgba(255,169,42,0));' />

        <h4 style='color:#FFA92A; margin-top:0;'>Reference</h4>
        <p style='font-size:0.9em; color:#4D4D4D;'>
          Giraudy, Agustina; Gonzalez, Guadalupe Andrea; Urdinez, Francisco, 2025, <em>\"Codebook: Subnational Politics Project (SPP) (v. 1)\"</em>, 
          <a href='https://doi.org/10.17605/OSF.IO/H96FD' target='_blank' style='color:#E5007D; text-decoration:none;'>https://doi.org/10.17605/OSF.IO/H96FD</a>.
        </p>
      </div>
    "),
      easyClose = TRUE,
      size = "xl",
      footer = modalButton("Close")
    )))
  })
  
  observe({
    tab     <- current_tab()
    country <- input$country_sel
    selvar  <- selected_vars_vector()
    
    is_legislative <- FALSE
    
    if (!is.null(selvar)) {
      
      # remove trailing _1 or _2 from the variable
      selvar_clean <- sub("_[12]$", "", selvar)
      
      dataset_val <- dict %>%
        dplyr::filter(variable == selvar_clean) %>%
        dplyr::pull(dataset) %>%
        unique()
      
      is_legislative <- identical(dataset_val, "Legislative Elections")
      
    }
    
    if (
      tab == "camera" ||
      (tab == "map_tab" && identical(country, "MEXICO") && is_legislative)
    ) {
      showModal(
        tags$div(
          id = "devNoticeModal",
          modalDialog(
            title = HTML(""),
            HTML("
            <div style='color:#fff; font-size: 1em; text-align:center;'>
              <p>
                <strong>UNDER CONSTRUCTION</strong>
              </p>
              <p>
                COMING SOON!
              </p>
            </div>
          "),
            easyClose = FALSE,
            size = "s",
            footer = modalButton("Back to the tool")
          )
        )
      )
    }
  })
  
  # ==== HOW-TO MODAL ======================================================
  
  
  
  
observeEvent(input$btn_howto, {
  
  # Pick the right explanation based on the current tab
  tab_name <- current_tab()
  
  howto_html <- switch(
    tab_name,
    
    "map_tab" = "
      <h4><i class='fa fa-map'></i> Mapping tool</h4>
      <p>
        Explore subnational data visually on an interactive map.
        Use the variable tree on the left to select an indicator, 
        choose a country, and move the year slider to see how it changes over time.
      </p>
      <p>
        Hover over subnational unit for details or play the animation to view trends.
      </p>
    ",
    
    "graph_tab" = "
      <h4><i class='fa fa-chart-line'></i> Graphing tool</h4>
      <p>
        Create time-series plots comparing subnational indicators.
        Select one or more states from different countries and choose a variable.
        The graph updates dynamically to show trends across time.
      </p>
      <p>
        Use the legend box to toggle series visibility.
      </p>
    ",
    
    "camera" = "
      <h4><i class='fa fa-landmark'></i> Camera Viz tool</h4>
      <p>
        Visualize the composition of subnational legislatures.
        Select a country, state, and chamber (lower or upper chamber) to view
        party seat distributions for each election year.
      </p>

    ",
    
    "codebook" = "
      <h4><i class='fa fa-book-open'></i> Codebook</h4>
      <p>
        The codebook provides full definitions and sources for all variables
        included in the Subnational Politics Project datasets.
      </p>
      <p>
        Use it to understand variable meanings, coding schemes, and references.
      </p>
    ",
    
    "data_tab" = "
      <h4><i class='fa fa-table'></i> Databases</h4>
      <p>
        Access and download the SPP databases through the Harvard Dataverse repository.
      </p>
    ",
    
    "about" = "
      <h4><i class='fa fa-circle-info'></i> About</h4>
      <p>
        Learn about the Subnational Politics Project (SPP): 
        its mission, team, and data infrastructure for the study of subnational politics in Latin America.
      </p>
    ",
    
    # default fallback
    "
      <h4><i class='fa fa-circle-question'></i> Welcome to SPP</h4>
      <p>
        Use the sidebar or the top navigation tabs to explore the different sections of the app.
      </p>
    "
  )
  
  
    showModal(
      tags$div(
        id = "howToModal",
        modalDialog(
        title = HTML("
              <div style='display:flex; align-items:center; justify-content:center;'>
                <img src='spp_logo_v5.svg' height='60'/>
              </div>
            "),
      easyClose = TRUE,
      size = "m",
      footer = modalButton("Close"),
      HTML(howto_html)
    )
  )
  )
})

  
  # “No data” message (map)
  output$no_data_message <- renderText("⚠ No data available for this country, variable and year.")
  

  
  # ==== 4) DYNAMIC UI (SELECTORS) ===========================================
  # -- 4.1) States selector (graph - multi-country) --------------------------
  output$state_selector <- renderUI({
    req(current_tab() == "graph_tab")
    
    states_choices <- data %>%
      dplyr::distinct(country_name, state_name) %>%
      dplyr::arrange(country_name, state_name) %>%
      dplyr::group_by(country_name) %>%
      dplyr::group_split()
    
    # For each country group: labels = Title Case, values = original state_name
    choices_list <- lapply(states_choices, function(group) {
      vals  <- group$state_name
      labs  <- stringr::str_to_title(vals)
      stats::setNames(as.list(vals), labs)  # names = labels shown; list items = values returned
    })
    
    countries <- sapply(states_choices, function(g) unique(g$country_name))
    names(choices_list) <- countries
    
    shinyWidgets::pickerInput(
      inputId = "state_sel",
      label   = "Select states from any country:",
      choices = choices_list,
      selected = c("CAPITAL FEDERAL", "DISTRITO FEDERAL", "CDMX"),  # original values still work
      multiple = TRUE,
      options  = list(`actions-box` = TRUE,
                      `live-search` = TRUE,
                      `selectedTextFormat` = "values")
    )
  })
  

  # -- 4.4) Country/Year selectors (map_tab) ---------------------------------
  output$country_selector <- renderUI({
    selectInput("country_sel", "Country",
                #choices = c("Select a country", unique(data$country_name)),
                choices = unique(data$country_name),
                selected = "MEXICO")
  })
  

  
  output$year_selector <- renderUI({
    req(current_tab() == "map_tab", input$country_sel, selected_vars_vector())
    
    # Static dataset
    df <- data
    
    # Core column names (adjust if needed)
    country_col <- "country_name"
    year_col    <- "year"
    
    # Selected variable (single column name as string)
    var_name <- selected_vars_vector()
    if (length(var_name) > 1) var_name <- var_name[[1]]
    req(is.character(var_name), var_name %in% names(df))
    
    # --- Filter early to minimize data in memory ---
    df_filtered <- df |>
      dplyr::filter(.data[[country_col]] == input$country_sel) |>
      dplyr::select(dplyr::all_of(c(country_col, year_col, var_name)))
    
    # Determine the first non-NA year for the selected variable
    y_min <- if (nrow(df_filtered) == 0) {
      1983L
    } else {
      valid_years <- df_filtered |>
        dplyr::filter(!is.na(.data[[var_name]])) |>
        dplyr::pull(.data[[year_col]])
      
      if (length(valid_years) > 0) {
        min(valid_years, na.rm = TRUE)
      } else {
        min(df_filtered[[year_col]], na.rm = TRUE)
      }
    }
    
    # Determine the latest available year (country-specific)
    y_max <- if (nrow(df_filtered) > 0) {
      max(df_filtered[[year_col]], na.rm = TRUE)
    } else {
      max(df[[year_col]], na.rm = TRUE)
    }
    
    # Safety fallback
    if (!is.finite(y_min) || !is.finite(y_max) || y_min > y_max) {
      y_min <- 1983L
      y_max <- 2024L
    }
    
    shinyWidgets::sliderTextInput(
      inputId  = "year_sel",
      label    = "Year",
      choices  = as.character(seq(y_min, y_max, by = 1)),
      grid     = TRUE,
      width    = "90%",
      animate  = TRUE,
      selected = 2005
    )
  })
  
  
  
  
  # ==== 5) SHOW / HIDE CONTROLS BY TAB ======================================
  # “No data” visibility (map)
  observe({
    req(current_tab() == "map_tab", data_map())
    if (nrow(data_map()) == 0 || all(is.na(data_map()[[selected_vars_vector()]]))) {
      shinyjs::show("no_data_message")
    } else {
      shinyjs::hide("no_data_message")
    }
  })
  
  # Batch toggle helpers
  .combine_selector <- function(ids) if (length(ids)) paste0("#", ids, collapse = ", ") else NULL
  .batch_show <- function(ids) { sel <- .combine_selector(ids); if (!is.null(sel)) shinyjs::show(selector = sel) }
  .batch_hide <- function(ids) { sel <- .combine_selector(ids); if (!is.null(sel)) shinyjs::hide(selector = sel) }
  
  # Everything we may toggle anywhere in the app
  ALL_TOGGLES <- c(
    # map/graph controls
    "country_selector","var_sel","var_description_map","var_description_graph","jstree_container",
    "state_selector",#"jstree_vars_container",
    "jstree_vars_container_graph","fancytree_vars_demo_container", "fancytree_vars_container_graph",
    "fancytree_states_container",
    # data-tab selectors
    #"country_sel2","state_sel2","db_selector","years",
    # legacy camera selector
    "camera_selector",
    # camera-tab selectors (SLED driven)
    "country_selector_camera","state_selector_camera",
    #"chamber_selector_camera", "year_selector_camera"
    "chamber_sel_camera","year_sel_camera"
  )
  
  # Given a tab, return vector of IDs to show
  .ids_to_show_for_tab <- function(tab) {
    switch(tab,
           "map_tab"   = c("country_selector","var_sel","var_description_map","fancytree_vars_demo_container"),
           "graph_tab" = c("var_description_graph","state_selector","fancytree_states_container","fancytree_vars_container_graph"),
           #"data_tab"  = c("country_sel2","state_sel2","db_selector","years"),
           "camera"    = c("country_selector_camera","state_selector_camera",
                           #"chamber_selector_camera","year_selector_camera"
                           "chamber_sel_camera","year_sel_camera"
                           ),
           character(0)
    )
  }
  
  # Main visibility controller
  observeEvent(current_tab(), {
    to_show <- .ids_to_show_for_tab(current_tab())
    to_hide <- setdiff(ALL_TOGGLES, to_show)
    .batch_hide(to_hide)
    .batch_show(to_show)
  }, ignoreInit = FALSE)
  

  
  # ==== 6.1) FILTERED DATA FOR CAMERA =======================================
  sled_cam_filtered <- reactive({
    df <- SLED
    if (!is.null(input$country_sel_camera) && nzchar(input$country_sel_camera)) {
      df <- df[df$country_name == input$country_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$state_sel_camera) && nzchar(input$state_sel_camera)) {
      df <- df[df$state_name == input$state_sel_camera, , drop = FALSE]
    }
    if (!is.null(input$chamber_sel_camera)) {
      df <- df[df$chamber_election_sub_leg == as.integer(input$chamber_sel_camera), , drop = FALSE]
    }
    if (!is.null(input$year_sel_camera) && nzchar(input$year_sel_camera)) {
      df <- df[df$year == as.integer(input$year_sel_camera), , drop = FALSE]
    }
    df
  })
  
  
  
  
  
  
  
  # -- 6.2) Camera info text (camera tab) ------------------------------------
  output$text_camera <- renderUI({
    req(current_tab() == "camera", sled_cam_filtered(), input$state_sel_camera)
    df <- sled_cam_filtered()
    
    # helpers --------------------------------------------------------
    first_or_summary <- function(x) {
      vals <- unique(na.omit(x))
      if (length(vals) == 0) return("—")
      if (length(vals) == 1) return(as.character(vals))
      paste0("varies (", paste(sort(vals), collapse = ", "), ")")
    }
    
    map_renewal <- function(x) {
      labs <- c(
        `1` = "Staggered every 2 years",
        `2` = "Full renewal"
      )
      u <- unique(na.omit(as.integer(x)))
      if (!length(u)) return("—")
      out <- ifelse(as.character(u) %in% names(labs), labs[as.character(u)], as.character(u))
      if (length(out) == 1) out else paste0("varies (", paste(out, collapse = ", "), ")")
    }
    
    map_system <- function(x) {
      labs <- c(
        `1` = "Proportional Representation",
        `2` = "Simple Majority",
        `3` = "Mixed (PR + Simple Majority)",
        `4` = "Mixed (PR with predefined districts)"
      )
      u <- unique(na.omit(as.integer(x)))
      if (!length(u)) return("—")
      out <- ifelse(as.character(u) %in% names(labs), labs[as.character(u)], as.character(u))
      if (length(out) == 1) out else paste0("varies (", paste(out, collapse = ", "), ")")
    }
    
    # compute --------------------------------------------------------
    total_chamber  <- first_or_summary(df$total_chamber_seats_sub_leg)
    seats_contest  <- first_or_summary(df$total_seats_in_contest_sub_leg)
    renewal_type   <- map_renewal(df$renewal_type_sub_leg)
    elec_system    <- map_system(df$electoral_system_sub_leg)
    n_parties_cont <- first_or_summary(df$num_parties_election_contest_sub_leg)

    enp_vals <- sort(unique(na.omit(as.numeric(df$enp_sub_leg))))
    enp_txt <- if (!length(enp_vals)) {
      "—"
    } else if (length(enp_vals) == 1) {
      sprintf("%.2f", enp_vals)
    } else {
      sprintf("varies (%.2f–%.2f)", min(enp_vals), max(enp_vals))
    }
    
    # assemble -------------------------------------------------------
    HTML(paste0(
      "<b>Chamber seats (total):</b> ", total_chamber, "<br/>",
      "<b>Seats in contest:</b> ", seats_contest, "<br/>",
      "<b>Renewal type:</b> ", renewal_type, "<br/>",
      "<b>Electoral system:</b> ", elec_system, "<br/>",
      "<b>Parties contesting:</b> ", n_parties_cont, "<br/>",
      "<b>ENPL:</b> ", enp_txt
    ))
  })
  
  
  
  # ==== 7) VARIABLE DESCRIPTIONS (map & graph) ==============================
  output$var_description_map <- renderUI({
    req(selected_vars_vector())
    
    # Fancytree sends a single string key
    key <- input$selected_nodes_vars2
    if (is.null(key) || key == "") return(NULL)
    
    parts <- strsplit(key, "-", fixed = TRUE)[[1]]
    
    # Detect chamber (only applies to Legislative Elections)
    chamber <- NULL
    if (identical(parts[1], "Legislative Elections") &&
        length(parts) >= 3 &&
        parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      chamber <- parts[2]
    }
    
    # Remove _1/_2 suffix from variable ID
    clean_var <- sub("_[12]$", "", selected_vars_vector())
    
    # Retrieve metadata
    var_info <- dict %>%
      dplyr::filter(variable == clean_var) %>%
      dplyr::slice(1)
    
    # Build text
    text_d <- paste0(
      "<div>You are seeing <strong>",
      var_info$description_for_ui[1], "</strong>",
      if (!is.null(chamber)) paste0(" (", chamber, ")") else "",
      "; from the Subnational <strong>", parts[1],
      "</strong> Database.",ifelse(is.na(var_info$add_indices[1]),"",var_info$add_indices[1]),"</div>"
    )
    
    HTML(text_d)
  })
  
  
  output$var_description_graph <- renderUI({
    req(selected_vars_vector_graph())
    
    # Fancytree sends a single string key
    key <- input$selected_nodes_vars_graph2
    if (is.null(key) || key == "") return(NULL)
    
    parts <- strsplit(key, "-", fixed = TRUE)[[1]]
    
    # Detect chamber type (only for Legislative Elections)
    chamber <- NULL
    if (identical(parts[1], "Legislative Elections") &&
        length(parts) >= 3 &&
        parts[2] %in% c("Lower Chamber", "Upper Chamber")) {
      chamber <- parts[2]
    }
    
    # Clean variable (remove _1 or _2 suffix)
    clean_var <- sub("_[12]$", "", selected_vars_vector_graph())
    
    # Metadata row
    var_info <- dict %>%
      dplyr::filter(variable == clean_var) %>%
      dplyr::slice(1)
    
    # Build UI string
    text_d <- paste0(
      "<div>You are seeing <strong>",
      var_info$description_for_ui[1], "</strong>",
      if (!is.null(chamber)) paste0(" (", chamber, ")") else "",
      "; from the Subnational <strong>",
      parts[1],
      "</strong> Database.</div>"
    )
    
    HTML(text_d)
  })
  

  # Keep default options of var_sel and var_sel2 in sync
  observe({
    updateSelectInput(
      session, "var_sel",
      choices  = dict$pretty_name[dict$viewable_map == 1],
      selected = "Valid Votes"
    )
    updateSelectInput(
      session, "var_sel2",
      choices  = dict$pretty_name[dict$viewable_graph == 1],
      selected = "Voter Turnout Percentage"
    )
  })
  

  
  
  
  # ==== 9) MODULES (map / lines / table / camera) ===========================
  

  
  mapModuleServer(
    id = "map1",
    data_map = data_map,
    input_var_sel = selected_vars_vector,
    dict = dict,
    country_bboxes = country_bboxes,
    input_country_sel = reactive(input$country_sel),
    active_tab = current_tab
  )
  
  linePlotModuleServer(
    id = "lp",
    data = reactive(data),
    dict = dict,
    input_variable = selected_vars_vector_graph,
    input_states = selected_states_vector,
    Ymin = reactive(if (isTRUE(input$force_y0)) 0 else NULL),
    active_tab = current_tab
  )
  

  # Hemicycle: still using original module (with inputs), now pointing to camera-only selectors.
  camaraServer(
    id = "cam",
    data = SLED, # if you later refactor the module, change to data_r = sled_cam_filtered
    state_r   = reactive(input$state_sel_camera),
    chamber_r = reactive(input$chamber_sel_camera),
    year_r    = reactive(input$year_sel_camera),
    party_col = "party_name_sub_leg",
    seats_col = "total_seats_party_sub_leg",
    state_col = "state_name",
    chamber_filter_col = "chamber_election_sub_leg",
    year_col = "year"
    # ,
    # title_text = "Chamber composition"
  )
  

  sppAboutModuleServer("about")
  
  spp_mvp_server("spp1", current_tab = current_tab)
  

  # ==== 11) PDF VIEWER ======================================================
  output$pdf_visor <- renderUI({
    tags$iframe(style = "height:800px; width:100%;", src = "SPP_codebook.pdf")
  })
  
  # ==== 12) TABS ======================================================
  # 
  # observeEvent(input$tab_about, { updateTabItems(session, "tabs", "about") })
  # observeEvent(input$tab_map, { updateTabItems(session, "tabs", "map_tab") })
  # observeEvent(input$tab_graph, { updateTabItems(session, "tabs", "graph_tab") })
  # observeEvent(input$tab_camera, { updateTabItems(session, "tabs", "camera") })
  # observeEvent(input$tab_codebook, { updateTabItems(session, "tabs", "codebook") })
  # observeEvent(input$tab_data, { updateTabItems(session, "tabs", "data_tab") })
  # 
  # 
  # 
  # 
  
  # ==== 13) SWIPE ======================================================
  observeEvent(input$sidebar_swipe, {
    if (input$sidebar_swipe == "left") {
      shinyjs::runjs("
      console.log('Force closing sidebar (server).');
      $('body')
        .removeClass('sidebar-open')
        .addClass('sidebar-collapse');
    ");
    }
  })
  

  
}




