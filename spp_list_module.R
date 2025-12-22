

plan(multisession)  # run network calls off the main thread

# --- Small helpers ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x) || all(is.na(x))) y else x
now_sec <- function() as.numeric(Sys.time())

# --- In-memory cache + circuit breaker ---------------------------------------
.spp_cache <- new.env(parent = emptyenv())
get_cache_key <- function(subtree, per_page) paste0(subtree, "|", per_page)

cache_set <- function(key, value, ttl = 600) {  # 10 min default TTL
  .spp_cache[[key]] <- list(data = value, expire = now_sec() + ttl)
}
cache_get <- function(key) {
  entry <- .spp_cache[[key]]
  if (is.null(entry)) return(NULL)
  if (now_sec() > entry$expire) return(NULL)
  entry$data
}

.cb <- new.env(parent = emptyenv())
.cb$fail_count <- 0L
.cb$opened_until <- 0

cb_record_success <- function() { .cb$fail_count <- 0L; .cb$opened_until <- 0 }
cb_record_failure <- function(max_fail = 3L, cooloff_sec = 300) {
  .cb$fail_count <- .cb$fail_count + 1L
  if (.cb$fail_count >= max_fail) .cb$opened_until <- now_sec() + cooloff_sec
}
cb_is_open <- function() now_sec() < .cb$opened_until
cb_time_left <- function() max(0, round(.cb$opened_until - now_sec()))

# --- Citations dictionary (abbr -> citation + DOI) ----------------------------
spp_citations <- data.frame(
  abbr  = c("SED","SEED","SLED","SDI","CFTDFLD","NED","Codebook"),
  title = c(
    "Subnational Executive Database",
    "Subnational Executive Elections Database",
    "Subnational Legislative Elections Database",
    "Subnational Democracy Indices",
    "Capital Federal & Tierra del Fuego Legislatures Database",
    "National Executive Database",
    "Codebook:Subnational Politics Prpoject (SPP)"
  ),
  citation_text = c(
    "Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hern\u00E1ndez, and Francisco Urdinez. 2025. \u201CSubnational Executive Database (SED) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hern\u00E1ndez, and Francisco Urdinez. 2025. \u201CSubnational Executive Elections Database (SEED) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. \u201CSubnational Legislative Elections Database (SLED) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina. 2025. \u201CSubnational Democracy Indices (SDI) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. \u201CCapital Federal & Tierra Del Fuego Legislatures Database (CFTDFLD) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hern\u00E1ndez, and Francisco Urdinez. 2025. \u201CNational Executive Database (NED) (v. 1).\u201D Subnational Politics Project. ",
    "Giraudy, Agustina; Gonzalez, Guadalupe Andrea; Urdinez, Francisco, 2025, “Codebook: Subnational Politics Project (SPP) (v. 1)”. "
  ),
  doi  = c(
    "10.7910/DVN/1D3P3J",
    "10.7910/DVN/UPOWMW",
    "10.7910/DVN/084FXF",
    "10.7910/DVN/7TNLBW",
    "10.7910/DVN/AJJLHX",
    "10.7910/DVN/HNKQUH",
    "10.17605/OSF.IO/H96FD"
  ),
  stringsAsFactors = FALSE
)

spp_citations$citation_html <- sprintf(
  '%s<a href="https://doi.org/%s" target="_blank" class="spp-link">https://doi.org/%s</a>',
  htmlEscape(spp_citations$citation_text), spp_citations$doi, spp_citations$doi
)

# --- Robust Dataverse fetcher (timeout + retries) -----------------------------
dv_search_mvp_safe <- function(subtree = "spp", per_page = 50, timeout_sec = 20, retries = 2) {
  url <- sprintf(
    "https://dataverse.harvard.edu/api/search?q=*&type=dataset&subtree=%s&per_page=%d&start=0&sort=date&order=desc",
    utils::URLencode(subtree, reserved = TRUE), per_page
  )
  req <- request(url) |>
    req_user_agent("SPP-Shiny/1.0") |>
    req_timeout(seconds = timeout_sec) |>
    req_retry(max_tries = retries + 1, backoff = ~ 0.4 * (2^.x) + runif(1, 0, 0.3))
  
  resp <- req_perform(req)
  if (resp_status(resp) >= 300) return(data.frame())
  json <- resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(json) || is.null(json$data) || is.null(json$data$items)) return(data.frame())
  as.data.frame(json$data$items, stringsAsFactors = FALSE)
}

# Async wrapper (returns a promise)
dv_search_mvp_async <- function(subtree = "spp", per_page = 50) {
  future({
    dv_search_mvp_safe(subtree = subtree, per_page = per_page)
  })
}

# --- UI -----------------------------------------------------------------------
spp_mvp_ui <- function(id,
                       title = "SPP databases",
                       subtree = "spp",
                       per_page = 50,
                       max_width = 1050,
                       open_sections = c("datasets", "databases")) {
  ns <- NS(id)
  root_id <- ns("root")
  
  open_attr <- function(key, open_sections) if (key %in% open_sections) "open" else NULL
  
  style_css <- paste0(
    "/* Palette via CSS variables (fallbacks included) */\n",
    "#", root_id, " { background:#fff; color:var(--gray, #4D4D4D); font-family: Helvetica, Arial, sans-serif; padding:24px 10px; }\n",
    "#", root_id, " .spp-container { max-width:", max_width, "px; margin:0 auto; }\n",
    "#", root_id, " .kicker { color:var(--orange, #FFA92A); text-transform:uppercase; letter-spacing:0.12em; font-size:24px; margin:0 0 12px 0; }\n",
    "#", root_id, " p.description { text-align:center; color:#444; margin-bottom:20px; }\n",
    "#", root_id, " details.card { background:#fff; border:1px solid #e6e6e6; border-radius:12px; margin-bottom:18px; overflow:visible; transition: transform .12s ease, box-shadow .12s ease, border-color .12s ease; }\n",
    "#", root_id, " details.card:hover { transform: translateY(-2px); box-shadow: 0 6px 14px rgba(0,0,0,0.18); border-color: rgba(255,169,42,0.35); }\n",
    "#", root_id, " details.card[open] { box-shadow:0 6px 14px rgba(0,0,0,0.12); border-color: rgba(255,169,42,0.45); }\n",
    "#", root_id, " summary { list-style:none; cursor:pointer; padding:14px 16px; display:flex; align-items:center; gap:10px; }\n",
    "#", root_id, " summary::-webkit-details-marker { display:none; }\n",
    "#", root_id, " .chev { margin-left:auto; transition: transform .15s ease; }\n",
    "#", root_id, " details[open] .chev { transform: rotate(180deg); }\n",
    "#", root_id, " .card-title { font-weight:800; font-size:18px; color:var(--purple, #722464); }\n",
    "#", root_id, " .card-body { padding: 0 16px 16px 16px; color:var(--gray, #4D4D4D); }\n",
    "#", root_id, " .spp-text { color:var(--gray, #4D4D4D); line-height:1.6; text-align:justify; }\n",
    "#", root_id, " .spp-hr { border:0; height:1px; background:linear-gradient(90deg, rgba(255,169,42,0), rgba(255,169,42,0.8), rgba(255,169,42,0)); margin:14px 0; }\n",
    "#", root_id, " .figure { margin:10px 0 12px 0; text-align:center; }\n",
    "#", root_id, " .figure img { max-width:100%; height:auto; border-radius:12px; border:1px solid #eee; }\n",
    "#", root_id, " ol.vars { margin:10px 0 0 18px; }\n",
    "#", root_id, " ol.vars li { margin:6px 0; }\n",
    "#", root_id, " table.dataTable td:nth-child(1) { min-width: clamp(280px, 45vw, 520px); }\n",
    "#", root_id, " .loading-wrap { position:relative; min-height:120px; display:flex; align-items:center; justify-content:center; flex-direction:column; gap:10px; }\n",
    "#", root_id, " .spinner { width:42px; height:42px; border:4px solid #eee; border-top-color:var(--orange, #FFA92A); border-radius:50%; animation: spp-spin 0.9s linear infinite; }\n",
    "@keyframes spp-spin { to { transform: rotate(360deg); } }\n",
    "#", root_id, " .loading-text { color:#666; font-size:0.95rem; }\n",
    "#", root_id, " .spp-alert { background:#FFF4E5; border:1px solid #F5C07B; color:#7A4B00; padding:8px 10px; border-radius:8px; font-size:13px; }\n"
  )
  
  tagList(
    singleton(tags$style(HTML(style_css))),
    tags$div(
      id = root_id,
      tags$div(
        class = "spp-container",
        tags$p(class = "kicker", title),
        
        # --- Card 1: Datasets table ------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("datasets", open_sections),
          tags$summary(
            tags$span(class = "card-title", "Download SPP Databases"),
            shiny::icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$p(
              class = "spp-text",
              "Click a dataset to open and download the corresponding data from Harvard Dataverse."
            ),
            div(
              style="display:flex; gap:8px; align-items:center; margin:8px 0 12px 0;",
              actionButton(ns("refresh"), "Refresh", class = "btn btn-sm"),
              uiOutput(ns("status"), inline = TRUE)
            ),
            uiOutput(ns("datasets_panel"))
          )
        ),
        
        # --- Card 2: About Databases -----------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("databases", open_sections),
          tags$summary(
            tags$span(class = "card-title", "About SPP Databases"),
            shiny::icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            # Section A: Databases’ Structure
            tags$p(class = "spp-text", tags$strong("Databases’ Structure")),
            tags$p(
              class = "spp-text",
              "The Subnational Politics Project (SPP) is made up of different databases. Each database employs a country–state–year structure, with observations at the subnational unit level for each electoral year. Each observation represents a subnational unit (province/state) in a given year."
            ),
            tags$div(
              class = "figure",
              tags$img(src = "databases_spp.svg", alt = "Figure. SPP Databases")
            ),
            tags$hr(class = "spp-hr"),
            
            # Section B: Variable Information
            tags$p(class = "spp-text", tags$strong("Variable Information")),
            tags$p(
              class = "spp-text",
              "The databases in the Subnational Politics Project divide variables into the following types:"
            ),
            tags$ol(
              class = "vars",
              tags$li(tags$b("Identifier Variables:"), " Data identifying country names and codes, state names and codes, region names, and time periods."),
              tags$li(tags$b("Executive Branch Variables:"), " Data on national and subnational executive branches, such as length of term, incumbent party, cumulative years of president/governor in office, etc."),
              tags$li(tags$b("Electoral Variables:"), " Data on subnational executive and subnational legislative elections, including legislatures’ composition."),
              tags$li(tags$b("Indices:"), " Data generated by adding and combining variables, or creating cumulative scales.")
            ),
            tags$div(
              class = "figure",
              tags$img(src = "variables_database.svg", alt = "Figure. Variable Types in SPP")
            )
          )
        )
      )
    )
  )
}

# --- Server -------------------------------------------------------------------
spp_mvp_server <- function(id,
                           subtree = "spp",
                           per_page = 50,
                           current_tab = NULL,
                           tab_id = "data_tab",
                           cache_ttl = 600) {
  moduleServer(id, function(input, output, session) {
    
    items_rv     <- reactiveVal(NULL)   # merged items (with abbr & citation_html when available)
    error_rv     <- reactiveVal(NULL)   # last error message, if any
    loading_rv   <- reactiveVal(FALSE)
    fetched_once <- reactiveVal(FALSE)  # avoid refetching on every tab visit
    
    # Helper: detect first matching abbreviation token in a dataset name
    detect_abbr <- function(dataset_name) {
      hits <- spp_citations$abbr[
        vapply(
          spp_citations$abbr,
          function(a) grepl(paste0("\\b", a, "\\b"), dataset_name %||% "", ignore.case = TRUE),
          logical(1)
        )
      ]
      if (length(hits)) hits[[1]] else NA_character_
    }
    
    # Do one safe fetch (uses cache, circuit breaker, and async)
    do_fetch <- function(force = FALSE) {
      if (loading_rv()) return(invisible(NULL))
      key <- get_cache_key(subtree, per_page)
      
      # Serve cache immediately unless force
      if (!force) {
        cached <- cache_get(key)
        if (!is.null(cached)) {
          items_rv(cached); error_rv(NULL); fetched_once(TRUE)
          return(invisible(NULL))
        }
      }
      
      if (cb_is_open()) {
        error_rv(sprintf("Dataverse unreachable recently. Pausing new attempts for %s sec.", cb_time_left()))
        return(invisible(NULL))
      }
      
      loading_rv(TRUE); error_rv(NULL)
      
      dv_search_mvp_async(subtree = subtree, per_page = per_page) %...>%
        (function(df) {
          # Normalize/guard
          if (!nrow(df)) stop("Empty result from Dataverse.")
          for (col in c("url","name")) if (is.null(df[[col]])) df[[col]] <- NA_character_
          df[is.na(df$url),  "url"]  <- "#"
          df[is.na(df$name), "name"] <- "Untitled dataset"
          
          # Match abbreviations and merge citations
          df$abbr <- vapply(df$name, detect_abbr, character(1))
          merged <- merge(df, spp_citations[, c("abbr","citation_html")], by = "abbr", all.x = TRUE)
          
          cache_set(key, merged, ttl = cache_ttl)
          items_rv(merged); fetched_once(TRUE)
          cb_record_success()
        }) %...!% (function(e) {
          cb_record_failure()
          error_rv(paste("Failed to fetch Dataverse data.", conditionMessage(e)))
          items_rv(NULL)  # NULL signals we should show fallback
        }) %...>% (function(...) {
          loading_rv(FALSE)
        })
      
      invisible(NULL)
    }
    
    # Trigger the first fetch only when the tab/card is visible
    observeEvent({
      if (is.null(current_tab)) TRUE else current_tab() == tab_id
    }, {
      if (!fetched_once()) do_fetch(force = FALSE)
    }, ignoreInit = FALSE)
    
    # Manual refresh
    observeEvent(input$refresh, {
      do_fetch(force = TRUE)
    })
    
    # Status banner: only show when falling back or error present
    output$status <- renderUI({
      if (isTRUE(loading_rv()))
        return(tags$span("Loading…", style="color:#666;"))
      
      if (!is.null(error_rv())) {
        return(tags$div(
          class = "spp-alert",
          "Harvard Dataverse links are currently down or unreachable. The table below shows database names and suggested citations."
        ))
      }
      
      it <- items_rv()
      if (!is.null(it) && nrow(it) > 0 && all(it$url == "#" | is.na(it$url))) {
        return(tags$div(
          class = "spp-alert",
          "Dataset links are temporarily unavailable, but names and citations are shown below."
        ))
      }
      
      NULL  # No banner in the normal (success) case
    })
    
    output$datasets_panel <- renderUI({
      if (!is.null(current_tab) && current_tab() != tab_id) return(NULL)
      if (isTRUE(loading_rv())) {
        return(tags$div(
          class = "loading-wrap",
          `aria-live` = "polite",
          tags$div(class = "spinner"),
          tags$p(class = "loading-text", "Fetching datasets from Harvard Dataverse…")
        ))
      }
      DT::DTOutput(session$ns("tbl"))
    })
    
    output$tbl <- DT::renderDT({
      if (!is.null(current_tab)) req(current_tab() == tab_id)
      
      items <- items_rv()
      
      make_dt <- function(df, escape_cols) {
        DT::datatable(
          df,
          escape = escape_cols,
          rownames = FALSE,
          selection = "none",
          options = list(
            dom = "t",
            paging = FALSE,
            info = FALSE,
            searching = FALSE,
            ordering = FALSE,
            scrollX = TRUE
          )
        )
      }
      
      # Case A: SUCCESS — fetched items; show Dataverse links + citations
      if (!is.null(items) && nrow(items)) {
        df <- data.frame(
          `Database Name` = vapply(seq_len(nrow(items)), function(i) {
            as.character(tags$a(
              href = items$url[i], target = "_blank", rel = "noopener",
              HTML(htmlEscape(items$name[i]))
            ))
          }, character(1)),
          `Suggested Citation` = items$citation_html %||% "",
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        return(make_dt(df, escape_cols = c(FALSE, FALSE)))
      }
      
      # Case B: FAILURE / DOWN — fallback to dictionary (no Dataverse links)
      df_fallback <- data.frame(
        `Database Name` = vapply(spp_citations$title, htmlEscape, character(1)),
        `Suggested Citation` = spp_citations$citation_html,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      make_dt(df_fallback, escape_cols = c(TRUE, FALSE))
    })
  })
}
