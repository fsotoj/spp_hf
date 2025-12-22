#--------
# Party palette

palette_from_table <- function(parties, color_table,
                               party_col = "party_name_sub_leg",
                               color_col = "color",
                               default = "#888888") {
  
  pal <- color_table %>%
    dplyr::filter(.data[[party_col]] %in% parties) %>%
    dplyr::select(
      party = .data[[party_col]],
      color = .data[[color_col]]
    ) %>%
    tibble::deframe()
  
  # ensure every party has a color
  missing <- setdiff(parties, names(pal))
  if (length(missing) > 0) {
    pal[missing] <- default
  }
  
  pal
}






#-------------------------------
# Hemicycle layout (clean wedges)
#-------------------------------
hemicycle_layout <- function(N, layers = NULL, r_min = 0.4, r_max = 1) {
  if (N <= 0) return(tibble(theta = numeric(), r = numeric(), x = numeric(), y = numeric(), layer = integer(), pos = integer()))
  if (is.null(layers)) {
    layers <- dplyr::case_when(
      N <= 30 ~ 3L,
      N <= 60 ~ 4L,
      N <= 150 ~ 6L,
      N <= 300 ~ 8L,
      TRUE ~ 10L
    )
  }
  radii <- seq(r_min, r_max, length.out = layers)
  weights <- radii / sum(radii)
  n_per_layer <- pmax(1L, round(N * weights))
  dif <- N - sum(n_per_layer)
  if (dif != 0) {
    idx <- order(weights, decreasing = (dif > 0))
    for (i in seq_len(abs(dif))) {
      j <- idx[((i - 1) %% length(idx)) + 1]
      n_per_layer[j] <- n_per_layer[j] + sign(dif)
    }
  }
  
  coords <- purrr::map2_dfr(
    .x = seq_along(radii),
    .y = n_per_layer,
    ~{
      L <- .x; nL <- .y; r <- radii[L]
      theta <- if (nL == 1) pi/2 else seq(pi, 0, length.out = nL)  # left (π) -> right (0)
      tibble(theta = theta, r = r, x = r * cos(theta), y = r * sin(theta), layer = L)
    }
  ) %>% arrange(desc(theta), r)
  
  coords$pos <- seq_len(nrow(coords))
  coords
}

#-------------------------------------------
# Expand seats by party & assign contiguous blocks
#  - Respects df_agg current order (no reordering).
#  - If 'non-contested' label exists, it is placed at the END => right side.
#-------------------------------------------
expand_and_assign <- function(df_agg,
                              party_col = "party",
                              seats_col = "seats",
                              non_contested_label = "NON-CONTESTED SEATS") {
  if (nrow(df_agg) == 0) return(tibble(party = character(), x = numeric(), y = numeric()))
  
  df_agg <- df_agg %>%
    mutate(across(all_of(party_col), as.character)) %>%
    filter(.data[[seats_col]] > 0)
  
  # split non-contested vs real, keep given order
  df_nc   <- df_agg %>% filter(.data[[party_col]] == non_contested_label)
  df_real <- df_agg %>% filter(.data[[party_col]] != non_contested_label)
  
  df_ord <- bind_rows(df_real, df_nc)
  
  N   <- sum(df_ord[[seats_col]])
  if (N <= 0) return(tibble(party = character(), x = numeric(), y = numeric()))
  lay <- hemicycle_layout(N)
  
  # spans
  spans <- df_ord %>%
    mutate(
      start = cumsum(dplyr::lag(.data[[seats_col]], default = 0)) + 1L,
      end   = cumsum(.data[[seats_col]])
    )
  
  expanded <- purrr::map_dfr(seq_len(nrow(spans)), function(i) {
    tibble(
      party = spans[[party_col]][i],
      seat_index = seq.int(spans$start[i], spans$end[i])
    )
  })
  
  expanded %>%
    left_join(lay %>% select(pos, x, y), by = c("seat_index" = "pos")) %>%
    select(party, x, y)
}

#-------------------------------
# Distinct palette
#-------------------------------
# palette_distinct <- function(labels) {
#   labs <- unique(as.character(labels))
#   n <- length(labs)
#   if (n == 0) return(character())
#   hues <- seq(15, 375, length.out = n + 1)[-1]
#   cols <- grDevices::hcl(h = hues, c = 100, l = 60)
#   stats::setNames(cols, labs)
# }




#-------------------------------
# Module UI (chart only) - now Highcharts
#-------------------------------
camaraUI <- function(id, height = "70vh") {
  ns <- NS(id)
  highcharter::highchartOutput(ns("chart"), height = height)
}

#-------------------------------
# External Legend UI (scrollable, same as before)
#-------------------------------
camaraLegendUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf('
      #%1$s-legend { width: 100%%; }

      #%1$s-legend .legend-title {
        margin: 0 0 8px 0;
        color: #4D4D4D;
        font-weight: 600;
        font-size: 20px;
      }

      /* Scrollable legend */
      #%1$s-legend .legend-wrap {
        display: flex;
        flex-direction: column;
        gap: 12px;

        max-height: 240px;
        overflow-y: auto;
        padding-right: 6px;
      }
      #%1$s-legend .legend-wrap::-webkit-scrollbar {
        width: 6px;
      }
      #%1$s-legend .legend-wrap::-webkit-scrollbar-thumb {
        background: #CCCCCC;
        border-radius: 3px;
      }

      #%1$s-legend .legend-group {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      #%1$s-legend .legend-row {
        display: flex;
        flex-wrap: wrap;
        gap: 8px 10px;
      }

      #%1$s-legend .legend-item {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 6px 10px;
        border-radius: 9999px;
        background: #FFFFFF;
        box-shadow: 0 0 0 1px #E6E6E6 inset;
        color: #4D4D4D;
        font-size: 12px;
        transition: box-shadow .12s ease;
      }
      #%1$s-legend .legend-item:hover {
        box-shadow: 0 0 0 1px #FFA92A inset;
      }

      #%1$s-legend .legend-swatch {
        width: 14px;
        height: 14px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
      }

      #%1$s-legend .legend-text { white-space: nowrap; }
      #%1$s-legend svg { display: block; }
    ', ns("chart")))),
    
    div(
      id = paste0(ns("chart"), "-legend"),
      tags$div(class = "legend-title", "Chamber parties"),
      uiOutput(ns("legend"), container = div, inline = FALSE)
    )
  )
}


camaraServer <- function(id,
                         data,                      # static data.frame OR reactive
                         state_r,                   # reactive: state (string) or vector; "" ignored
                         chamber_r,                 # reactive: 1/2
                         year_r,                    # reactive: year
                         party_col   = "party_name_sub_leg",
                         seats_col   = "total_seats_party_sub_leg",
                         state_col   = "state_name",
                         chamber_filter_col = "chamber_election_sub_leg",
                         year_col    = "year",
                         previous_name  = "NON-CONTESTED SEATS",
                         previous_color = "rgba(154,160,166,0.40)") {
  
  moduleServer(id, function(input, output, session) {
    
    # --- Reactive data wrapper ---
    data_r <- if (inherits(data, "reactive")) data else reactive(data)
    
    #-------------------------------
    # Chart
    #-------------------------------
    output$chart <- highcharter::renderHighchart({
      req(state_r(), chamber_r(), year_r())
      df <- data_r()
      
      if (!inherits(df, c("data.frame","tbl","tbl_df"))) {
        return(
          highcharter::highchart() %>%
            highcharter::hc_chart(
              type = "scatter",
              zoomType = NULL,
              animation = FALSE
            ) %>%
            highcharter::hc_title(text = "No data (invalid input table)") %>%
            highcharter::hc_xAxis(visible = FALSE) %>%
            highcharter::hc_yAxis(visible = FALSE) %>%
            highcharter::hc_series(
              list(
                name = "No data",
                type = "scatter",
                data = list(list(0, 0))
              )
            ) %>%
            highcharter::hc_legend(enabled = FALSE) %>%
            highcharter::hc_exporting(enabled = FALSE) %>%
            highcharter::hc_credits(enabled = FALSE)
        )
      }
      
      # columns check
      cols_needed <- c(state_col, chamber_filter_col, year_col, party_col, seats_col)
      missing <- setdiff(cols_needed, names(df))
      if (length(missing) > 0) {
        return(
          highcharter::highchart() %>%
            highcharter::hc_chart(type = "scatter", zoomType = NULL, animation = FALSE) %>%
            highcharter::hc_title(
              text = paste("Missing columns:", paste(missing, collapse = ", "))
            ) %>%
            highcharter::hc_xAxis(visible = FALSE) %>%
            highcharter::hc_yAxis(visible = FALSE) %>%
            highcharter::hc_series(
              list(
                name = "No data",
                type = "scatter",
                data = list(list(0, 0))
              )
            ) %>%
            highcharter::hc_legend(enabled = FALSE) %>%
            highcharter::hc_exporting(enabled = FALSE) %>%
            highcharter::hc_credits(enabled = FALSE)
        )
      }
      
      # types
      df[[state_col]]          <- as.character(df[[state_col]])
      df[[party_col]]          <- as.character(df[[party_col]])
      df[[seats_col]]          <- suppressWarnings(as.integer(df[[seats_col]]))
      df[[year_col]]           <- suppressWarnings(as.integer(df[[year_col]]))
      df[[chamber_filter_col]] <- suppressWarnings(as.integer(df[[chamber_filter_col]]))
      df$total_chamber_seats_sub_leg <- suppressWarnings(
        as.integer(as.character(df$total_chamber_seats_sub_leg))
      )
      
      # inputs
      sel_states  <- tryCatch(state_r(), error = function(e) NULL)
      if (length(sel_states) == 1 && identical(sel_states, "")) sel_states <- NULL
      sel_states  <- if (!is.null(sel_states)) as.character(sel_states) else NULL
      sel_year    <- suppressWarnings(as.integer(tryCatch(year_r(), error = function(e) NA_integer_)))
      sel_chamber <- suppressWarnings(as.integer(tryCatch(chamber_r(), error = function(e) NA_integer_)))
      
      # filter
      dff <- df %>% dplyr::filter(.data[[seats_col]] != 0)
      if (!is.null(sel_states) && length(sel_states) > 0) {
        dff <- dff %>% dplyr::filter(.data[[state_col]] %in% sel_states)
      }
      if (!is.na(sel_chamber)) {
        dff <- dff %>% dplyr::filter(.data[[chamber_filter_col]] == sel_chamber)
      }
      if (!is.na(sel_year)) {
        dff <- dff %>% dplyr::filter(.data[[year_col]] == sel_year)
      }
      
      if (nrow(dff) == 0) {
        return(
          highcharter::highchart() %>%
            highcharter::hc_chart(type = "scatter", zoomType = NULL, animation = FALSE) %>%
            highcharter::hc_title(text = "No data for current selection") %>%
            highcharter::hc_xAxis(visible = FALSE) %>%
            highcharter::hc_yAxis(visible = FALSE) %>%
            highcharter::hc_series(
              list(
                name = "No data",
                type = "scatter",
                data = list(list(0, 0))
              )
            ) %>%
            highcharter::hc_legend(enabled = FALSE) %>%
            highcharter::hc_exporting(enabled = FALSE) %>%
            highcharter::hc_credits(enabled = FALSE)
        )
      }
      
      # aggregate base
      agg_base <- dff %>%
        dplyr::transmute(party = .data[[party_col]], seats = .data[[seats_col]]) %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(seats = sum(seats, na.rm = TRUE), .groups = "drop")
      
      # total chamber + NON-CONTESTED seats
      total_chamber_vec <- suppressWarnings(as.integer(na.omit(dff$total_chamber_seats_sub_leg)))
      total_chamber <- if (length(total_chamber_vec)) {
        tb <- sort(table(total_chamber_vec), decreasing = TRUE)
        cand <- as.integer(names(tb[ tb == max(tb) ]))
        max(cand)
      } else NA_integer_
      
      agg <- agg_base
      if (!is.na(total_chamber)) {
        falta <- total_chamber - sum(agg_base$seats, na.rm = TRUE)
        if (falta > 0) {
          agg <- dplyr::bind_rows(
            agg_base,
            tibble(party = previous_name, seats = falta)
          )
        }
      }
      
      # order: real by size, non-contested last
      agg <- agg %>%
        dplyr::mutate(.is_prev = as.integer(party == previous_name)) %>%
        dplyr::arrange(.is_prev, dplyr::desc(seats), party) %>%
        dplyr::select(-.is_prev)
      
      # expand seats (force non-contested to the right/end)
      pts <- expand_and_assign(
        agg,
        party_col = "party",
        seats_col = "seats",
        non_contested_label = previous_name
      )
      if (nrow(pts) == 0) {
        return(
          highcharter::highchart() %>%
            highcharter::hc_chart(type = "scatter", zoomType = NULL, animation = FALSE) %>%
            highcharter::hc_title(text = "No seats to plot") %>%
            highcharter::hc_xAxis(visible = FALSE) %>%
            highcharter::hc_yAxis(visible = FALSE) %>%
            highcharter::hc_series(
              list(
                name = "No seats",
                type = "scatter",
                data = list(list(0, 0))
              )
            ) %>%
            highcharter::hc_legend(enabled = FALSE) %>%
            highcharter::hc_exporting(enabled = FALSE) %>%
            highcharter::hc_credits(enabled = FALSE)
        )
      }
      
      # series order & palette (SYNC SOURCE for legend)
      pts <- pts %>% dplyr::mutate(party = factor(party, levels = agg$party))
      
      
      # pal <- palette_distinct(levels(pts$party))
      # if (previous_name %in% names(pal)) pal[previous_name] <- previous_color
      
      
      pal <- palette_from_table(
        parties = levels(pts$party),
        color_table = party_colors_leg
      )
      
      if (previous_name %in% names(pal)) {
        pal[previous_name] <- previous_color
      }
      
      
      
      
      
      
      
      
      # Add total_votes_party_sub_leg grouped per party
      vote_lookup <- dff %>%
        dplyr::group_by(.data[[party_col]]) %>%
        dplyr::summarise(total_votes = unique(.data$total_votes_party_sub_leg, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(total_votes = format(total_votes, big.mark = ",", scientific = FALSE))
        
      
      seat_lookup <- agg %>% 
        dplyr::select(party, seats)
      
      
      
      
      pts <- pts %>%
        dplyr::left_join(
          vote_lookup,
          by = c("party" = party_col)
        ) %>%
        dplyr::left_join(seat_lookup, by = "party")
      
      
      
      
      
      # dot sizes
      N <- nrow(pts)
      pt_size <- dplyr::case_when(
        N <= 30 ~ 26,
        N <= 50 ~ 22,
        N <= 100 ~ 14,
        N <= 150 ~ 12,
        TRUE ~ 7
      )
      
      
      
      
      
      # build series list for Highcharts
      series_list <- pts %>%
        dplyr::group_split(party) %>%
        purrr::map(function(df_party) {
          name <- as.character(df_party$party[1])
          votes <- df_party$total_votes[1]
          
          list(
            name = name,
            type = "scatter",
            data = lapply(seq_len(nrow(df_party)), function(i) {
              list(
                x = df_party$x[i],
                y = df_party$y[i],
                name = name,
                votes = df_party$total_votes[i],   # same for all party points
                seats = df_party$seats[i]          # same for all party points
              )
            }),
            marker = list(
              radius = pt_size,
              symbol = "circle",
              lineWidth = 0,
              fillColor = pal[[name]] %||% "#888888"
            )
          )
        })
      
      
      # JS for external legend interaction (mouseenter / mouseleave / click)
      js_events <- highcharter::JS(sprintf("
        function() {

          var legendId = '%s-legend';
          var chartId  = '%s';

          function getChart(){
            return Highcharts.charts.find(function(c){
              return c && c.renderTo && c.renderTo.id === chartId;
            });
          }

          var locked = null;
          var isBinding = false;

          function bindLegendEvents(){
            if (isBinding) return;
            isBinding = true;

            var root = document.getElementById(legendId);
            if(!root){
              isBinding = false;
              return;
            }

            function setInactive(name){
              var chart = getChart();
              if(!chart) {
                isBinding = false;
                return;
              }
              chart.series.forEach(function(s){
                if(!s.visible) return;
                if(s.name === name){
                  s.setState('hover');
                  if(s.group) s.group.toFront();
                  if(s.markerGroup) s.markerGroup.toFront();
                } else {
                  s.setState('inactive');
                }
              });
              root.querySelectorAll('[data-party]').forEach(function(el){
                var st = el.getAttribute('data-party');
                el.style.opacity = (st === name ? '1' : '0.4');
                el.style.boxShadow = (st === name
                  ? 'inset 0 0 0 1px #FFA92A'
                  : 'inset 0 0 0 1px #E6E6E6');
              });
            }

            function clearStates(){
              var chart = getChart();
              if(!chart){
                isBinding = false;
                return;
              }
              chart.series.forEach(function(s){
                if(!s.visible) return;
                s.setState('normal');
              });
              root.querySelectorAll('[data-party]').forEach(function(el){
                el.style.opacity = '1';
                el.style.boxShadow = 'inset 0 0 0 1px #E6E6E6';
              });
            }

            // Bind only once per element using a flag
            root.querySelectorAll('[data-party]').forEach(function(el){
              if (el.dataset.bound === '1') return; // already has listeners
              el.dataset.bound = '1';

              el.style.cursor = 'pointer';

              el.addEventListener('mouseenter', function(){
                if(locked) return;
                setInactive(this.getAttribute('data-party'));
              });

              el.addEventListener('mouseleave', function(){
                if(locked) return;
                clearStates();
              });

              el.addEventListener('click', function(){
                var st = this.getAttribute('data-party');
                if(locked === st){
                  locked = null;
                  clearStates();
                } else {
                  locked = st;
                  setInactive(st);
                }
              });
            });

            isBinding = false;
          }

          // Initial bind
          bindLegendEvents();

          // Watch for future changes (adding/removing parties)
          var legendRoot = document.getElementById(legendId);
          if(!legendRoot) return;

          var obs = new MutationObserver(function(){
            bindLegendEvents();
          });

          obs.observe(legendRoot, { childList: true, subtree: true });

        }
      ",
                                           session$ns("chart"),
                                           session$ns("chart")
      ))
      
      highcharter::highchart() %>%
        
        highcharter::hc_chart(
          type = "scatter",
          zoomType = NULL,
          animation = FALSE,
          marginBottom = 40,
          options = list(
            lang = list(
              thousandsSep = ","
            )
          ),
          events = list(load = js_events)
        ) %>%
        highcharter::hc_xAxis(
          visible = FALSE,
          min = -1.1,
          max =  1.1
        ) %>%
        highcharter::hc_yAxis(
          visible = FALSE,
          min = 0,
          max = 1.1
        ) %>%
        hc_plotOptions(
          scatter = list(
            animation = list(
              duration = 0,    # quick
              easing = "easeOutQuad"
            ),
            states = list(
              inactive = list(
                enabled = TRUE,
                opacity = 0.2   # softer inactive state
              ),
              hover = list(enabled = TRUE)
            )
          ),
          series = list(
            turboThreshold = 0,
            animation = list(
              duration = 0,
              easing = "easeOutQuad"
            )
          )
        ) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_exporting(enabled = FALSE) %>%
        highcharter::hc_credits(enabled = FALSE) %>%
        highcharter::hc_add_series_list(series_list) %>% 
        highcharter::hc_tooltip(
          useHTML = TRUE,
          headerFormat = "",
          pointFormat = paste0(
            "<b>{point.name}</b><br>",
            "Votes: {point.votes:,.0f}<br>",
            "Seats: {point.seats}"
          )
        )
    })
    
    #-------------------------------
    # External legend (colors synced with chart)
    #-------------------------------
    output$legend <- renderUI({
      req(data_r(), state_r(), chamber_r(), year_r())
      df <- data_r()
      
      dff <- df %>% dplyr::filter(.data[[seats_col]] != 0)
      sel_states  <- tryCatch(state_r(), error = function(e) NULL)
      if (length(sel_states) == 1 && identical(sel_states, "")) sel_states <- NULL
      sel_states  <- if (!is.null(sel_states)) as.character(sel_states) else NULL
      sel_year    <- suppressWarnings(as.integer(tryCatch(year_r(), error = function(e) NA_integer_)))
      sel_chamber <- suppressWarnings(as.integer(tryCatch(chamber_r(), error = function(e) NA_integer_)))
      if (!is.null(sel_states) && length(sel_states) > 0) dff <- dff %>% dplyr::filter(.data[[state_col]] %in% sel_states)
      if (!is.na(sel_chamber)) dff <- dff %>% dplyr::filter(.data[[chamber_filter_col]] == sel_chamber)
      if (!is.na(sel_year))    dff <- dff %>% dplyr::filter(.data[[year_col]] == sel_year)
      
      agg_base <- dff %>%
        dplyr::transmute(party = .data[[party_col]], seats = .data[[seats_col]]) %>%
        dplyr::group_by(party) %>%
        dplyr::summarise(seats = sum(seats, na.rm = TRUE), .groups = "drop")
      
      total_chamber_vec <- suppressWarnings(as.integer(na.omit(dff$total_chamber_seats_sub_leg)))
      total_chamber <- if (length(total_chamber_vec)) {
        tb <- sort(table(total_chamber_vec), decreasing = TRUE)
        cand <- as.integer(names(tb[ tb == max(tb) ]))
        max(cand)
      } else NA_integer_
      
      agg <- agg_base
      if (!is.na(total_chamber)) {
        falta <- total_chamber - sum(agg_base$seats, na.rm = TRUE)
        if (falta > 0) {
          agg <- dplyr::bind_rows(
            agg_base,
            tibble(party = previous_name, seats = falta)
          )
        }
      }
      
      agg <- agg %>%
        dplyr::mutate(.is_prev = as.integer(party == previous_name)) %>%
        dplyr::arrange(.is_prev, dplyr::desc(seats), party) %>%
        dplyr::select(-.is_prev)
      
      if (nrow(agg) == 0) return(NULL)
      
      # pal <- palette_distinct(agg$party)
      # if (previous_name %in% names(pal)) pal[previous_name] <- previous_color
      
      pal <- palette_from_table(
        parties = agg$party,
        color_table = party_colors_leg
      )
      
      if (previous_name %in% names(pal)) {
        pal[previous_name] <- previous_color
      }
      
      
      
      
      items <- lapply(seq_len(nrow(agg)), function(i) {
        p <- agg$party[i]
        col <- pal[[p]] %||% "#888"
        htmltools::tags$span(
          class = "legend-item",
          `data-party` = p,
          htmltools::span(class = "legend-swatch", style = paste0("background:", col, ";")),
          htmltools::span(class = "legend-text", p)
        )
      })
      
      htmltools::div(
        id = paste0(session$ns("chart"), "-legend"),
        class = "legend-wrap",
        htmltools::div(
          class = "legend-group",
          htmltools::div(class = "legend-row", items)
        )
      )
    })
    
  })
}

