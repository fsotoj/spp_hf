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
palette_distinct <- function(labels) {
  labs <- unique(as.character(labels))
  n <- length(labs)
  if (n == 0) return(character())
  hues <- seq(15, 375, length.out = n + 1)[-1]
  cols <- grDevices::hcl(h = hues, c = 100, l = 60)
  stats::setNames(cols, labs)
}

#-------------------------------
# Module UI (chart only)
#-------------------------------
camaraUI <- function(id, height = "70vh") {
  ns <- NS(id)
  echarts4rOutput(ns("chart"), height = height)
}

#-------------------------------
# External Legend UI (place anywhere, e.g., below your last box)
#  - Non-clickable
#  - Colors strictly synced with chart colors
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

      /* NOW SCROLLABLE */
      #%1$s-legend .legend-wrap {
        display: flex;
        flex-direction: column;
        gap: 12px;

        max-height: 240px;   /* Adjust height as needed */
        overflow-y: auto;
        padding-right: 6px;  /* Space for scrollbar */
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

      #%1$s-legend .legend-country {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        color: #4D4D4D;
        font-weight: 700;
        font-size: 12px;
        opacity: 0.95;
      }

      #%1$s-legend .country-swatch {
        width: 10px;
        height: 10px;
        border-radius: 2px;
        display: inline-block;
        box-shadow: 0 0 0 1px #DDDDDD;
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


#-------------------------------
# Module Server
#-------------------------------
camaraServer <- function(id,
                         data,                      # static data.frame OR reactive
                         state_r,                   # reactive: estado (string) o vector; "" se ignora
                         chamber_r,                 # reactive: 1/2 (integer/char)
                         year_r,                    # reactive: year (integer/char)
                         party_col   = "party_name_sub_leg",
                         seats_col   = "total_seats_party_sub_leg",
                         state_col   = "state_name",
                         chamber_filter_col = "chamber_election_sub_leg",
                         year_col    = "year",
                       #  title_text  = "Chamber composition",
                         previous_name  = "NON-CONTESTED SEATS",
                         previous_color = "rgba(154,160,166,0.40)") {
  
  moduleServer(id, function(input, output, session) {
    
    # --- Reactive data wrapper ---
    data_r <- if (inherits(data, "reactive")) data else reactive(data)
    
    output$chart <- renderEcharts4r({
      req(state_r(), chamber_r(), year_r())
      df <- data_r()
      if (!inherits(df, c("data.frame","tbl","tbl_df"))) {
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>%
            e_charts(x) %>% e_scatter(y) %>%
            e_title("No data (invalid input table)") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # columns check
      cols_needed <- c(state_col, chamber_filter_col, year_col, party_col, seats_col)
      missing <- setdiff(cols_needed, names(df))
      if (length(missing) > 0) {
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>% e_charts(x) %>% e_scatter(y) %>%
            e_title(paste("Missing columns:", paste(missing, collapse = ", "))) %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
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
      dff <- df %>% filter(.data[[seats_col]] != 0)
      if (!is.null(sel_states) && length(sel_states) > 0) {
        dff <- dff %>% filter(.data[[state_col]] %in% sel_states)
      }
      if (!is.na(sel_chamber)) {
        dff <- dff %>% filter(.data[[chamber_filter_col]] == sel_chamber)
      }
      if (!is.na(sel_year)) {
        dff <- dff %>% filter(.data[[year_col]] == sel_year)
      }
      
      if (nrow(dff) == 0) {
        return(
          tibble(x = 0, y = 0, party = "No data") %>%
            group_by(party) %>% e_charts(x) %>% e_scatter(y) %>%
            e_title("No data for current selection") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # aggregate base
      agg_base <- dff %>%
        transmute(party = .data[[party_col]], seats = .data[[seats_col]]) %>%
        group_by(party) %>%
        summarise(seats = sum(seats, na.rm = TRUE), .groups = "drop")

      
      
      # total chamber + NON-CONTESTED seats (exactly as requested)
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
          agg <- bind_rows(
            agg_base,
            tibble(party = previous_name, seats = falta)
          )
        }
      }
      

      # order: real by size, non-contested last
      agg <- agg %>%
        mutate(.is_prev = as.integer(party == previous_name)) %>%
        arrange(.is_prev, desc(seats), party) %>%
        select(-.is_prev)
      
      # expand seats (force non-contested to the right/end)
      pts <- expand_and_assign(
        agg,
        party_col = "party",
        seats_col = "seats",
        non_contested_label = previous_name
      )
      if (nrow(pts) == 0) {
        return(
          tibble(x = 0, y = 0, party = "No seats") %>%
            group_by(party) %>% e_charts(x) %>% e_scatter(y) %>%
            e_title("No seats to plot") %>%
            e_x_axis(show = FALSE) %>% e_y_axis(show = FALSE)
        )
      }
      
      # series order & palette (SYNC SOURCE for legend)
      pts <- pts %>% mutate(party = factor(party, levels = agg$party))
      pal <- palette_distinct(levels(pts$party))
      if (previous_name %in% names(pal)) pal[previous_name] <- previous_color
      
      # dot sizes
      N <- nrow(pts)
      pt_size <- dplyr::case_when(
        N <= 30 ~ 60, N <= 50 ~ 40, N <= 100 ~ 30, N <= 150 ~ 20,
        TRUE ~ 16
      )
      
      # chart (legend OFF; external legend mirrors 'agg' & 'pal')
      pts %>%
        group_by(party) %>%
        e_charts(x) %>%
        e_scatter(
          y,
          name = ~party,
          symbolSize = pt_size,
          itemStyle = list(opacity = 0.95)
        ) %>%
        e_color(unname(pal[levels(pts$party)])) %>%
        e_x_axis(min = -1.1, max = 1.1, show = FALSE) %>%
        e_y_axis(min = 0,   max = 1.1, show = FALSE) %>%
        e_legend(show = FALSE) %>%
        e_grid(left = "4%", right = "2%", top = "0%", bottom = "10%", containLabel = FALSE) %>%
        #e_title(title_text) %>%
        e_animation(duration = 100)
    })
    
    # ---- External legend (NON-clickable). Colors synced with plot ----
    output$legend <- renderUI({
      req(data_r(), state_r(), chamber_r(), year_r())
      df <- data_r()
      
      # replicate the same filter + aggregation used for the chart
      dff <- df %>% filter(.data[[seats_col]] != 0)
      sel_states  <- tryCatch(state_r(), error = function(e) NULL)
      if (length(sel_states) == 1 && identical(sel_states, "")) sel_states <- NULL
      sel_states  <- if (!is.null(sel_states)) as.character(sel_states) else NULL
      sel_year    <- suppressWarnings(as.integer(tryCatch(year_r(), error = function(e) NA_integer_)))
      sel_chamber <- suppressWarnings(as.integer(tryCatch(chamber_r(), error = function(e) NA_integer_)))
      if (!is.null(sel_states) && length(sel_states) > 0) dff <- dff %>% filter(.data[[state_col]] %in% sel_states)
      if (!is.na(sel_chamber)) dff <- dff %>% filter(.data[[chamber_filter_col]] == sel_chamber)
      if (!is.na(sel_year))    dff <- dff %>% filter(.data[[year_col]] == sel_year)
      
      agg_base <- dff %>%
        transmute(party = .data[[party_col]], seats = .data[[seats_col]]) %>%
        group_by(party) %>% summarise(seats = sum(seats, na.rm = TRUE), .groups = "drop")
      
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
          agg <- bind_rows(
            agg_base,
            tibble(party = previous_name, seats = falta)
          )
        }
      }
      
      agg <- agg %>%
        mutate(.is_prev = as.integer(party == previous_name)) %>%
        arrange(.is_prev, desc(seats), party) %>%
        select(-.is_prev)
      
      if (nrow(agg) == 0) return(NULL)
      
      # Build palette in the exact same order as the chart
      pal <- palette_distinct(agg$party)
      if (previous_name %in% names(pal)) pal[previous_name] <- previous_color
      
      # Build items according to 'agg$party' order so colors line up 1:1
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
      
      htmltools::div(id = paste0(session$ns("chart"), "-legend"), class = "legend-wrap",
                     htmltools::div(class = "legend-group",
                                    htmltools::div(class = "legend-row", items)
                     )
      )
    })
    
  })
}
