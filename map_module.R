get_leaflet_palette <- function(type, palette_vector, values) {
  if (length(values) == 0) return(list(pal = NULL, legend = NULL, domain = NULL, colors = NULL))
  
  na_color <- "#999999"  # default NA color
  
  if (type == "binary") {
    domain <- c(0, 1)
    pal <- colorFactor(palette = palette_vector, domain = domain, na.color = na_color)
    legend_labels <- c("No", "Yes")
    
  } else if (type == "gender") {
    domain <- c(0, 1,2)
    pal <- colorFactor(palette = palette_vector, domain = domain, na.color = na_color)
    legend_labels <- c("Male", "Female","Other")
    
  } else if (type == "chamber") {
    domain <- c(1,2)
    pal <- colorFactor(palette = palette_vector, domain = domain, na.color = na_color)
    legend_labels <- c("Unicameral", "Bicameral")
    
  } else if (type == "system") {
    domain <- c(1,2,3,4)
    pal <- colorFactor(palette = palette_vector, domain = domain, na.color = na_color)
    legend_labels <- c(
      "Proportional Representation (PR)",
      "Simple Majority",
      "Mixed (PR + Majority)",
      "Mixed (PR + predefined districts)"
    )
    
  } else if (type == "renewal") {
    domain <- c(1,2)
    pal <- colorFactor(palette = palette_vector, domain = domain, na.color = na_color)
    legend_labels <- c("Staggered every 2 years", "Full renewal")
    
  } else if (type == "categorical") {
    
    domain <- sort(unique(values))
    
    # Handle 'party_colors' dependency more gracefully for self-contained example
    if (!exists("party_colors", envir = .GlobalEnv) || !is.data.frame(get("party_colors", envir = .GlobalEnv))) {
      warning("`party_colors` not found or not a data.frame. Using a default palette for 'categorical' type.")
      # Fallback to a generic color palette if party_colors is not defined
      palette_vector <- RColorBrewer::brewer.pal(max(3, length(domain)), "Set1")[seq_along(domain)]
    } else {
      # Assuming party_colors is a data.frame with 'head_party_sub' and 'color'
      party_colors_df <- get("party_colors", envir = .GlobalEnv)
      party_colors_subset <- party_colors_df %>%
        filter(head_party_sub_exe %in% domain)
      
      party_colors_ordered <- party_colors_subset %>%
        slice(match(domain, head_party_sub_exe))
      
      palette_vector <- party_colors_ordered$color
      if (length(palette_vector) == 0 && length(domain) > 0) {
        warning("No colors found in `party_colors` for the current domain. Using a default palette.")
        palette_vector <- RColorBrewer::brewer.pal(max(3, length(domain)), "Set1")[seq_along(domain)]
      }
    }
    
    pal <- colorFactor(palette = palette_vector, domain = domain, na.color = na_color)
    legend_labels <- domain %>% stringr::str_to_title()
    
  } else if (type == "ordinal") {
    domain <- 1:4
    pal <- colorFactor(palette = palette_vector, domain = domain, na.color = na_color)
    legend_labels <- c("Left", "Center Left", "Center Right", "Right")
    
  } else if (type %in% c("discrete", "continuous","percentage")) {
    pal_list <- tryCatch({
      # Ensure values are not all NA for classInt
      valid_values <- values[!is.na(values)] %>% as.double()
      if (length(valid_values) == 0) {
        return(list(pal = NULL, legend = NULL, domain = NULL, colors = NULL))
      }
      
      ci <- classInt::classIntervals(valid_values, n = length(palette_vector), style = "jenks")
      breaks <- ci$brks
      
      if (anyDuplicated(breaks)) {
        ci <- classInt::classIntervals(valid_values, n = length(palette_vector), style = "pretty")
        breaks <- ci$brks
      }
      
      pal <- colorBin(palette = palette_vector, domain = values, bins = breaks, pretty = TRUE, na.color = na_color)
      
      if (type %in% c("discrete", "continuous")) {
        n_round <- ifelse(type == "continuous", 2, 0)
        legend_labels <- paste0(
          format(round(breaks[-length(breaks)], n_round), nsmall = n_round, big.mark = ","),
          " - ",
          format(round(breaks[-1], n_round), nsmall = n_round, big.mark = ",")
        )
        
      } else {
        legend_labels <- paste0(
          format(round(breaks[-length(breaks)], 2), nsmall = 2, big.mark = ","),
          "% - ",
          format(round(breaks[-1], 2), nsmall = 2, big.mark = ","),
          "%"
        )
      }
      
      list(pal = pal, legend = c("Not available", legend_labels), domain = c(NA,values), colors = NULL)
      
    }, error = function(e) {
      if (grepl("single unique value", e$message) && length(unique(valid_values)) == 1) {
        val <- unique(valid_values)[1]
        breaks <- c(val, val + 1e-6) # Small range to define a bin for single value
        pal <- colorBin(palette = tail(palette_vector, 1), domain = values, bins = breaks, pretty = FALSE, na.color = na_color)
        legend_labels <- c(format_leaflet_value(val, type)) # Display the single value
        return(list(pal = pal, legend = c("Not available", legend_labels), domain = c(NA,values), colors = NULL))
      } else {
        warning(paste("Error in get_leaflet_palette (discrete/continuous/percentage):", e$message))
        return(list(pal = NULL, legend = NULL, domain = NULL, colors = NULL))
      }
    })
    return(pal_list) # Return the entire list
  } else {
    return(list(pal = NULL, legend = NULL, domain = NULL, colors = NULL))
  }
  
  # Add NA label for non-numeric types
  if (type == "gender") {
    legend_labels <- c(legend_labels,"Not available")
    domain <- c(domain, NA)
  } else if (!is.null(pal) && !(type %in% c("discrete", "continuous", "percentage"))) {
    legend_labels <- c("Not available", legend_labels)
    domain <- c(NA, domain)
  }
  
  return(list(
    pal = pal,
    legend = legend_labels,
    domain = domain,
    colors = c(palette_vector, na_color)
  ))
}


addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors,
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = getMapData(map), decreasing = TRUE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors))
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula"))
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins))
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1)
        pretty(values, bins)
      else bins
      if (length(bins) > 2)
        if (!all(abs(diff(bins, differences = 2)) <=
                 sqrt(.Machine$double.eps)))
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      full_levels <- sort(unique(na.omit(values)))
      
      # Use full levels to get colors and labels
      if (decreasing == TRUE) {
        colors <- pal(rev(full_levels))
        labels <- rev(labFormat(type = "factor", full_levels))
      } else {
        colors <- pal(full_levels)
        labels <- labFormat(type = "factor", full_levels)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values)))
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels))
      stop("'colors' and 'labels' must be of the same length")
  }
  if (all(is.na(values))) {
    colors <- na.color
    labels <- na.label
    na.color <- NULL  # evita mostrar color NA doble
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                 na_color = na.color, na_label = na.label, opacity = opacity,
                 position = position, type = type, title = title, extra = extra,
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

format_leaflet_value <- function(value, type) {
  # Asegurarse que type tenga misma longitud o es único
  if (length(type) == 1) {
    type <- rep(type, length(value))
  }
  
  out <- vector("character", length(value))
  
  for (i in seq_along(value)) {
    if (is.na(value[i])) {
      out[i] <- "Not available"
    } else {
      out[i] <- switch(type[i],
                       "binary" = ifelse(value[i] == 1, "Yes", "No"),
                       "gender" = case_when(
                         value[i] == "0" ~ "Male",
                         value[i] == "1" ~ "Female",
                         value[i] == "2" ~ "Other",
                         TRUE ~ as.character(value[i])
                       ),
                       "chamber" = case_when(
                         value[i] == "1" ~ "Unicameral",
                         value[i] == "2" ~ "Bicameral",
                         TRUE ~ as.character(value[i])
                       ),
                       "system" = case_when(
                         value[i] == "1" ~ "Proportional Representation (PR)",
                         value[i] == "2" ~ "Simple Majority",
                         value[i] == "3" ~ "Mixed (PR + Majority)",
                         value[i] == "4" ~ "Mixed (PR + predefined districts)",
                         TRUE ~ as.character(value[i])
                       ),
                       "renewal" =case_when(
                         value[i] == "1" ~ "Staggered every 2 years",
                         value[i] == "2" ~ "Full renewal",
                         TRUE ~ as.character(value[i])
                       ),
                       "ordinal" = dplyr::case_when(
                         value[i] == 1 ~ "Left",
                         value[i] == 2 ~ "Center Left",
                         value[i] == 3 ~ "Center Right",
                         value[i] == 4 ~ "Right",
                         TRUE ~ as.character(value[i])
                       ),
                       "categorical" = stringr::str_to_title(value[i]),
                       "discrete" = format(value[i] %>% as.double(), big.mark = ",", scientific = FALSE),
                       "continuous" = format(round(value[i]%>% as.double(), 2), nsmall = 2, big.mark = ","),
                       "percentage" = paste0(format(round(as.double(value[i]), 2), nsmall = 2, big.mark = ","),"%"),
                       as.character(value[i]) # fallback
      )
    }
  }
  
  return(out)
}

# --- Map Module UI ---

# mapModuleUI function
mapModuleUI <- function(id) {
  ns <- NS(id)
  map_id <- ns("map")
  
  bootstrapPage(
    
      tags$head(
        tags$script(src = "https://unpkg.com/leaflet-easyprint@2.1.9/dist/bundle.js"),
        tags$script(src = "leaflet-export.js")
      ),
    
    div(
      class = "outer",
      tags$style(
        type = "text/css",
        ".outer {
          position: fixed;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          overflow: hidden;
          padding: 0;
        }"
      ),
      leafletOutput(map_id, height = "100%")
    )
  )
}

# --- Map Module Server ---

# mapModuleServer function
mapModuleServer <- function(id, data_map, input_var_sel, dict, country_bboxes, input_country_sel = "ARGENTINA", active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    var_info <- reactive({
      clean_var <- sub("_[12]$", "", input_var_sel())  # remove "_1" or "_2" at the end
      dict %>% 
        filter(variable == clean_var) %>% 
        slice(1)
    })
    
    
    df_map <- reactive({
      req(data_map(), input_var_sel(), active_tab() == "map_tab")
      data <- data_map()
      data[[".leaflet_value"]] <- data[[input_var_sel()]]
      if (var_info()$type %in% c("discrete", "continuous","percentage")) {
        data[[".leaflet_value"]] <- as.double(data[[".leaflet_value"]])
      }
      data
    })
    
    values <- reactive({
      df_map()[[".leaflet_value"]]
    })
    
    
    
    
    palette_vector <- reactive({
      unlist(strsplit(var_info()$palette, ","))
    })
    
    pal <- reactive({
      get_leaflet_palette(var_info()$type, palette_vector(), values())
    })
    
    prev_domain <- reactiveVal(NULL)
    prev_country <- reactiveVal(NULL)
    prev_var <- reactiveVal(NULL)
    prev_tab <- reactiveVal(NULL)  # Nueva variable reactiva para trackear el tab anterior
    
    output$map <- renderLeaflet({
      req(active_tab() == "map_tab", input_country_sel() != "", input_var_sel())
      
      leaflet(options = leafletOptions(preferCanvas = TRUE, zoomControl = FALSE)) %>%
        fitBounds(
          lng1 = country_bboxes[[input_country_sel()]]$lng1,
          lat1 = country_bboxes[[input_country_sel()]]$lat1,
          lng2 = country_bboxes[[input_country_sel()]]$lng2,
          lat2 = country_bboxes[[input_country_sel()]]$lat2
        ) %>%
        addProviderTiles("CartoDB.PositronNoLabels") # clean white base, no labels
    })
    

    
    observe({
      # Detecta cambio de tab primero, sin req que bloquee
      tab_changed <- !identical(active_tab(), prev_tab())
      prev_tab(active_tab())  # Actualiza inmediatamente el tab anterior
      
      # Solo procede si estamos en map_tab
      if (active_tab() != "map_tab") return()
      
      # Ahora sí requerimos los datos
      req(df_map(), input_var_sel(), input_country_sel())
      
      proxy <- leafletProxy(ns("map"), data = df_map())
      
      if (nrow(df_map()) == 0 || all(is.na(df_map()[[".leaflet_value"]]))) {
        proxy %>%
          clearShapes() %>%
          clearControls()

        
        prev_domain(NULL)
        prev_country(NULL)
        prev_var(NULL)
        return()
      }
      
      current_domain <- pal()$domain
      domain_changed <- !identical(current_domain, prev_domain())
      country_changed <- !identical(input_country_sel(), prev_country())
      var_changed <- !identical(input_var_sel(), prev_var())
      
      proxy %>%
        addPolygons(
          layerId = ~country_state_code,
          fillColor = ~pal()$pal(.leaflet_value),
          color = "#111",
          weight = 1.2,
          fillOpacity = 0.9,
          highlightOptions = highlightOptions(weight = 5, color = "#666", fillOpacity = 1),
          label = ~paste0(stringr::str_to_title(state_name_geom), ": ", format_leaflet_value(.leaflet_value, var_info()$type)),
          popup = ~paste0(
            #"<div style='background-color:#041d2d; color:#f4e842; padding:6px 6px;
            "<style>
            /* Base chevron */
            details summary::before {
            content: '▶';
            float: right;
            display: inline-block;
            transition: transform 0.2s ease-in-out;
            }
            /* Rotate when open */
            details[open] summary::before {
            transform: rotate(90deg);
            }
            /* Normal state (blue + underline) */
            details summary {
            color: var(--magenta);
            text-decoration:underline;
            cursor:pointer;
            display:inline-flex;
            font-weight:600;
            }
            /* Open state (darker, no underline) */
            details[open] summary {
            color:var(--purple);
            text-decoration:none;
            }
            </style>",
            
            "<div style='color:#00000; padding:1px 1px;
            border-radius:1px; font-weight:bold; font-size:15px; text-align:left;'>",
            var_info()$pretty_name, ": ", format_leaflet_value(.leaflet_value, var_info()$type),
            "</div>",
            
            "<div style='border:0; height:1px; 
            background:linear-gradient(90deg, rgba(255,169,42,0), rgba(255,169,42,0.8), rgba(255,169,42,0));
            margin:1px 0;'></div>",
            "<b>State:</b> ", stringr::str_to_title(state_name_geom), "<br/>",
            "<b>Governor:</b> ", stringr::str_to_title(winner_candidate_sub_exe), "<br/>",
            "<b>Party:</b> ", stringr::str_to_title(head_party_sub_exe), "<br/>",
            "<b>Chamber Structure:</b> ", case_when(chamber_sub_leg == 1 ~ "Unicameral",
                                                    chamber_sub_leg == 2 ~ "Bicameral",
                                                    is.na(chamber_sub_leg)~ "Not available"), "<br/>",
            
            "<details style='margin-top:4px;'>",
            "<summary>Governor details</summary>",
            "<div style='margin-top:1px;'>",
            "<b>Party Ideology:</b> ", dplyr::case_when(
              ideo_party_sub_exe == 1 ~ "Left",
              ideo_party_sub_exe == 2 ~ "Center Left",
              ideo_party_sub_exe == 3 ~ "Center Right",
              ideo_party_sub_exe == 4 ~ "Right",
              TRUE ~ as.character(ideo_party_sub_exe)
            ), "<br/>",
            "<b>Alignment:</b> ", ifelse(alignment_with_nat_sub_exe == 1, "Yes", "No"), "<br/>",
            "<b>Reelected:</b> ", ifelse(consecutive_reelection_sub_exe == 1, "Yes", "No"), "<br/>",
            "</div>",
            "</details>",
            
            ifelse(is.na(chamber_sub_leg),"",
                   paste0("<details style='margin-top:4px;'>",
                          "<summary>Legislative details</summary>",
                          "<div style='margin-top:1px;'>",
                          "<b>Lower Chamber seats:</b> ", total_chamber_seats_sub_leg_1, "<br/>",
                          ifelse(chamber_sub_leg == 1,"",
                                 paste0("<b>Upper Chamber seats:</b> ", total_chamber_seats_sub_leg_2, "<br/>")),
                          "</div>",
                          "</details>"
                          ))
            )
        )
      
      
      # Incluye tab_changed en la condición para re-renderizar la leyenda
      if (domain_changed || country_changed || var_changed || tab_changed) {
        proxy %>%
          clearControls() %>%
          addLegend_decreasing(
            position = "bottomright",
            pal = pal()$pal,
            values = pal()$domain,
            labFormat = function(type, cuts, p) { pal()$legend },
            opacity = 0.9,
            title = var_info()$pretty_name,
            decreasing = if (var_info()$type=="gender") FALSE else TRUE
          )
      }
      

      
      
      prev_domain(current_domain)
      prev_country(input_country_sel())
      prev_var(input_var_sel())
      
      
      # --- after polygons and legend updates ---
      later::later(function() {
        session$sendCustomMessage("addExportButton", list(mapId = paste0(id, "-map")))
      }, 0.0000001)
    })
    
    
  })
}