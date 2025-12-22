# to_title_case <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
# }
# 
# get_jstree_data <- function(df) {
#   tree_data <- list()
#   countries <- unique(df$country_name)
#   for (country in countries) {
#     country_node <- list(
#       id = country,
#       text = country,
#       children = list()
#     )
#     states <- unique(df$state_name[df$country_name == country &  !is.na(data$state_name)]) %>% sort()
#     for (state in states) {
#       # Muestra el nombre en formato de nombre propio en el árbol
#       state_display_name <- to_title_case(state)
#       state_node <- list(
#         # El ID se mantiene en mayúsculas para el filtrado
#         id = paste(country, state, sep = "-"),
#         text = state_display_name
#       )
#       country_node$children[[length(country_node$children) + 1]] <- state_node
#     }
#     tree_data[[length(tree_data) + 1]] <- country_node
#   }
#   return(toJSON(tree_data, auto_unbox = TRUE))
# }
# 

# ------------------------------------------
# Better title-case helper (vectorized)
# ------------------------------------------
to_title_case <- function(x) {
  sapply(x, function(txt) {
    words <- strsplit(txt, " ")[[1]]
    words <- paste0(toupper(substring(words, 1, 1)),
                    tolower(substring(words, 2)))
    paste(words, collapse = " ")
  })
}

# ------------------------------------------
# Fancytree data builder (replicates jsTree)
# ------------------------------------------
get_fancytree_data_states <- function(df) {
  
  tree_data <- list()
  countries <- unique(df$country_name)
  
  for (country in countries) {
    
    # Initialize list of children
    state_nodes <- list()
    
    # Extract states for this country
    states <- df$state_name[df$country_name == country & !is.na(df$state_name)] |> sort() %>% unique()
    
    # Apply title-case to displayed names
    titles <- to_title_case(states)
    
    # Build state nodes
    if (length(states) > 0) {
      state_nodes <- lapply(seq_along(states), function(i) {
        list(
          key   = paste(country, states[i], sep = "-"),   # Key remains uppercase for filtering
          title = titles[i]                               # Pretty display
        )
      })
    }
    
    # Build country folder node
    country_node <- list(
      key = country,
      title = country,
      folder = TRUE,
      children = state_nodes
    )
    
    tree_data[[length(tree_data) + 1]] <- country_node
  }
  
  jsonlite::toJSON(tree_data, auto_unbox = TRUE, null = "null")
}


# # Build a JSTree where each dataset is a parent; for SLED add Lower/Upper branches
# get_jstree_data_vars <- function(dict, chamber_structure = TRUE) {
#   tree_data <- list()
#   datasets <- unique(dict$dataset)
#   for (ds in datasets) {
#     ds_node <- list(
#       id = ds,
#       text = ds,
#       children = list()
#     )
#     
#     vars <- unique(dict$pretty_name[dict$dataset == ds])
#     
#     if (identical(ds, "Legislative Elections")) {
#       # Build LOWER branch
#     
#       
#       lower_children <- lapply(vars, function(v) {
#         list(
#           id   = paste("Legislative Elections", "Lower Chamber", v, sep = "-"),  # e.g., "SLED-Lower-Voter Turnout Percentage"
#           text = v
#         )
#       })
#       lower_node <- list(
#         id = "Legislative Elections-Lower Chamber",
#         text = "Lower Chamber",
#         children = lower_children
#       )
#       
#       # Build UPPER branch
#       upper_children <- lapply(vars, function(v) {
#         list(
#           id   = paste("Legislative Elections", "Upper Chamber", v, sep = "-"),  # e.g., "SLED-Upper-Voter Turnout Percentage"
#           text = v
#         )
#       })
#       upper_node <- list(
#         id = "Legislative Elections-Upper Chamber",
#         text = "Upper Chamber",
#         children = upper_children
#       )
#     
#       if (chamber_structure) {
#         chamber_node <- list(
#           id = "Legislative Elections-Type of Chamber",
#           text = "Type of Chamber"
#         )
#         
#         ds_node$children <- append(ds_node$children,
#                                    list(chamber_node,lower_node, upper_node))
#       } else {
#         
#         ds_node$children <- append(ds_node$children,
#                                    list(lower_node, upper_node))
#         }
#       
#         
#       
#       
#     } else {
#       # Default: dataset -> variables (single level)
#       for (v in vars) {
#         var_node <- list(
#           id   = paste(ds, v, sep = "-"),
#           text = v
#         )
#         ds_node$children[[length(ds_node$children) + 1]] <- var_node
#       }
#     }
#     
#     tree_data[[length(tree_data) + 1]] <- ds_node
#   }
#   return(jsonlite::toJSON(tree_data, auto_unbox = TRUE))
# }


get_fancytree_data_vars <- function(dict, chamber_structure = TRUE) {
  
  # ------------------------------------------------------
  # Helper: create a node safely (never produces data.frame)
  # ------------------------------------------------------
  make_node <- function(key, title, children = NULL) {
    node <- list(
      key = key,
      title = title
    )
    
    if (!is.null(children) && length(children) > 0) {
      node$folder <- TRUE
      node$children <- children
    }
    
    return(node)
  }
  
  # ------------------------------------------------------
  # Helper: recursively clean nodes so Fancytree never receives NULL children
  # ------------------------------------------------------
  clean_node <- function(node) {
    
    # If no children → remove children and folder
    if (is.null(node$children) || length(node$children) == 0) {
      node$children <- NULL
      node$folder <- NULL
      return(node)
    }
    
    # If children exist → folder must be TRUE
    node$folder <- TRUE
    
    # Clean all children recursively
    node$children <- lapply(node$children, clean_node)
    
    return(node)
  }
  
  # ------------------------------------------------------
  # Build tree
  # ------------------------------------------------------
  datasets <- unique(dict$dataset)
  tree_data <- list()
  
  for (ds in datasets) {
    
    vars <- unique(dict$pretty_name[dict$dataset == ds])
    
    # Top-level dataset node
    ds_node_children <- list()
    
    if (identical(ds, "Legislative Elections")) {
      
      # --- LOWER CHAMBER ---
      lower_children <- lapply(vars, function(v) {
        make_node(
          key   = paste("Legislative Elections", "Lower Chamber", v, sep = "-"),
          title = v
        )
      })
      
      lower_node <- make_node(
        key = "Legislative Elections-Lower Chamber",
        title = "Lower Chamber",
        children = lower_children
      )
      
      # --- UPPER CHAMBER ---
      upper_children <- lapply(vars, function(v) {
        make_node(
          key   = paste("Legislative Elections", "Upper Chamber", v, sep = "-"),
          title = v
        )
      })
      
      upper_node <- make_node(
        key = "Legislative Elections-Upper Chamber",
        title = "Upper Chamber",
        children = upper_children
      )
      
      # --- Optional "Type of Chamber" (leaf) ---
      if (chamber_structure) {
        chamber_node <- make_node(
          key = "Legislative Elections-Type of Chamber",
          title = "Type of Chamber"
        )
        
        ds_node_children <- list(chamber_node, lower_node, upper_node)
        
      } else {
        ds_node_children <- list(lower_node, upper_node)
      }
      
    } else {
      
      # --- DEFAULT DATASET: simple list of variables ---
      ds_node_children <- lapply(vars, function(v) {
        make_node(
          key   = paste(ds, v, sep = "-"),
          title = v
        )
      })
    }
    
    # Build top-level dataset node
    ds_node <- make_node(
      key = ds,
      title = ds,
      children = ds_node_children
    )
    
    tree_data[[length(tree_data) + 1]] <- ds_node
  }
  
  # ------------------------------------------------------
  # Final cleaning: no nulls, no NA, no bad structures
  # ------------------------------------------------------
  tree_data <- lapply(tree_data, clean_node)
  
  # Return JSON string
  jsonlite::toJSON(tree_data, auto_unbox = TRUE, null = "null")
}


