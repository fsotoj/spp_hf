

ui <- dashboardPage(  
  dashboardHeader(
    title = tags$div(
      class = "app-header-logo",
      tags$img(src = "spp_logo_v5.svg", height = "50px"),
      tags$div(
        class = "tec-logo-title",  
        tags$img(
          src = "EscuelaCienciasSocialesyGobierno_Horizontal_Negro.jpg",
          height = "36px",
          style = "margin-left: 10px; vertical-align: middle;"
        )
      )
    ),
    
    titleWidth = 250,
    
    #--- TOP NAVIGATION MENU ---
    # tags$li(class = "dropdown header-tab", a(href="#", id="tab_about", icon("info-circle"), "About")),
    # tags$li(class = "dropdown header-tab", a(href="#", id="tab_map", icon("map"), "Mapping tool")),
    # tags$li(class = "dropdown header-tab", a(href="#", id="tab_graph", icon("chart-line"), "Graphing tool")),
    # tags$li(class = "dropdown header-tab", a(href="#", id="tab_camera", icon("landmark"), "Camera Viz")),
    # tags$li(class = "dropdown header-tab", a(href="#", id="tab_codebook", icon("book-open"), "Codebook")),
    # tags$li(class = "dropdown header-tab", a(href="#", id="tab_data", icon("table"), "Databases")),
    
    
    
    # How to button
    tags$li(
      class = "dropdown",
      tags$a(
        id = "btn_howto",
        href = "#",
        icon("circle-question"),
        span(class = "hidden-xs", " How to")
      )
    ),
    
    
    
    # --- Logos and Contact Link (keep these as before) ---
    tags$li(
      class = "dropdown",
      tags$a(
        id = "contact-email",
        href = "mailto:subnationalpoliticsproject@gmail.com?subject=SPP%20contact&body=Hi%20SPP%20team%2C%0D%0A",
        target = "_blank",
        `aria-label` = "Email contact",
        icon("envelope"),
        span(class = "hidden-xs", " Contact")
      )
    ),
    tags$li(
      class = "dropdown app-header-logo tec-logo",
      tags$a(
        href = "https://egobiernoytp.tec.mx/es/escuela-de-ciencias-sociales-y-gobierno",
        target = "_blank",
        tags$img(
          src = "EscuelaCienciasSocialesyGobierno_Horizontal_Blanco.png",
          height = "40px",
          style = "vertical-align: middle;"
        )
      )
    )

    
  ),
  title = "SPP-Subnational Politics Project",   # <-- this sets <title>
  dashboardSidebar(
    width = 250,
    useShinyjs(),
    tagList(
      sidebarMenu(id = "tabs",
                  menuItem("About", tabName = "about", icon = icon("info-circle")),
                  menuItem("Mapping tool", tabName = "map_tab", icon = icon("map"),selected = TRUE),
                  menuItem("Graphing tool", tabName = "graph_tab", icon = icon("chart-line")),
                  menuItem("Camera Viz tool", tabName = "camera", icon = icon("landmark")),
                  menuItem("Codebook", tabName = "codebook", icon = icon("book-open")),
                  menuItem("Databases", tabName = "data_tab", icon = icon("table"))

      ),
      # tags$head(
      #   tags$style(HTML("
      #   .main-sidebar {
      #     background-image: url('background_gray.svg');
      #     background-size: cover;
      #     background-repeat: no-repeat;
      #     background-position: center;
      #   }
      # "))
      # ),
      # 
      # 
      # div(
      #   style = "display:none;",
      #   sidebarMenu(id = "tabs",
      #               menuItem("About", tabName = "about"),
      #               menuItem("Mapping tool", tabName = "map_tab",selected = TRUE),
      #               menuItem("Graphing tool", tabName = "graph_tab"),
      #               menuItem("Camera Viz tool", tabName = "camera"),
      #               menuItem("Codebook", tabName = "codebook"),
      #               menuItem("Databases", tabName = "data_tab")
      #   )
      # ),
      
      hidden(uiOutput("db_selector")),
      hidden(uiOutput("country_selector")),  # default: visible
      shinyjs::hidden(
        div(
          id = "fancytree_states_container",   # new container id (renamed for clarity)
          style = "padding: 15px;",
          
          tags$label("Select a state:", `for` = "fancytree_states_demo"),
          
          div(id = "fancytree_states_demo")   # tree mounts here
        )
      ),
      
      hidden(
        div( id= "fancytree_vars_demo_container",
             style = "padding: 15px;",
          tags$label("Select a variable:", `for` = "fancytree_vars_demo"),
          div(id = "fancytree_vars_demo")
        )
      ),
      
      hidden(
        div( id= "fancytree_vars_container_graph",
             style = "padding: 15px;",
             tags$label("Select a variable:", `for` = "fancytree_vars_demo_graph"),
             div(id = "fancytree_vars_demo_graph")
        )
      ),
      
      
      hidden(selectInput("country_sel2", "Select a country:", choices = c(unique(data$country_name)), selected = "ARGENTINA")),
      hidden(selectInput("state_sel2", "Select a state:", choices = NULL)),
      hidden(uiOutput("country_selector_camera")),
      hidden(uiOutput("state_selector_camera")),
      hidden(selectInput(
        "chamber_sel_camera", "Chamber",
        choices = c("Lower chamber" = 1, "Upper chamber" = 2),
        selected = 1
      ))
      )
  ),
  dashboardBody(
    #
    
    tags$head(includeHTML("ga.html"),
              tags$script(src="tab_analytics.js"),
              tags$script(src = "input_analytics.js")
    ),
    
    
    
    
    
    tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "spp_logo_tab_v2.svg")
    ),
    tags$head(
      # --- HOW TO TRIGGER---
      tags$script(HTML("
        $(document).on('click', '#btn_howto', function(e) {
          e.preventDefault();
          Shiny.setInputValue('btn_howto', Date.now(), {priority: 'event'});
        });
      ")),
      
      # SIDE BAR SWIPE
      tags$script(src = "sidebar_swipe.js")
      
      
    ),
    
    # --- FOOTER
    
    tags$footer(
      style = "
      position: fixed;
      bottom: 0;
      left: 0;
      padding: 5px 15px;
      font-size: 11px;
      text-align: left;
      width: max-content;
      background: linear-gradient(to right, var(--orange), rgba(18, 18, 18, 0));
      color: var(--gray);
      z-index: 1050;
    ",
      HTML("Tool developed by <strong><a href='https://www.linkedin.com/in/felipesotojorquera/' target='_blank' style='color:var(--magenta, #E5007D)'>Felipe Soto Jorquera.</a></strong>")
    ),
    
    tags$head(
      
      ## move the toggle
      tags$script(HTML("
        $(function () {
          var $logo   = $('.main-header .logo').first();
          var $toggle = $('.main-header .navbar .sidebar-toggle').first();
          if ($logo.length && $toggle.length) {
            // Move the existing toggle into the logo, before the title text
            $toggle.attr('id','sidebar-toggle-relocated'); // give it an id for styling
            $toggle.detach().prependTo($logo);
          }
        });
      ")),
      
      tags$script(HTML("
        $('#btn_howto').on('click', function(e) {
          e.preventDefault();
          Shiny.setInputValue('btn_howto', new Date().getTime());
        });
      ")),
      
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jstree/3.3.11/jstree.min.js"),
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/jquery.fancytree@2/dist/skin-lion/ui.fancytree.min.css"
      ),
      tags$script(
        src = "https://cdn.jsdelivr.net/npm/jquery.fancytree@2/dist/jquery.fancytree-all-deps.min.js"
      ),
      
      
      tags$script(src = "fancytree_init.js"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/jstree/3.3.11/themes/default/style.min.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$link(id = "theme-css", rel = "stylesheet", type = "text/css", href = "styles.css"),
      
      
    ),
    tags$script(HTML("
      function fixAnimateButtons() {
        $('.slider-animate-button').each(function() {
          const btn = $(this);
    
          // Prevent re-initializing the same button
          if (btn.hasClass('ab-fixed')) return;
    
          btn.addClass('ab-fixed');
    
          // Insert text span if missing
          if (!btn.find('.btn-text').length) {
            btn.append('<span class=\"btn-text\"> Play</span>');
          }
    
          // Keep text in sync with Shiny internal state
          const updateText = () => {
            const playing = btn.hasClass('playing');
            btn.find('.btn-text').text(playing ? ' Pause' : ' Play');
          };
    
          // Click handler (only once)
          btn.on('click.animatefix', function() {
            // Toggle ONLY the visual class.
            btn.toggleClass('playing');
            updateText();
          });
    
          // Update when Shiny updates the widget
          $(document).on('shiny:value', updateText);
    
          // Initial sync
          updateText();
        });
      }
    
      // Run on load and whenever the DOM changes
      const obs = new MutationObserver(fixAnimateButtons);
      obs.observe(document.body, { childList: true, subtree: true });
    
      // Also run once at startup
      $(document).on('shiny:connected', fixAnimateButtons);
    ")),
    tabItems(
      tabItem(
        tabName = "map_tab", # QUE PASA ACAAAAAA
        tagList(
          mapModuleUI("map1"),
          
          absolutePanel(
            top = 90, right = 12, 
            width = 300,
            draggable = F,
            div(class = "small-text-box",
                id = "var-desc-map",
                box(
                  solidHeader = F,
                  collapsible = F,
                  collapsed = FALSE,
                  width = NULL,
                  closable = TRUE,
                  uiOutput("var_description_map")
                )
            )),
          
          # Custom year selector placed at the bottom of the tab
          div(
            style = "position: absolute; bottom: 50%; left: 40%; z-index: 1000;",
            hidden(textOutput("no_data_message"))
          ),
          div(
            style = "position: absolute; bottom: 30px; left: 40%; right: 20%; z-index: 1000; overflow-y: hidden; overflow-x: hidden;",
            uiOutput("year_selector")
          ),

          
          
        )
      ),

      
      tabItem(
        tabName = "graph_tab",  # Este va directamente dentro de tabItem()
        fluidRow(
          column(9,
                 linePlotModuleUI("lp")
          ),
          column(3,
                 box(
                   #title = "Variable description", 
                   solidHeader = F,
                   collapsible = FALSE, 
                   collapsed = FALSE,
                   width = NULL,
                   closable = TRUE,
                   uiOutput("var_description_graph")
                 ),
                 box(
                   #title = "States:", 
                   #solidHeader = F,
                   width = NULL,
                   height = NULL,
                   linePlotLegendUI("lp")),
                 box(width = NULL,
                     height = NULL,
                     checkboxInput("force_y0", "Y-axis starts at 0", value = FALSE, ))
          ),
          
        )
      ),
      
      
      tabItem(tabName = "camera",

              tagList(
                fluidRow(
                  column(9,
                         camaraUI("cam"),
                         div(
                           style = "left: 40%; right: 40%; z-index: 1000; overflow-y: hidden; overflow-x: hidden;",
                           shinyWidgets::sliderTextInput(
                             inputId  = "year_sel_camera", label = "Year",
                             choices  = as.character(seq(1983, 2024, 1)),
                             grid     = TRUE, width = "90%",
                             selected = 2019,
                             animate  = shiny::animationOptions(
                               interval = 1000,
                               loop = FALSE
                             )
                           )
                         )
                  ),
                  column(3,
                         box(
                           #title = "Election Description", 
                           solidHeader = F,
                           collapsible = FALSE, 
                           collapsed = FALSE,
                           width = NULL,
                           uiOutput("text_camera")
                         ),
                         box(width = NULL,
                             height = NULL,
                             camaraLegendUI("cam")))

                ),
                
              )

      ),
      
      

      tabItem(tabName = "codebook", 
              uiOutput("pdf_visor")),
      
      
      tabItem(
        tabName = "data_tab",
        spp_mvp_ui("spp1")
      ),
      
      tabItem(tabName = "about", 
              sppAboutModuleUI("about", title = "ABOUT"))
      
      
    ),
    skin = "blue"
    
    
  )
)
