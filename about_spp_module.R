
sppAboutModuleUI <- function(id,
                             title = "Subnational Politics Project",
                             max_width = 900,
                             open_sections = c("about")) {
  ns <- NS(id)
  root_id <- ns("root")
  
  # helper: TRUE if section should start open
  open_attr <- function(key, open_sections) if (key %in% open_sections) "open" else NULL
  
  style_css <- paste0(
    "/* Palette via CSS variables from styles.css (fallbacks included) */\n",
    "#", root_id, " { background:#fff; color:var(--gray, #4D4D4D); font-family: Helvetica, Arial, sans-serif; padding:24px 16px 24px; }\n",
    "#", root_id, " .spp-container { max-width:", max_width, "px; margin:0 auto; }\n",
    "#", root_id, " .kicker { color:var(--orange, #FFA92A); text-transform:uppercase; letter-spacing:0.12em; font-size:24px; margin:0 0 12px 0; }\n",
    
    "/* Collapsible card built on <details> */\n",
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
    "#", root_id, " .ref { color:var(--gray, #4D4D4D); font-size:12px; }\n",
    "#", root_id, " .spp-link { color:var(--magenta, #E5007D); text-decoration:none; }\n",
    "#", root_id, " .spp-link:hover { text-decoration:underline; }\n",
    
    "/* People grid & cards */\n",
    "#", root_id, " .team-grid { display:grid; grid-template-columns:repeat(auto-fit, minmax(220px, 1fr)); gap:20px; text-align:center; }\n",
    "#", root_id, " .team-card { background:#fff; border:1px solid #e6e6e6; border-radius:12px; padding:16px; position:relative; overflow:visible; transition: transform .12s ease, box-shadow .12s ease, border-color .12s ease; }\n",
    "#", root_id, " .team-card:hover { transform: translateY(-4px); box-shadow: 0 10px 22px rgba(0,0,0,0.18); border-color: rgba(255,169,42,0.45); }\n",
    "#", root_id, " .team-avatar { width:120px; height:120px; object-fit:cover; border-radius:50%; margin-bottom:12px; border:2px solid rgba(255,169,42,0.6); filter:grayscale(100%); transition:filter .3s ease; }\n",
    "#", root_id, " .team-avatar:hover { filter:grayscale(0%); }\n",
    "#", root_id, " .collab-name { color:var(--gray, #4D4D4D); font-weight:700; margin:0 0 6px 0; }\n",
    "#", root_id, " .badge { display:inline-block; padding:2px 8px; border-radius:999px; font-size:11px; font-weight:700; vertical-align:middle; margin-right:6px; }\n",
    "#", root_id, " .badge-pi { background:var(--orange, #FFA92A); color:#111; }\n",
    "#", root_id, " .badge-collab { background:rgba(114,36,100,0.12); color:var(--purple, #722464); border:1px solid rgba(114,36,100,0.35); }\n",
    "#", root_id, " .badge-r_assist { background:rgba(33,150,243,0.12); color:#1565C0; border:1px solid rgba(33,150,243,0.35); }\n",
    "#", root_id, " .lnk-btn { display:inline-block; width:20px; height:20px; line-height:20px; text-align:center; border-radius:50%; background:#0A66C2; color:#fff; text-decoration:none; font-weight:700; font-family:Arial, Helvetica, sans-serif; font-size:11px; transition:filter .15s ease; vertical-align:middle; }\n",
    "#", root_id, " .lnk-btn:hover { filter:brightness(1.15); }\n",
    
    "/* Figures */\n",
    "#", root_id, " .figure { margin:10px 0 12px 0; text-align:center; }\n",
    "#", root_id, " .figure img { max-width:100%; height:auto; border-radius:12px; border:1px solid #eee; }\n",
    "#", root_id, " .caption { font-size:0.92rem; color:var(--gray, #4D4D4D); margin-top:6px; }\n",
    "#", root_id, " ol.vars { margin:10px 0 0 18px; }\n",
    "#", root_id, " ol.vars li { margin:6px 0; }\n",
    
    "/* Avatar tooltip (scoped) */\n",
    "#", root_id, " .avatar-link { position:relative; display:inline-block; }\n",
    "#", root_id, " .tooltip-text { visibility:hidden; min-width: 110px; background-color:var(--orange, #FFA92A); color:#111; text-align:center; border-radius:6px; padding:4px 8px; position:absolute; z-index:9999; bottom: calc(100% + 8px); left:50%; transform: translateX(-50%); opacity:0; transition:opacity 0.25s ease; font-size:12px; box-shadow:0 4px 8px rgba(0,0,0,0.15); }\n",
    "#", root_id, " .tooltip-text::after { content:''; position:absolute; top:100%; left:50%; transform:translateX(-50%); border-width:6px; border-style:solid; border-color:var(--orange, #FFA92A) transparent transparent transparent; }\n",
    "#", root_id, " .avatar-link:hover .tooltip-text { visibility:visible; opacity:1; }\n"
  )
  
  tagList(
    singleton(tags$style(HTML(style_css))),
    tags$div(
      id = root_id,
      tags$div(
        class = "spp-container",
        if (!is.null(title) && nzchar(title)) tags$p(class = "kicker", title),
        
        # 1) About SPP -----------------------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("about", open_sections),   # <- sticks reliably
          tags$summary(
            tags$span(class = "card-title", "About SPP"),
            icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$p(
              class = "spp-text",
              tags$strong("The Subnational Politics Project (SPP)"),
              "is a collaborative initiative dedicated to compiling, generating, and disseminating systematic, transparent, and publicly accessible data on subnational political institutions, processes, and electoral outcomes across Latin America."
            ),
            tags$p(
              class = "spp-text",
              "The SPP’s central goal is to build a comprehensive and standardized data infrastructure that enables both detailed within-country analysis and robust cross-national comparisons of subnational political dynamics."
            ),
            tags$p(
              class = "spp-text",
              "By providing consistent, high-quality, and spatially disaggregated longitudinal data, the SPP seeks to advance scholarly and policy-oriented research on the political foundations and consequences of territorial inequality in Latin America."
            ),
            tags$p(
              class = "spp-text",
              "This data infrastructure will support empirical work on a wide range of topics, including federalism, decentralization, subnational democracy and authoritarianism, party competition, electoral accountability, territorial governance, among others."
            )
          )
        ),
        
        # 3) People Behind SPP ---------------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("people", open_sections),   # <- sticks reliably
          tags$summary(
            tags$span(class = "card-title", "People Behind SPP"),
            icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$div(
              class = "team-grid",
              
              # Agustina
              tags$div(
                class = "team-card",
                tags$a(
                  href   = "https://agustinagiraudy.com",
                  target = "_blank",
                  class  = "avatar-link",
                  tags$img(src = "agustina.jpg", alt = "Agustina Giraudy", class = "team-avatar"),
                  tags$span(class = "tooltip-text", "Go to my webpage")
                ),
                tags$p(class = "collab-name", "Agustina Giraudy"),
                tags$div(
                  tags$span(class = "badge badge-pi", "Principal Investigator"),
                  tags$a(href = "https://www.linkedin.com/in/agustina-giraudy-72a3b81a9/", target = "_blank", class = "lnk-btn", "in")
                ),
                tags$p(class = "pi-affil", "American University / Tecnológico de Monterrey")
              ),
              
              # Francisco
              tags$div(
                class = "team-card",
                tags$a(
                  href   = "https://www.furdinez.com/",
                  target = "_blank",
                  class  = "avatar-link",
                  tags$img(src = "francisco.jpg", alt = "Francisco Urdinez", class = "team-avatar"),
                  tags$span(class = "tooltip-text", "Go to my webpage")
                ),
                tags$p(class = "collab-name", "Francisco Urdinez"),
                tags$div(
                  tags$span(class = "badge badge-collab", "Collaborator"),
                  tags$a(href = "https://www.linkedin.com/in/francisco-urdinez-a8061813/", target = "_blank", class = "lnk-btn", "in")
                ),
                tags$p(class = "pi-affil", "Universidad Católica de Chile")
              ),
              
              
              # Guadalupe
              tags$div(
                class = "team-card",
                tags$a(
                  href   = "https://guadagonzalez.com/",
                  target = "_blank",
                  class  = "avatar-link",
                  tags$img(src = "guadalupe.jpg", alt = "Guadalupe González", class = "team-avatar"),
                  tags$span(class = "tooltip-text", "Go to my webpage")
                ),
                tags$p(class = "collab-name", "Guadalupe González"),
                tags$div(
                  tags$span(class = "badge badge-collab", "Collaborator"),
                  tags$a(href = "https://www.linkedin.com/in/guadag12/", target = "_blank", class = "lnk-btn", "in")
                ),
                tags$p(class = "pi-affil", "University of Maryland, College Park")
              ),
              
              # Felipe (tooltip but no external link)
              tags$div(
                class = "team-card",
                tags$span(
                  class  = "avatar-link",
                  tags$img(src = "felipe.jpg", alt = "Felipe Soto Jorquera", class = "team-avatar"),
                  tags$span(class = "tooltip-text", "Go to my webpage")
                ),
                tags$p(class = "collab-name", "Felipe Soto Jorquera"),
                tags$div(
                  tags$span(class = "badge badge-collab", "Collaborator"),
                  tags$a(href = "https://www.linkedin.com/in/felipesotojorquera/", target = "_blank", class = "lnk-btn", "in")
                ),
                tags$p(class = "pi-affil", "Hertie School, Berlin")
              ),
              
              # Sergio
              tags$div(
                class = "team-card",
                tags$a(
                  href   = "https://serhuertas.github.io/",
                  target = "_blank",
                  class  = "avatar-link",
                  tags$img(src = "sergio.jpg", alt = "Sergio Huertas Hernández", class = "team-avatar"),
                  tags$span(class = "tooltip-text", "Go to my webpage")
                ),
                tags$p(class = "collab-name", "Sergio Huertas Hernández"),
                tags$div(
                  tags$span(class = "badge badge-r_assist", "Research Assistant"),
                  tags$a(href = "https://www.linkedin.com/in/sergio-huertas-hern%C3%A1ndez/", target = "_blank", class = "lnk-btn", "in")
                ),
                tags$p(class = "pi-affil", "Universidad Católica de Chile")
              ),
              
              
            )
          )
        ),
        
        # 4) About Databases -----------------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("databases", open_sections),   # <- sticks reliably
          tags$summary(
            tags$span(class = "card-title", "About SPP Databases"),
            icon("chevron-down", class = "chev")
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
              tags$img(src = "databases_spp.svg", alt = "Figure 1. SPP Databases")
              # ,
              # tags$div(class = "caption", "Figure 1. Databases that comprise the Subnational Politics Project (SPP).")
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
              tags$img(src = "variables_database.svg", alt = "Figure 2. Variable Types in SPP")
              # ,
              # tags$div(class = "caption", "Figure 2. Variable types in the SPP databases.")
            )
            
          )
        ),
        
        # 2) References ----------------------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("refs", open_sections),
          tags$summary(
            tags$span(class = "card-title", "References"),
            icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            
            # Codebook citation first
            tags$p(class="ref", tags$strong("Suggested Citation for Codebook")),
            tags$p(
              class="ref",
              "Giraudy, Agustina; Gonzalez, Guadalupe Andrea; Urdinez, Francisco, 2025, ",
              "“Codebook: Subnational Politics Project (SPP) (v. 1)”, ",
              tags$a(
                href="https://doi.org/10.17605/OSF.IO/H96FD",
                target="_blank",
                class="spp-link",
                "https://doi.org/10.17605/OSF.IO/H96FD"
              )
            ),
            
            tags$hr(class="spp-hr"),
            
            # Table of database citations
            tags$table(
              style = "width:100%; border-collapse:collapse; font-size:12px;",
              tags$thead(
                tags$tr(
                  tags$th("Database Name", style="text-align:left; border-bottom:2px solid #ddd; padding:6px;"),
                  tags$th("Abbreviation", style="text-align:left; border-bottom:2px solid #ddd; padding:6px;"),
                  tags$th("Suggested Citation", style="text-align:left; border-bottom:2px solid #ddd; padding:6px;")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Subnational Executive Database", style="padding:6px;"),
                  tags$td("SED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hernández, and Francisco Urdinez. 2025. “Subnational Executive Database (SED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/1D3P3J", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/1D3P3J"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Subnational Executive Elections Database", style="padding:6px;"),
                  tags$td("SEED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hernández, and Francisco Urdinez. 2025. “Subnational Executive Elections Database (SEED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/UPOWMW", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/UPOWMW"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Subnational Legislative Elections Database", style="padding:6px;"),
                  tags$td("SLED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. “Subnational Legislative Elections Database (SLED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/084FXF", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/084FXF"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Subnational Democracy Indices", style="padding:6px;"),
                  tags$td("SDI", style="padding:6px;"),
                  tags$td("Giraudy, Agustina. 2025. “Subnational Democracy Indices (SDI) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/7TNLBW", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/7TNLBW"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Capital Federal & Tierra del Fuego Legislatures Database", style="padding:6px;"),
                  tags$td("CFTDFLD", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. “Capital Federal & Tierra Del Fuego Legislatures Database (CFTDFLD) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/AJJLHX", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/AJJLHX"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("National Executive Database", style="padding:6px;"),
                  tags$td("NED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hernández, and Francisco Urdinez. 2025. “National Executive Database (NED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/HNKQUH", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/HNKQUH"), style="padding:6px;")
                )
              )
            )
          )
        )
        
      )
    )
  )
}

sppAboutModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
