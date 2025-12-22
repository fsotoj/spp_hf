#options(shiny.autoreload = FALSE)

source("ui.R")
source("server.R")



shinyApp(ui, server)



#rsconnect::writeManifest(appDir = ".", appPrimaryDoc = NULL, contentCategory = NULL)

# SACAR TOTAL ELECTIONS
# Click on a title to open and download the corresponding dataset from the Harvard Dataverse.