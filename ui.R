tagList(
  useShinyjs(),
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'css/styles.css')
  ),
  shinyjs::hidden(
    div(id = "page_shield", img(id = "shield_loader", src = "img/loading-icon.gif"))
  ),

  # semanticPage(
  navbarPage(
    title = strong("MIDAS"),
    windowTitle = "Maritime Intelligence & Data Analytics System - Visualization Tools for Informed Decision Making",
    fluid = TRUE,
    footer = includeHTML("ui/footer.html"),
    id = "nav",

    source("ui/homeTab.R", local = TRUE)$value,
    source("ui/FLIC.R", local = TRUE)$value,
    source("ui/MarketPlace.R", local = TRUE)$value,
    source("ui/TradeFlow.R", local = TRUE)$value,
    source("ui/ProjectsDesk.R", local = TRUE)$value,
    source("ui/CalculatorsUI.R", local = TRUE)$value
    # source('ui/Admin.R', local = TRUE)$value
   
  )
)

# addDeps(
#   tags$body(shiny::navbarPage(ui)
#   ))

