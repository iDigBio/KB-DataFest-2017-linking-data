library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Phenomap"),
  
  fluidRow(
    column(3, 
           h2("Select Taxa")
           ),
    column(3,
           h2("Active Taxa")
           ),
    column(6,
           h2("Map")
           )
  ),
  
  fluidRow(
    column(6,
           h2("Heatmap")
           ),
    column(6,
           h2("Phylogeny")
           )
  )
)

server <- function(input, output) {
}


shinyApp(ui = ui, server = server)