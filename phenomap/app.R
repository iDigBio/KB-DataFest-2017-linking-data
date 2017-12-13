library(shiny)
library(ggplot2)

pheno_taxa = read.csv("../data/queryResults_phenoscape_taxonomy.csv",
                      stringsAsFactors = FALSE)

##############
## plot data by coords
world <- map_data("world")
worldmap <- ggplot() + 
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) +
  theme_bw()
#worldmap + geom_point(data=idig_merged,aes(x=long,y=lat))

ui <- fluidPage(
  
  # App title ----
  titlePanel("Phenomap"),
  
  fluidRow(
    column(3, 
           h2("Select Taxa"),
           selectInput("selected_taxon", "", choices=pheno_taxa$vto_label),
           actionButton("add_taxon", "Add")
           ),
    column(3,
           h2("Active Taxa"),
           checkboxGroupInput("active_taxa", "", choices=list())
           ),
    column(6,
           h2("Map"),
           plotOutput(outputId = "map")
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

server <- function(input, output, session) {
  observeEvent(input$add_taxon, {
    tc <- input$active_taxa
    updateCheckboxGroupInput(session, "active_taxa", 
                             choices=c(input$active_taxa, input$selected_taxon)
    )
    print(paste(input$active_taxa))
  })
  
  output$map <- renderPlot({
    worldmap 
  })
}


shinyApp(ui = ui, server = server)