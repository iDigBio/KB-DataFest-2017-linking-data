library(shiny)
library(ggplot2)

pheno_taxa <- read.csv("../data/queryResults_phenoscape_taxonomy.csv",
                      stringsAsFactors = FALSE)

active_taxa_list <- c()

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
           plotOutput("map")
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
    active_taxa_list <<- unique(c(active_taxa_list, input$selected_taxon))
    updateCheckboxGroupInput(session, "active_taxa", 
                             choices=active_taxa_list
    )
    print(active_taxa_list)
  })
  
  output$map <- renderPlot({
    worldmap 
  })
}


shinyApp(ui = ui, server = server)