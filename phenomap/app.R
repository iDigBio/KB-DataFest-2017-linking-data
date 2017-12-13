library(shiny)
library(ggplot2)
library(dplyr)

## DEPRECATED
## List of all taxa in Phenoscape. Fix-up to make ordered for drop-downs
## and chop out a string VTO to use to pick subsets from iDigBio points.
#pheno_taxa <- read.csv("../data/queryResults_phenoscape_taxonomy_tiny.csv",
#                      stringsAsFactors = FALSE)
#pheno_taxa <- pheno_taxa %>%
#  mutate(vto_short = gsub("_", ":", unlist(strsplit(vto, '/'))[[5]])) %>%
#  arrange(vto_label)
## The above mutate returns the same value for every row

# Pheno characters + iDigBio specimens joined by taxon
pheno_specimens <- read.csv("../data/pheno_specimen_with_chars_tiny.csv", stringsAsFactors = FALSE)

# Calculate the taxonomy from unique entries in the actual data file
distinct_taxa <- pheno_specimens %>% distinct(vto_short, vto_label)
pheno_taxa_vector <- setNames(distinct_taxa[["vto_short"]], distinct_taxa[["vto_label"]])


# Global var for tracking what should be listed in the checkboxgroup
active_taxa_vector <- c()

##############
## plot data by coords
world <- map_data("world")


ui <- fluidPage(
  
  # App title ----
  titlePanel("Phenomap"),
  
  fluidRow(
    column(3, 
           h2("Select Taxa"),
           selectInput("selected_taxon", "", 
                       choices=pheno_taxa_vector),
           actionButton("add_taxon", "Add")
           ),
    column(3,
           h2("Active Taxa"),
           checkboxGroupInput("active_taxa", "", choices=active_taxa_vector)
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
    new_choice <- setNames(
      input$selected_taxon,
      (distinct_taxa %>% filter(vto_short == input$selected_taxon))[["vto_label"]]
    )
    #print(new_choice)
    active_taxa_vector <<- c(active_taxa_vector, new_choice) # used to have unique() here but that function eats names of elements
    #print(active_taxa_vector)
    updateCheckboxGroupInput(session, "active_taxa", 
                             choices=active_taxa_vector,
                             selected=active_taxa_vector
    )
  })
  
  output$map <- renderPlot({
    map_points <- pheno_specimens %>%
                    filter(vto_short %in% input$active_taxa)
    
    worldmap <- ggplot() + 
      geom_path(data=world, aes(x=long, y=lat, group=group)) +
      scale_y_continuous(breaks=(-2:2) * 30) +
      scale_x_continuous(breaks=(-4:4) * 45) +
      theme_bw()
    worldmap + geom_point(data=map_points, aes(x=lon, y=lat, color=vto_label))
  })
}


shinyApp(ui = ui, server = server)