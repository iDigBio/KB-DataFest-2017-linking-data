library(shiny)
library(tidyr)

# Pheno characters + iDigBio specimens joined by taxon
pheno_specimens <- read.csv("../data/pheno_specimen_with_chars_tiny.csv", stringsAsFactors = FALSE)

# Calculate the taxonomy from unique entries in the actual data file
distinct_taxa <- pheno_specimens %>% distinct(vto_short, vto_label)
pheno_taxa_vector <- setNames(distinct_taxa[["vto_short"]], distinct_taxa[["vto_label"]])

# Re-shape to long form to make filtering on characters possible
pheno_specimens_long <- gather(pheno_specimens, character, value, char_sesamoid_bone_of_manus:char_pelvic_sucking_disc)

# Global var of vto_shorts for tracking what should be listed in the checkboxgroup, 
# requires elements to be labeled with vto_label for display
active_taxa_vector <- c()

# Global var of character column names
active_chars_vector <- c()

##############
## plot data by coords
world <- map_data("world")


ui <- fluidPage(
  
  # App title ----
  titlePanel("Phenomap"),
  
  fluidRow(
    column(6, 
           fluidRow(
             column(6,
                    h2("Select Taxa"),
                    selectInput("selected_taxon", "", choices=pheno_taxa_vector),
                    actionButton("add_taxon", "Add")
             ),
             column(6,
                    h2("Active Taxa"),
                    checkboxGroupInput("active_taxa", "", choices=active_taxa_vector),
                    actionButton("clear_taxa", "Clear")
             )
           ),
           fluidRow(
             column(6,
                    h2("Select Character"),
                    selectInput("selected_char", "", choices=active_chars_vector)
                    # No longer allowin selection of multiple characters
                    #actionButton("add_character", "Add")
             ),
             column(6
                    # No longer allowin selection of multiple characters
                    #h2("Active Characters"),
                    #checkboxGroupInput("active_chars", "", choices=active_chars_vector)
             )
           )
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
  
  observeEvent(input$clear_taxa, {
    active_taxa_vector <<- c()
    print(active_taxa_vector)
    updateCheckboxGroupInput(session, "active_taxa", 
                             choices=active_taxa_vector,
                             selected=active_taxa_vector
    )
  })
  
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
    
    
    # Calculate available characters based on the active taxa
    active_chars_vector <<- (pheno_specimens_long %>%
                             filter(vto_short %in% active_taxa_vector) %>%
                             distinct(character))
    print(input$active_taxa)
    print(active_chars_vector)
    #[["character"]]
    updateSelectInput(session, "selected_char", choices=active_chars_vector)
    
  })

# No longer allowing multiple characters  
#  observeEvent(input$add_character, {
#    active_chars_vector <<- c(active_chars_vector, input$selected_char)
#    updateCheckboxGroupInput(session, "active_chars", 
#                             choices=active_chars_vector,
#                             selected=active_chars_vector
#    )
#  })
  
  output$map <- renderPlot({
    map_points <- pheno_specimens_long %>%
                    filter(vto_short %in% input$active_taxa) %>%
                    filter(character %in% input$selected_char) %>%
                    distinct(uuid, vto_label, lon, lat, value)
    
    worldmap <- ggplot() + 
      geom_path(data=world, aes(x=long, y=lat, group=group)) +
      scale_y_continuous(breaks=(-2:2) * 30) +
      scale_x_continuous(breaks=(-4:4) * 45) +
      theme_bw()
    worldmap + geom_point(data=map_points, aes(x=lon, y=lat, color=vto_label, shape=value))
  })
}


shinyApp(ui = ui, server = server)