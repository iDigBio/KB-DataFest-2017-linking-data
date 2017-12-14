library(shiny)
library(tidyr)

# Pheno characters + iDigBio specimens joined by taxon
pheno_specimens <- read.csv("../data/pheno_specimen_with_chars_tiny.csv", stringsAsFactors = FALSE)

# Re-shape to long form to make filtering on characters possible, remove NAs since the wide
# matrix may have been sparse.
pheno_specimens_long <- gather(pheno_specimens, character, value, 
                               char_sesamoid_bone_of_manus:char_pelvic_sucking_disc) %>%
                        filter(!is.na(value))

# Index of taxa-characters to use with UI controls
taxa_chars_index <- pheno_specimens_long %>%
                       distinct(vto_short, vto_label, character)

# Calculate the taxonomy list from unique entries in the index
distinct_taxa <- taxa_chars_index %>% distinct(vto_short, vto_label)
pheno_taxa_vector <- setNames(distinct_taxa[["vto_short"]], distinct_taxa[["vto_label"]])

# Global var of vto_shorts for tracking what should be listed in the checkboxgroup, 
# requires elements to be labeled with vto_label for display
active_taxa_vector <- c()



##############
## plot data by coords
world <- map_data("world")

map_height = 600

ui <- fluidPage(
  
  # App title ----
  titlePanel("Phenomap"),
  
  fluidRow(
    column(3, 
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
             column(12,
                    h2("Select Character"),
                    selectInput("selected_char", "", choices=c())
                    # No longer allowin selection of multiple characters
                    #actionButton("add_character", "Add")
             )
           )
    ),
    column(9,
           h2("Map"),
           div(style = paste0("height:", map_height, "px"), plotOutput("map"))
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
                             choices=character(0)
    )
  })
  
  observeEvent(input$add_taxon, {
    new_choice <- setNames(
      input$selected_taxon,
      (distinct_taxa %>% filter(vto_short == input$selected_taxon))[["vto_label"]]
    )
    active_taxa_vector <<- c(active_taxa_vector, new_choice) # used to have unique() here but that function eats names of elements
    updateCheckboxGroupInput(session, "active_taxa", 
                             choices=active_taxa_vector,
                             selected=active_taxa_vector
    )
    
    
    # Calculate available characters based on the active taxa
#    # This method returns the union set of all characters of any active taxa
#    chars_vector <- (taxa_chars_index %>%
#                             filter(vto_short %in% active_taxa_vector) %>%
#                             distinct(character))[["character"]]
    # This method returns the intersection set of characters that all active taxa have
    chars_vector <- (taxa_chars_index %>%
                       filter(vto_short %in% active_taxa_vector) %>%
                       group_by(character) %>%
                       count() %>%
                       filter(n == length(active_taxa_vector)) %>%
                       distinct(character))[["character"]]
    updateSelectInput(session, "selected_char", choices=chars_vector)
    
  })

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
    worldmap + geom_point(data=map_points, aes(x=lon, y=lat, color=vto_label, shape=value), size=5)
  }, height=map_height)
}


shinyApp(ui = ui, server = server)