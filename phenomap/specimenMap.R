
specimenMapUI <- function(id, map_height=600){
  ns <- NS(id)
  
  tagList(
    h2("Map of Selected Character for Active Taxa"),
    div(style = paste0("height:", map_height), "px"), plotOutput(ns("map")))
  )
}


specimenMap <- function(input, output, session, map_height=600){
  renderPlot({
    map_points <- pheno_specimens_long %>%
      filter(vto_short %in% input$active_taxa) %>%
      filter(character %in% input$selected_char) %>%
      distinct(uuid, vto_label, lon, lat, value)
     
    worldmap <- ggplot() + 
      geom_path(data=world, aes(x=long, y=lat, group=group)) +
      scale_y_continuous(breaks=(-2:2) * 30) +
      scale_x_continuous(breaks=(-4:4) * 45) +
      theme_bw()
    worldmap + geom_point(data=map_points, aes(x=lon, y=lat, color=value, shape=vto_label), size=5)
  }, height=map_height)

}

# function that only draws the map
# document arguments, including how to make source data

# test code to run if sourced directly
# document in contributing
# have to worry about loading libraries if just sourcing this file, of course the
# R session is global so only need to load them once ever, perhaps just tell people
# to run the app once.