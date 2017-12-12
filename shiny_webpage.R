library(shiny)

##########

### set working directory
#wd <- "/home/blubb/Documents/R/"
wd <- "/Users/RDT/Documents/Extracurricular/Phenoscape"
setwd(wd)


##############
## import data from idigbio
idig<-na.omit(read.csv('pheno_specimen.csv'))
colnames(idig)[7]<-'long'


##############
## plot data by coords
world <- map_data("world")
worldmap <- ggplot() +  geom_path(data=world, aes(x=long, y=lat, group=group))+  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) + theme_bw()
worldmap + geom_point(data=idig_merged,aes(x=long,y=lat))


ui<-fluidPage(
  checkboxGroupInput(inputId = "taxa",
              label = "Choose a taxon",
              choices=levels(idig$genus),
              inline = TRUE),
  # selectInput(inputId = "char",
  #                    label = "Choose a character",
  #             choices = #character names
  # )
  plotOutput(outputId = "map")
)

server <- function(input, output) {
  output$map <- renderPlot({
    worldmap + geom_point(data=subset(idig,genus %in% input$taxa),aes(x=long,y=lat,color=genus))
  })
}

shinyApp(ui = ui, server = server)
