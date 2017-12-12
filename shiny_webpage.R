library(shiny)

##########

### set working directory
wd <- "/Users/RDT/Documents/Extracurricular/Phenoscape"
setwd(wd)


##############
## import data from idigbio
idig<-na.omit(read.csv('pheno_specimen.csv'))
colnames(idig)[7]<-'long'

pecs<-read.csv('data/pectoralFin-ontotrace.csv',sep='\t')
colnames(pecs)<-c('pub','vto','vto_label','matrix_taxon','taxon_comment','specimens','state')
pecs$state<-as.character(pecs$state)

overlap<-matrix(nrow=)

idig$pecs<-c(rep(NA,nrow(idig)))

for(i in levels(idig$vto_label)){
  if(i %in% pecs$vto_label){
    idig$pecs[idig$vto_label==i]<-pecs$state[pecs$vto_label==i]
  }
  
}

idig_small<-na.omit(idig)


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
    worldmap + geom_point(data=subset(idig_small,genus %in% input$taxa),aes(x=long,y=lat,color=genus,shape=pecs))
  })
}

shinyApp(ui = ui, server = server)
