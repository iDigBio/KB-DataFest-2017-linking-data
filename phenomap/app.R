library(shiny)
library(tidyr)
library(gtable)
library(grid)


library(rotl)
library(rgdal)
library(stringr)
library(rworldmap)
library(ggplot2)
library(mapproj)
library(ggmap)
library(dplyr)
library(reshape2)


# Data set of Phenoscape characters + iDigBio specimens joined by taxon. See 
# https://github.com/phenoscape/KB-DataFest-2017-linking-data/blob/master/Build_Specimen_List_by_Taxonomy.ipynb
# Full dataset available from: http://elk.acis.ufl.edu/pheno_specimen_with_chars.csv.gz
pheno_specimens <- read.csv("../data/pheno_specimen_with_chars_tiny.csv", stringsAsFactors = FALSE)
pheno_specimens[pheno_specimens=='?'] <- NA
pheno_specimens[pheno_specimens==''] <- NA

# Re-shape to long form to make filtering on characters possible, remove NAs since the wide
# matrix may have been sparse.
pheno_specimens_long <- gather(pheno_specimens, character, value,
                               char_sesamoid_bone_of_manus:char_pelvic_sucking_disc) %>%
                        filter(!is.na(value)) %>%
                        filter(value!='-60.0230556')
pheno_specimens_long$value[pheno_specimens_long$value=='1 and 0'] <- '0 and 1'


# Index of taxa-characters to use with UI controls
taxa_chars_index <- pheno_specimens_long %>%
                       distinct(vto_short, vto_label, character)

# Calculate the list of all taxonomy from unique entries in the index
distinct_taxa <- taxa_chars_index %>% 
                     distinct(vto_short, vto_label) %>%
                     arrange(vto_label)
pheno_taxa_vector <- setNames(distinct_taxa[["vto_short"]], distinct_taxa[["vto_label"]])

# Global var of vto_shorts for tracking what should be listed in the checkboxgroup, 
# requires elements to be labeled with vto_label for display
active_taxa_vector <- c()
# Hard-code xamples for the demo, also pick the first item in the list from full data set
#active_taxa_vector <- setNames(c("VTO:0062083","VTO:0043441"), c("Adontosternarchus sachsi", "Anabantidae"))

# Map data and setup
world <- map_data("world")
map_height = 600

# Heat map setup

##############
## import ott numbers
ott<-read.csv('../data/phenoscape_taxonomy_ottids.csv')
colnames(ott)[2]<-'vto_short'
ott$vto_short<-str_replace(ott$vto_short, '_',':')
ott$vto_short<-str_replace(ott$vto_short, ' ','')

char_names<-colnames(pheno_specimens[grep("^char",colnames(pheno_specimens))])
pheno_specimens %>%  group_by(vto_short,family,genus,specificepithet) %>% summarize(count=n()) %>% as.data.frame()-> pheno_specimens_sum
pheno_specimens %>% select(vto_short,char_names)  %>% unique() %>% as.data.frame() -> pheno_specimens_charvals
pheno_specimens_chars_merged<-merge(pheno_specimens_sum,pheno_specimens_charvals,by='vto_short') 
pheno_specimens_chars_ott<-merge(pheno_specimens_chars_merged,ott,by='vto_short')
pheno_specimens_chars_ott$genus_species<-paste(pheno_specimens_chars_ott$genus,pheno_specimens_chars_ott$specificepithet,sep=' ')
hm<-melt(pheno_specimens_chars_ott,id.vars=c('vto_short','family','genus','genus_species','ott_id','count','vto_label'),
         measure.vars = c(char_names)) %>% filter(value!='-60.0230556')
hm$value[hm$value=='1 and 0'] <- '0 and 1'

# Tree data setup

ott<-read.csv('../data/phenoscape_taxonomy_ottids.csv', stringsAsFactors=FALSE)
colnames(ott)[2]<-'vto_short'

ott$vto_short <-  stringr::str_replace(ott$vto_short, ' ', '' )
ott$vto_short <-  stringr::str_replace(ott$vto_short, '_', ':' )


pheno_specimens_chars_ott<-merge(pheno_specimens_chars_merged,ott)
pheno_specimens_sum$genus_species<-paste(pheno_specimens_sum$genus,pheno_specimens_sum$specificepithet,sep=' ')




ui <- fluidPage(
  
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
             )
           )
    ),
    column(9,
           h2("Map of Selected Character for Active Taxa"),
           div(style = paste0("height:", map_height, "px"), plotOutput("map"))
           )
  ),
  
  fluidRow(
    column(6,
           h2("Heatmap"),
           plotOutput("heatmap")
           ),
    column(6,
           h2("Phylogeny"),
           plotOutput("phylogeny")
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
    active_taxa_vector <<- c(active_taxa_vector, new_choice) # used to have unique() here to prevent multiple additions but that function eats names of elements
    updateCheckboxGroupInput(session, "active_taxa", 
                             choices=active_taxa_vector,
                             selected=active_taxa_vector
    )
    
    
    # Calculate available characters based on the active taxa
    # This method returns the union set of all characters of any active taxa
    chars_vector <- (taxa_chars_index %>%
                             filter(vto_short %in% active_taxa_vector) %>%
                             distinct(character))[["character"]]
    # This method returns the intersection set of characters that all active taxa have
#    chars_vector <- (taxa_chars_index %>%
#                       filter(vto_short %in% active_taxa_vector) %>%
#                       group_by(character) %>%
#                       count() %>%
#                       filter(n == length(active_taxa_vector)) %>%
#                       distinct(character))[["character"]]
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
    worldmap + geom_point(data=map_points, aes(x=lon, y=lat, color=value, shape=vto_label), size=5)
  }, height=map_height)
  
  
  output$heatmap <- renderPlot({
    print(input$selected_char)
    
    # Set aside the idea of pretty character names for now
    #x_charlabs<-str_replace_all(str_to_title(str_replace(char_names,'char_','')),'_',' ')
    
    chars<-ggplot(hm %>% filter(vto_short %in% input$active_taxa), aes(variable,genus,fill=value)) + geom_tile() + 
      scale_fill_discrete(na.value = 'white',labels=c('1'='present','0'='absent','0 and 1'='ambiguous'),name='Character State: ') +
      labs(y=NULL,x='Phenoscape Character') + theme(axis.title.y=element_blank(),axis.text.y=element_blank(), legend.position="top",
                                                    axis.text.x=element_text(angle=-90,hjust = 0)) #+ 
     # scale_x_discrete(labels=c(x_charlabs))
    
    specimens<-ggplot(hm %>% filter(vto_short %in% input$active_taxa), aes(' ',genus,fill=count)) + geom_tile() + 
      scale_fill_gradient(trans="log",low='white',high='blue',breaks=c(1,10000)) + 
      theme(legend.position="top",legend.title = element_blank(),axis.text.x=element_text(angle=-90,hjust = 0),
            axis.title.x = element_blank()) +
      scale_x_discrete(labels='Number of Museum Specimens')
    
    plot_grid(specimens,chars,rel_widths = c(1, 3),align='h')  
    
  })
  
  output$phylogeny <- renderPlot({
    print(input$selected_char)
    pheno_specimens_otol_subset <- pheno_specimens_chars_merged[which(ott$vto_short %in% pheno_specimens_chars_merged$vto_short),]
    ott_l <- ott[which(ott$vto_short %in% pheno_specimens_otol_subset$vto_short),]
    ott_l <- ott_l[,4]
    ol <- str_replace(ott_l, 'ott', '')
    
    
    ol <- as.numeric(ol)
    excl <- c(609231, 87893, 618300, 933436, 303038, 460871, 348162, 348162, 363181, 363181, 734459, 892956, 892956, 214115, 258647, 883411, 883411)
    
    for (item in excl){
      ol <- ol[ol != item]
    }
    ol<-c(na.omit(as.numeric(ol)))
    
    tr <- tol_induced_subtree(ott_ids=ol)
    plot(tr)
  })

}


shinyApp(ui = ui, server = server)