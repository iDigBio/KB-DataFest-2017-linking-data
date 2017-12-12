
##################
# Hackathon Phenoscape
# aim: map phenotypes onto a map
# Dec 12, 2017
# authors: 
# version: 0.001
####################
# install.packages("rotl")
# install.packages('rworldmap')
# install.packages('mapproj')
# install.packages('ggmap')

library(rotl)
library(rgdal)
library(stringr)
library(rworldmap)
library(ggplot2)
library(mapproj)
library(ggmap)

##########

### set working directory
#wd <- "/home/blubb/Documents/R/"
wd <- "/Users/RDT/Documents/Extracurricular/Phenoscape//KB-DataFest-2017-linking-data/"
setwd(wd)


##############
## import data from idigbio
idig<-na.omit(read.csv('pheno_specimen.csv'))
colnames(idig)[7]<-'long'

##############
## load in data from phenoscape
pecs<-read.csv('data/pectoralFin-ontotrace.csv',sep='\t')
merge(pecs,idig)

###################
## make a map of your points

# par(mfrow=c(1,1), mar=c(4,4,1,1))




####################
## get the Open Tree of Life tree
idig$genus_species<-paste(idig$genus,idig$specificepithet,sep=' ')


taxa <- c(unique(idig$genus_species))
resolved_names <- tnrs_match_names(taxa)
taxon_search <- tnrs_match_names(taxa, context_name="All life")

id <- ott_id(resolved_names)

typeof(id[[1]])

# tr <- tol_subtree(ott_id = id[[1]])

tr <- tol_induced_subtree(ott_ids=id)
plot(tr)


##############
## pull ott codes to add to idig dataset

codes<-matrix(nrow=length(id),ncol=2)
colnames(codes)<-c('genus_species','ott')
count=1
for(i in names(id)){
  codes[count,]<-c(as.character(i),id[i][[1]])
  count=count+1
}
keys<-as.data.frame(codes)
keys$genus_species<-str_to_lower(keys$genus_species)

idig_merged<-merge(idig,keys)
idig_merged$tiplabel<-paste(idig_merged$genus,idig_merged$specificepithet,paste('ott',idig_merged$ott,sep=''),sep='_')

## make unique row names
rownames(idig_merged)<-make.names(idig_merged$tiplabel,unique=TRUE)

## make pseudotips on tree


##############
## plot coordinates from idig


world <- map_data("world")

worldmap <- ggplot() +  geom_path(data=world, aes(x=long, y=lat, group=group))+  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) + theme_bw()

worldmap + geom_point(data=subset(idig_merged,genus=='scytalina'),aes(x=long,y=lat,color=genus))


##############
## plot occurence and phylo on map


tree <- tr

tiplabels <- tr$tip.label
# sort(tiplabels)

coords <- idig


cord <- coords[tiplabels,]


phylo.to.map(tree, coords)



