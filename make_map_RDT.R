
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
wd <- "/Users/RDT/Documents/Extracurricular/Phenoscape"
setwd(wd)


##############
## import data from idigbio
idig<-na.omit(read.csv('pheno_specimen.csv'))
colnames(idig)[7]<-'long'

###################
## make a map of your points

# par(mfrow=c(1,1), mar=c(4,4,1,1))


####load shapefiles
# Natural Earth shape files -- global (Robinson) projections
# get shapefiles from http://www.naturalearthdata.com
shape_path <- "/Users/RDT/Documents/Extracurricular/Phenoscape/HYP_HR_SR_OB_DR"
coast_shapefile <- paste(shape_path, "ne_50m_coastline.shp", sep="")
ocean_shapefile <- paste(shape_path, "ne_50m_ocean.shp", sep="")
admin0_shapefile <- paste(shape_path, "ne_50m_admin_0_countries.shp", sep="")
admin1_shapefile <- paste(shape_path, "ne_50m_admin_1_states_provinces_lakes.shp", sep="")
lakes_shapefile <- paste(shape_path, "ne_50m_lakes.shp", sep="")
bb_shapefile <- paste(shape_path, "ne_50m_wgs84_bounding_box.shp", sep="")
grat30_shapefile <- paste(shape_path, "ne_50m_graticules_30.shp", sep="")


layer <- ogrListLayers(coast_shapefile)
coast_lines <- readOGR(coast_shapefile, layer=layer)

layer <- ogrListLayers(ocean_shapefile)
ocean_poly <- readOGR(ocean_shapefile, layer=layer)

layer <- ogrListLayers(admin0_shapefile)
admin0_poly <- readOGR(admin0_shapefile, layer=layer)

layer <- ogrListLayers(admin1_shapefile)
admin1_poly <- readOGR(admin1_shapefile, layer=layer)

layer <- ogrListLayers(lakes_shapefile)
lakes_poly <- readOGR(lakes_shapefile, layer=layer)

layer <- ogrListLayers(grat30_shapefile)
grat30_lines <- readOGR(grat30_shapefile, layer=layer)

layer <- ogrListLayers(bb_shapefile)
bb_poly <- readOGR(bb_shapefile, layer=layer)

bb_lines <- as(bb_poly, "SpatialLines")


# plot everything
plot(coast_lines, col="black")
plot(admin0_poly, bor="gray", add=TRUE)
plot(lakes_poly, bor="lightblue", add=TRUE)
plot(coast_lines, col="black", add=TRUE)

with(occ_data, points(longitude, latitude, pch=".", col="red"))




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



