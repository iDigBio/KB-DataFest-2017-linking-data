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
# install.packages('dplyr')

library(rotl)
library(rgdal)
library(stringr)
library(rworldmap)
library(ggplot2)
library(mapproj)
library(ggmap)
library(dplyr)
library(reshape2)

##########

### set working directory
#wd <- "/home/blubb/Documents/R/"
wd <- "/Users/RDT/Documents/Extracurricular/Phenoscape/KB-DataFest-2017-linking-data/"
setwd(wd)


##############
## import data from idigbio
idig<-read.csv('../pheno_specimen_with_chars.csv')
idig[idig=='?'] <- NA
idig[idig==''] <- NA

# idig$genus_species<-as.factor(idig$genus_species)

##############
## import ott numbers
ott<-read.csv('data/phenoscape_taxonomy_ottids.csv')
colnames(ott)[2]<-'vto_short'


##############
## load in data from phenoscape
# char<-read.csv('data/fin-Ontotrace.txt',sep='\t')
# 
# char[,c(3:5)]<-as.character(char[,c(3:5)])
# 
# idig$pelvic.fin<-c(rep(NA,nrow(idig)))
# idig$pectoral.fin<-c(rep(NA,nrow(idig)))
# idig$pelvic.sucking.disc<-c(rep(NA,nrow(idig)))
# 
# 
# for(i in levels(idig$vto_short)){
#   # print(i)
#   if(i %in% char$Valid.Taxon){
#     idig$pelvic.fin[idig$vto_short==i]<-char$pelvic.fin[char$Valid.Taxon==i]
#     idig$pectoral.fin[idig$vto_short==i]<-char$pectoral.fin[char$Valid.Taxon==i]
#     idig$pelvic.sucking.disc[idig$vto_short==i]<-char$pelvic.sucking.disc[char$Valid.Taxon==i]
#   }
#   
# }


####################
## build a table with counts

char_names<-colnames(idig[grep("^char",colnames(idig))])
idig %>%  group_by(vto_short,family,genus,specificepithet) %>% summarize(count=n()) %>% as.data.frame()-> idig_sum
idig %>% select(vto_short,char_names)  %>% unique() %>% as.data.frame() -> idig_charvals
idig_chars_merged<-merge(idig_sum,idig_charvals,by='vto_short') 
idig_chars_ott<-merge(idig_chars_merged,ott)
idig_sum$genus_species<-paste(idig_sum$genus,idig_sum$specificepithet,sep=' ')

####################
## make a heatmap with sum table

head(idig_chars_merged)

idig_chars_merged %>% select(vto_short,genus_species,count,char_names) %>% melt()

melt(idig_chars_merged) %>% head()
ggplot(idig_chars_merged) 



####################
## get the Open Tree of Life tree
idig_sum$genus_species<-paste(idig_sum$genus,idig_sum$specificepithet,sep=' ')


taxa <- c(unique(idig_sum$genus_species))
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

pecs_merged<-merge(pecs,keys)
pecs_merged$tiplabel<-paste(pecs_merged$Valid.Taxon.label,paste('ott',pecs_merged$ott,sep=''),sep='_')

## make pseudotips on tree


##############
## plot coordinates from idig
# subset(names(idig),'char' %in%)
idig[grep("^char"),colnames(idig)]

world <- map_data("world")

worldmap <- ggplot() +  geom_path(data=world, aes(x=long, y=lat, group=group))+  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) + theme_bw()

idig_manus<-na.omit(subset(idig[,1:11]))

worldmap + geom_point(data=idig_manus, aes(x=lon,y=lat,color=genus,shape=char_sesamoid_bone_of_manus))




##############
## plot occurence and phylo on map


tree <- tr

tiplabels <- tr$tip.label
# sort(tiplabels)

coords <- idig


cord <- coords[tiplabels,]


phylo.to.map(tree, coords)



