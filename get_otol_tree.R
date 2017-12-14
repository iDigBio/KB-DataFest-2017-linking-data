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
wd <- "/home/blubb/sync-TP-T470s/Phenoscape_Hackathon/"
setwd(wd)


##############
## import data from idigbio
idig<-read.csv('./pheno_specimen_with_chars.csv', stringsAsFactors=FALSE)
idig[idig=='?'] <- NA
idig[idig==''] <- NA

# idig$genus_species<-as.factor(idig$genus_species)

##############
## import ott numbers
ott<-read.csv('./KB-DataFest-2017-linking-data/data/phenoscape_taxonomy_ottids.csv', stringsAsFactors=FALSE)
colnames(ott)[2]<-'vto_short'

####################
## build a table with counts

char_names<-colnames(idig[grep("^char",colnames(idig))])
idig %>%  group_by(vto_short,family,genus,specificepithet) %>% summarize(count=n()) %>% as.data.frame()-> idig_sum
idig %>% select(vto_short,char_names)  %>% unique() %>% as.data.frame() -> idig_charvals

ott$vto_short <-  stringr::str_replace(ott$vto_short, ' ', '' )
ott$vto_short <-  stringr::str_replace(ott$vto_short, '_', ':' )


idig_chars_merged<-merge(idig_sum,idig_charvals,by='vto_short') 
idig_chars_ott<-merge(idig_chars_merged,ott)
idig_sum$genus_species<-paste(idig_sum$genus,idig_sum$specificepithet,sep=' ')


## get the Open Tree of Life tree

idig_otol_subset <- idig_chars_merged[which(ott$vto_short %in% idig_chars_merged$vto_short),]

ott_l <- ott[which(ott$vto_short %in% idig_otol_subset$vto_short),]


ott_l <- ott_l[,4]

# taxa <- c(unique(idig_sum$genus_species))
# resolved_names <- tnrs_match_names(taxa) 
# taxon_search <- tnrs_match_names(taxa, context_name="All life")
# 
# id <- ott_id(resolved_names)
# 
# typeof(id[[1]])
# 
# # tr <- tol_subtree(ott_id = id[[1]])

ol <- str_replace(ott_l, 'ott', '')


ol <- as.numeric(ol)
excl <- c(609231, 87893, 618300, 933436)

for (item in excl){
  ol <- ol[ol != item]
}


tr <- tol_induced_subtree(ott_ids=ol)
plot(tr)


