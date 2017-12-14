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
idig<-read.csv('./pheno_specimen_with_chars.csv')
idig[idig=='?'] <- NA
idig[idig==''] <- NA

# idig$genus_species<-as.factor(idig$genus_species)

##############
## import ott numbers
ott<-read.csv('./KB-DataFest-2017-linking-data/data/phenoscape_taxonomy_ottids.csv', stringsAsFactors=FALSE)
colnames(ott)[2]<-'vto_short'


str(ott$ott_id)

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

head(idig_chars_merged)
head(ott)

ott$vto_short <-  stringr::str_replace(ott$vto_short, ' ', '' )

ott$vto_short <-  stringr::str_replace(ott$vto_short, '_', ':' )


idig_chars_merged<-merge(idig_sum,idig_charvals,by='vto_short') 
idig_chars_ott<-merge(idig_chars_merged,ott)
idig_sum$genus_species<-paste(idig_sum$genus,idig_sum$specificepithet,sep=' ')


## get the Open Tree of Life tree

head(ott)
head(idig_chars_merged)


idig_otol_subset <- idig_chars_merged[which(ott$vto_short %in% idig_chars_merged$vto_short),]
head(idig_otol_subset)


ott_l <- ott[which(ott$vto_short %in% idig_otol_subset$vto_short),]

head(ott_l)

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
ol <- as.list(ol)
typeof(ol[[1]])


excl <- c(609231, 87893, 618300, 933436)

for (item in excl){
  ol <- ol[ol != item]
}


tr <- tol_induced_subtree(ott_ids=ol)
plot(tr)

########

tplabel <- tr$tip.label
tl <- strsplit(tplabel, split = '_')


tll <- list()
for (item in tl){
  tll <- c(tll, as.character(item[length(item)]))
}



head(idig_chars_ott$ott_id)
head(tll)


typeof(tll[[1]])
typeof(tll[[1]])
typeof(idig_chars_ott$ott_id[[1]])

# 
# idig_chars_ott$ott_id <- as.list(idig_chars_ott$ott_id)
# tll <- as.list(tll)

for (item in idig_chars_ott$ott_id ){
  print(item)

}

idig_chars_ott$ott_id  <- str_replace(idig_chars_ott$ott_id , ' ', '')



char_sub <- idig_chars_ott[which(idig_chars_ott$ott_id %in% tll), ]

char_unique<- unique(char_sub[, c(1,7,8,9,10,11,12,13,14,15, 16, 17, 18, 19, 20, 23 )])

tl_str <- unlist(tll)
#tl_str <- as.list(tll)

head(char_unique )
typeof(char_unique$ott_id)
typeof(tll[[1]])
tr$tip.label <- tll

##################
library(picante)
trait.plot(tr, char_unique[14], cols = c("pink", "red", "blue"))

unique( char_unique$ott_id)

char_unique2 <- char_unique[which(unique(char_unique$ott_id) %in% tll), ]
rownames(char_unique2) <- tll


typeof(char_unique2[94,15])

typeof(char_unique2[14])

# 
# #Create a factor for the example
# levels(char_unique2)<-c(levels(char_unique2),"None")  #Add the extra level to your factor
# char_unique2[is.na(char_unique2)] <- "None"           #Change NA to "None"
# 
# is.na(char_unique2) <- 3
# 
# for(item in char_unique2){
#   print(item)
# 
# if (is.na(char_unique2[item])){
#   char_unique2[item] <- 5
# }
# }
# 
# 
# char_unique2[is.na(char_unique2)] <- "3"
# 
# 


is.na(char_unique2)
char_unique3 <- as.matrix(char_unique2)                     # conver to matrix
y <- which(is.na(char_unique2)==TRUE)         # get index of NA values
char_unique3[y] <- 7

typeof(rownames(char_unique3))
typeof(tr$tip.label)

tr_char <- as.character(tr$tip.label)
rownames(char_unique3) <- tr_char

char_unique4 <- as.data.frame(char_unique3)
char_unique3[14]

char14 <- list(char_unique4[14])
##problem is that names(df) is null




char_unique5 <- as.data.frame(lapply(char_unique4, unlist))
trait_df <- char_unique5
trait_df <- as.data.frame.numeric(trait_df)

# 
# orderVector1
# 
# slibrary(devtools)
# install_github("hadley/dplyr")
# install.packages("ggtree")
# 
# source("https://bioconductor.org/biocLite.R")
# # biocLite("BiocUpgrade") # you may need this
# biocLite("ggtree")
# library(ggtree)
plot_tree = ggplot(tr, layout = "fan", right = TRUE, size = 0.1)
plot_tree %<+% trait_df$trait_df[,14:15] + geom_tippoint(aes(color = trait1, alpha = 0.5))

order((trait_df$trait_df))

########################################################


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



