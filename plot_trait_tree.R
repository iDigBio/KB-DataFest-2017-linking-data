

tplabel <- tr$tip.label
tl <- strsplit(tplabel, split = '_')


tll <- c()
for (item in tl){
  tll <- c(tll, as.character(item[length(item)]))
}


idig_chars_ott$ott_id  <- str_replace(idig_chars_ott$ott_id , ' ', '')



char_sub <- idig_chars_ott[which(idig_chars_ott$ott_id %in% tll), ]


char_unique<- unique(char_sub[, c(1,7,8,9,10,11,12,13,14,15, 16, 17, 18, 19, 20, 22 )])
rownames(char_unique) <- char_unique$ott_id


char_unique2 <- subset(char_unique, !duplicated(ott_id))

rownames(char_unique2) <- char_unique2$ott_id

sort_char <- char_unique2[tll, ]







tl_str <- unlist(tll)
#tl_str <- as.list(tll)


tr$tip.label <- tll

#



rownames(char_unique2) <- tll

#install.packages("Biostrings")

library("colorspace")
library("Biostrings")
library("ape")
library("ggplot2")
library("ggtree")


###########################33

p8 <- ggtree(tr) + 
  xlim(0, 125) +
  geom_tiplab(size=2, offset=17) 

# pecfin<-as.data.frame(as.integer(sc2$char_pectoral_fin))
# rownames(pecfin)<-rownames(sc2)




# add heatmap
p9 <-  gheatmap(p8, sort_char[colnames(sort_char)[grep('^char', colnames(sort_char))]] , offset=0.2, width=0.2, low="white", high="black", colnames_position = "top", font.size=2)

# plot
plot(p9)