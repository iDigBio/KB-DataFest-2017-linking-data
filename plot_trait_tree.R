####I loaded the csv with strings are no factors.

tplabel <- tr$tip.label
tl <- strsplit(tplabel, split = '_')


tll <- c()
for (item in tl){
  tll <- c(tll, as.character(item[length(item)]))
}
tr$tip.label <- tll

#

idig_chars_ott$ott_id  <- str_replace(idig_chars_ott$ott_id , ' ', '')

char_sub <- idig_chars_ott[which(idig_chars_ott$ott_id %in% tll), ]
#char_unique<- unique(char_sub[, c(1,7,8,9,10,11,12,13,14,15, 16, 17, 18, 19, 20, 22 )])

char_unique <- unique(char_sub[, c(grep('^char', colnames(sort_char)), length(char_sub))])
char_unique2 <- subset(char_unique, !duplicated(ott_id))

#[grep('^char', colnames(sort_char))]

sort_char[colnames(sort_char)[grep('^char', colnames(sort_char))]]

#char_unique2 <- subset(char_unique, !duplicated(ott_id))
rownames(char_unique2) <- char_unique2$ott_id
sort_char <- char_unique2[tll, ]
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
  geom_tiplab(size=2, offset=10) 

plot(p8)

# add heatmap
p9 <-  gheatmap(p8, sort_char[colnames(sort_char)[grep('^char', colnames(sort_char))]] , offset=1, width=0.5, low="white", high="black", colnames_angle= 45, colnames_position = "top", font.size=1)

# plot
plot(p9)

