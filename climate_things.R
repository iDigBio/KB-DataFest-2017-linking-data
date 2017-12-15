
## function based on Remko Duursma's work

get_worldclim_rasters <- function(topath, clean=FALSE){
  
  download_worldclim <- function(basen, topath){
    
    wc_fn_full <- file.path(topath, basen)
    
    if(!file.exists(wc_fn_full)){
      message("Downloading WorldClim 10min layers ... ", appendLF=FALSE)
      download.file(file.path("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur",basen),
                    wc_fn_full, mode="wb")
      message("done.")
    }
    
    u <- unzip(wc_fn_full, exdir=topath)
    
    return(u)
  }
  
  download_worldclim("tmean_10m_esri.zip", topath)
  download_worldclim("prec_10m_esri.zip", topath) 
  
  if(clean){
    unlink(c(wc_fn_full,dir(file.path(topath,"tmean"),recursive=TRUE)))
    unlink(c(wc_fn_full,dir(file.path(topath,"prec"),recursive=TRUE)))
  }
  
  # Read the rasters into a list
  tmean_raster <- list()
  prec_raster <- list()
  
  for(i in 1:12){
    tmean_raster[[i]] <- raster(file.path(topath, sprintf("tmean/tmean_%s", i)))
    prec_raster[[i]] <- raster(file.path(topath, sprintf("prec/prec_%s", i)))
  }
 
  return(list(tmean_raster=tmean_raster, prec_raster=prec_raster))
}

get_worldclim_prectemp <- function(data, topath=tempdir(), return=c("all","summary"), worldclim=NULL){
  
  return <- match.arg(return)
  
  if(is.null(worldclim)){
    worldclim <- get_worldclim_rasters(topath)
  }
  tmean_raster <- worldclim$tmean_raster
  prec_raster <- worldclim$prec_raster
  
  #extract worldclim data; extract the gridCell ID for observations
  tmeanm <- precm <- matrix(ncol=12, nrow=nrow(data))
  for(i in 1:12){
    tmeanm[,i] <- 0.1 * extract(tmean_raster[[i]], cbind(data$longitude,data$latitude), method='simple')
    precm[,i] <- extract(prec_raster[[i]], cbind(data$longitude,data$latitude), method='simple')
  }
  colnames(tmeanm) <- paste0("tmean_",1:12)
  colnames(precm) <- paste0("prec_",1:12)
  
  pxy <- cbind(data, as.data.frame(tmeanm), as.data.frame(precm))
  names(pxy)[2:3] <- c("longitude","latitude")
  
  pxy$MAT <- apply(pxy[,grep("tmean_",names(pxy))],1,mean)
  pxy$MAP <- apply(pxy[,grep("prec_",names(pxy))],1,sum)
  
  # 
  if(return == "all")return(pxy)
  
  if(return == "summary"){
    
    dfr <- suppressWarnings(with(pxy, data.frame(species=unique(data$species),
                                                 n=nrow(data),
                                                 lat_mean=mean(latitude,na.rm=TRUE),
                                                 long_mean=mean(longitude,na.rm=TRUE),
                                                 MAT_mean=mean(MAT,na.rm=TRUE),
                                                 MAT_q05=quantile(MAT,0.05,na.rm=TRUE),
                                                 MAT_q95=quantile(MAT,0.95,na.rm=TRUE),
                                                 MAP_mean=mean(MAP,na.rm=TRUE),
                                                 MAP_q05=quantile(MAP,0.05,na.rm=TRUE),
                                                 MAP_q95=quantile(MAP,0.95,na.rm=TRUE))))
    rownames(dfr) <- NULL
    return(dfr)
  }
}

################################
library(raster)
####
idig<-read.csv('./pheno_specimen_with_chars.csv', stringsAsFactors=FALSE)
idig[idig=='?'] <- NA
idig[idig==''] <- NA

head(idig)
idig$"spn" <- paste(idig$genus, idig$specificepithet, sep="_")
cleaned.df <- idig[c(25,7, 9:24)]


world <- map_data("world")


### map occurences of traits present
for (char in colnames(cleaned.df[c(5:18)]))
{
  
  print(char)
  col <- which(colnames(cleaned.df)==char)
  sub_char <- na.omit(cleaned.df[c(1:4,col)], cols= cleaned.df[c(col, 3:4)])  
    
  char_present <- subset(sub_char, sub_char[char]=="1")
    
  worldmap <- ggplot() +  geom_path(data=world, aes(x=long, y=lat, group=group))+  scale_y_continuous(breaks=(-2:2) * 30) +
      scale_x_continuous(breaks=(-4:4) * 45) + theme_bw()
    
  worldmap + geom_point(data=char_present, aes(x=lon,y=lat,color=char))
  
  #### plotting occurences of taxa with trait presence
  ### !this represents partly only that sampling in some regions is denser than in others!
  
  par(cex.main=0.9, mfrow=c(1,2), mar=c(4,4,1,1), mgp=c(2,0.5,0), tcl=0.2)
  hist(char_present$lon, breaks=100, main="Longitidunal distribution of species with trait present", xlab="Longitude")
  hist(char_present$lat, breaks=100, main="Latitudinal distribution of species with trait present", xlab="Latitude")
  
  
  
  input <- na.omit(char_present[c(1,3,4)])
  colnames(input) <- c("speciesname", "latitude", "longitude")
  clim_occ <- get_worldclim_prectemp(input, return="all")
  par(cex.main=0.9, mfrow=c(1,2), mar=c(4,4,1,1), mgp=c(2,0.5,0), tcl=0.2)
  with(clim_occ, hist(MAT, main="Mean annual temperature of species with the trait present based on all occurence points for the species in IDigBio"))
  with(clim_occ, hist(MAP, main="Mean annual precipitation of species with the trait present based on all occurence points for the species in IDigBio"))

  
}  

