library(rgdal)
library(sp)
crs <- CRS("+proj=longlat +datum=WGS84")
shapes <- list.files("/Users/Tingleylab/Dropbox/Work/Alex_Lees/All/All", 
                     pattern = ".shp$", full.names=T, recursive=T)

taxonomy <- read.csv('/Users/Tingleylab/Dropbox/Work/ProtectedAreas/BirdLife_Checklist_Version_91/BirdLife_Checklist_Version_9_1.csv',
                     skip = 5)
taxonomy <- taxonomy[-which(taxonomy$BirdLife.taxonomic.treatment..R...recognised.as.a.species..NR...not.recognised.as.a.species == 'NR'), ]

taxonomy2 <- read.csv('/Users/Tingleylab/Dropbox/Work/ProtectedAreas/BirdLife_Checklist_Version_61/BirdLife_Checklist_Version_6.csv', skip=1)


mem.shapes <- vector("list", length(shapes))
for(i in 1:length(mem.shapes)){
  mem.shapes[[i]] <- maptools::readShapePoly(shapes[[i]], 
                                             delete_null_obj=T, force_ring=T, 
                                             proj4string=crs)
  print(i)
}

makeUniform<-function(SPDF){
  pref <- paste("shape_number", i, sep="")  #just putting the file name in front.
  newSPDF <- spChFIDs(SPDF,as.character(paste(pref,rownames(as(SPDF,"data.frame")),sep="_")))
  return(newSPDF)
}

newIDs <- list()
for(i in 1:length(mem.shapes)){
  newIDs[[i]] <- makeUniform(mem.shapes[[i]])
  print(i)
}
#newIDs<-lapply(mem.shapes,function(x) makeUniform(x))

birdRangeDF <- do.call( rbind, newIDs )

save(birdRangeDF, file="/Users/Tingleylab/Dropbox/Work/ProtectedAreas/birdRangeDF.Rdata")
load("/Users/Tingleylab/Dropbox/Work/ProtectedAreas/birdRangeDF.Rdata")

breedingDF <- birdRangeDF[which(birdRangeDF$SEASONAL %in% c(1,2)), ]

breedingDF$forest <- rep(NA, dim(breedingDF)[1])

for(i in 1:dim(breedingDF)[[1]]){
  sciname <- as.character(breedingDF$SCINAME[i])
  if(sciname %in% taxonomy$Scientific.name){
    comname <- as.character(taxonomy$Common.name[which(taxonomy$Scientific.name == sciname)])
    scin_alt <- gsub(" ", "-", sciname)
    comn_alt <- gsub(" ", "-", comname)
    comn_alt <- gsub("\'", "", comn_alt)
    thepage <- tryCatch(readLines(paste('http://datazone.birdlife.org/species/factsheet/', comn_alt, '-', scin_alt, '/details', sep='')), error = function(e){return(NA)})
    forestlines <- grep("Forest", thepage)
    if(length(forestlines) > 0){
      breedingDF$forest[i] <- as.numeric(length(which(grep("major", thepage[forestlines+2]) %in% c(grep(">resident", thepage[forestlines+3]), grep(">breeding", thepage[forestlines+3])))))
    }else{
      breedingDF$forest[i] <- 0
    }
  }
  print(i)
}

counter <- 0
for(i in which(is.na(breedingDF$forest))){
  counter <- counter+1
  print(counter)
  sciname2 <- as.character(breedingDF$SCINAME[i])
  if(sciname2 %in% taxonomy2$Scientific.name){
    spcode <- taxonomy2$SpcRecID..unique.value.asigned.to.each.taxonomic.entity.by.BirdLife..Use.this.to.compare.year.to.year.changes.[which(taxonomy2$Scientific.name == sciname2)]
    sciname <- as.character(taxonomy$Scientific.name[which(taxonomy$SpcRecID..used.in.previous.versions.of.the.checklist. == spcode)])
    comname <- as.character(taxonomy$Common.name[which(taxonomy$Scientific.name == sciname)])
    scin_alt <- gsub(" ", "-", sciname)
    comn_alt <- gsub(" ", "-", comname)
    comn_alt <- gsub("\'", "", comn_alt)
    thepage <- tryCatch(readLines(paste('http://datazone.birdlife.org/species/factsheet/', comn_alt, '-', scin_alt, '/details', sep='')), error = function(e){return(NA)})
    forestlines <- grep("Forest", thepage)
    if(length(forestlines) > 0){
      breedingDF$forest[which(breedingDF$SCINAME == sciname2)] <- 
        as.numeric(length(which(grep("major", thepage[forestlines+2]) %in% c(grep(">resident", thepage[forestlines+3]), grep(">breeding", thepage[forestlines+3])))))
    }else{
      breedingDF$forest[which(breedingDF$SCINAME == sciname2)] <- 0
    }
  }
}

View(breedingDF[(is.na(breedingDF$forest)), ])

save(breedingDF, file="/Users/Tingleylab/Dropbox/Work/ProtectedAreas/breedingDF.Rdata")

forestDF <- breedingDF[which(breedingDF$forest==1),]

save(forestDF, file="/Users/Tingleylab/Dropbox/Work/ProtectedAreas/forestDF.Rdata")


birdRangeDF_1 <- birdRangeDF[1:50,]
birdRangeDF_2 <- birdRangeDF[51:100,]

writeOGR(birdRangeDF_1, dsn = "/Users/Tingleylab/Dropbox/Work/ProtectedAreas/Birds/Birds1", layer="birds1", driver="ESRI Shapefile")
writeOGR(birdRangeDF_1, dsn = "/Users/Tingleylab/Dropbox/Work/ProtectedAreas/Birds/Birds2", layer="birds2", driver="ESRI Shapefile")


writeOGR(birdRangeDF, dsn = "/Users/Tingleylab/Dropbox/Work/ProtectedAreas/BirdsKML/birds.kml", layer="birds", driver="KML")





birdRangeDF <- readOGR("/Users/Tingleylab/Dropbox/Work/ProtectedAreas/Birds")
