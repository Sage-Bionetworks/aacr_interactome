library(XLConnect)
library(rjson)
library(wordcloud)
library(org.Hs.eg.db)
source("code/common.R")
source("code/geoFuncs.R")

options( java.parameters = "-Xmx4g" )
aacr2016 <- readWorksheetFromFile("data/AM 2016 Interactome Data_Reg Abstracts.xlsx",sheet="Interactome_Abstract_Bodies_FIN")
#author2016 <- readWorksheetFromFile("data/AM 2016 Interactome Data_Reg Abstract Author Data.xlsx",sheet="88576BC859294C6981300941E73099C")

# Not able to load two big excel files at the same time. Converted file to .csv using xlsx2csv 
aacr2016.csv <- read.csv("data/abstractData2016.csv", header=T)
author2016.csv <- read.csv("data/authorData2016.csv", header=T)
gc()


# Randomly select 400 out of 5266 
sample400.rand <- sample(1:5266,400,replace=F)
sample400 <- aacr2016[sample400.rand,]

# Get lat, lng of the location
location400 <- findLatLong(sample400$PRESENTER.CITY, sample400$PRESENTER.STATE, sample400$PRESENTER.COUNTRY)
sample400 <- cbind(sample400, location400)

# Get description of Categrory
sample400$CategoryDes <- sapply(sample400$Category,function(x){
  sub('.{2,3}\\s{3}', '', x)
})

# Make dtm
txt400 <- prepareText(paste(sample400$ABSTRACT.TITLE,
                         sample400$AbstractBody,
                         sample400$Category,
                         sample400$Subcategory,
                         sample400$Subclassification,
                         sample400$KEYWORD1, 
                         sample400$KEYWORD2, 
                         sample400$KEYWORD3,
                         sample400$KEYWORD4))

sample400$txt <- prepareText(txt400)

dtm400 <- makeDTM(sample400$txt)
d400 <- proxy::dist(as.matrix(dtm400), method = "cosine")
D400 <- as.matrix(d400)

# export abstract text to json 
apply(sample400, 1, function(x){
  id <- sub(" ", "",x[["CONTROLNUMBER"]])
  l <- list()
  l[["ID"]] <- id
  l[["institution"]] <- x[["PRESENTER.INSTITUTION"]]
  authors <- author2016.csv[author2016.csv$CONTROLNUMBER==id,]
  l[["authors"]] <- paste(authors$"AUTHORFIRSTNAME", authors$"AUTHORLASTNAME",sep=" ",collapse=", ")
  l[["text"]] <- x[["AbstractBody"]]
  fileConn<-file(paste("./sampleJSON/",id,".json",sep=""))
  writeLines(toJSON(l), fileConn)
  close(fileConn)
  return(NA)
})

df <- sample400
rownames(df) <- df$CONTROLNUMBER
S <- 1 - D400
ids <- df$CONTROLNUMBER
rownames(S) <- ids
colnames(S) <- ids

M <- computeGeoDistanceMatrix(df$lat,df$lng)
M_siml <- geo2SimilarityMatrix(M)
rownames(M_siml) <- ids
colnames(M_siml) <- ids


# Abstract
sampleHC <- cluster_mat(S,"euclidean",'ward.D2')
sampleHC.JSON <- HCtoJSON(sampleHC)
fileConn<-file("AACR2016_sample400_txt.json")
writeLines(toJSON(sampleHC.JSON), fileConn)
close(fileConn)
# # Location
# sampleGeoHC <- cluster_mat(M_siml,"euclidean",'ward.D2')
# sampleGeoHC.JSON <- HCtoJSON(sampleGeoHC)
# fileConn<-file("AACR2016_sample_geo.json")
# writeLines(toJSON(sampleGeoHC.JSON), fileConn)
# close(fileConn)
# Location csv
sampleGeo <- sample400[,c("CONTROLNUMBER","PRESENTER.FIRST","PRESENTER.LAST","PRESENTER.INSTITUTION","PRESENTER.CITY","PRESENTER.COUNTRY","ABSTRACT.TITLE","lat","lng")]
write.csv(sampleGeo, file = "AACR_sample400_geo_data.csv")


## build json for AACR catergories
root <- list(name="root",title="")
tmp <- split(df, factor(df$CategoryDes))
root[["children"]] <- lapply(names(tmp), function(categoryName){
  L <- list(id=categoryName,name=categoryName,presenterLast=categoryName)
  children <- list()
  tbl <- tmp[[categoryName]]
  for(i in 1:nrow(tbl)){
    row <- tbl[i,]
    children[[i]] <- list(id = row[["CONTROLNUMBER"]],
                          name=row[["CONTROLNUMBER"]],
                          title=row[["ABSTRACT.TITLE"]],
                          presenterFirst=row[["PRESENTER.FIRST"]],
                          presenterLast=row[["PRESENTER.LAST"]],
                          presenterInstitution=row[["PRESENTER.INSTITUTION"]],
                          presenterCity=row[["PRESENTER.CITY"]],
                          presenterCountry=row[["PRESENTER.COUNTRY"]],
                          keywords=paste(row[["KEYWORD1"]],row[["KEYWORD3"]],row[["KEYWORD3"]],row[["KEYWORD4"]],sep=";"))
                          #sageCode=gsub(" ","", row[["Sage.code"]], fixed=TRUE),
                          #phase=row[["Phase"]]) 
  }
  L[["children"]] <- children
  return (L)
})

fileConn<-file("AACR2016_sample400_cat.json")
writeLines(toJSON(root), fileConn)
close(fileConn)
