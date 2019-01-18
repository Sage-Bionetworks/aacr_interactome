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

# Use only a sample to continue testing
# the first 100 abstracts
sample <- aacr2016[1:100,]
location <- findLatLong(sample$PRESENTER.CITY, sample$PRESENTER.STATE, sample$PRESENTER.COUNTRY)
sample <- cbind(sample, location)

# remove duplicates
dups <- duplicated(sample$ABSTRACT.TITLE)
sample <- sample[!dups,]

sample$CategoryAbbr <- sapply(sample$Category,function(x){
  unlist(strsplit(x,' '))[1]
})

sample$SubcategoryAbbr <- sapply(sample$Subcategory,function(x){
  unlist(strsplit(x,' '))[1]
})

sample$SubclassificationAbbr <- sapply(sample$Subclassification,function(x){
  unlist(strsplit(x,' '))[1]
})

txt <- prepareText(paste(sample$ABSTRACT.TITLE,
                         sample$AbstractBody,
                         sample$CategoryAbbr,
                         sample$Subcategory,
                         sample$SubclassificationAbbr,
                         sample$KEYWORD1, 
                         sample$KEYWORD2, 
                         sample$KEYWORD3,
                         sample$KEYWORD4))

sample$txt <- prepareText(txt)

# should change the Abbr to Description
dtm <- makeDTM(sample$txt)
word.freq <- as.numeric(as.array(slam::rollup(dtm, 1, FUN=function(x) { sum(x > 0)})) )


d <- proxy::dist(as.matrix(dtm), method = "cosine")
D <- as.matrix(d)
fit <- hclust(d, method="ward.D") 
plot(as.dendrogram(fit)) # display dendogram


# word cloud
pdf("cloud_sample.pdf"); 
names(word.freq) <- colnames(dtm)
word.freq.s <- sort(word.freq,decreasing=T)
wordcloud(names(word.freq.s)[1:70], word.freq.s[1:70],random.color=FALSE, colors=brewer.pal(9,"BuGn"),random.order=FALSE); 
dev.off()

pdf("countryCloud_sample.pdf")
loc <- sample$PRESENTER.COUNTRY
isUs <- loc == "United States"
loc[isUs] <- sample$PRESENTER.STATE[isUs]
tmp <- table(loc)
wordcloud(names(tmp), tmp, min.freq=1,random.color=FALSE,colors=rev(brewer.pal(9,"RdGy")))
dev.off()


# export abstract text to json 
apply(sample, 1, function(x){
  id <- sub(" ", "",x[["CONTROLNUMBER"]])
  l <- list()
  l[["ID"]] <- id
  l[["institution"]] <- x[["PRESENTER.INSTITUTION"]]
  authors <- author2016.csv[author2016.csv$CONTROLNUMBER==id,]
  l[["authors"]] <- paste(authors$"AUTHORFIRSTNAME", authors$"AUTHORLASTNAME",sep=" ",collapse=", ")
  l[["text"]] <- x[["AbstractBody"]]
  fileConn<-file(paste("./testJSON/",id,".json",sep=""))
  writeLines(toJSON(l), fileConn)
  close(fileConn)
  return(NA)
})

df <- sample
rownames(df) <- df$CONTROLNUMBER
S <- 1 - D
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
fileConn<-file("AACR2016_sample_txt.json")
writeLines(toJSON(sampleHC.JSON), fileConn)
close(fileConn)
# Location
sampleGeoHC <- cluster_mat(M_siml,"euclidean",'ward.D2')
sampleGeoHC.JSON <- HCtoJSON(sampleGeoHC)
fileConn<-file("AACR2016_sample_geo.json")
writeLines(toJSON(sampleGeoHC.JSON), fileConn)
close(fileConn)
# Location csv
sampleGeo <- sample[,c("CONTROLNUMBER","PRESENTER.FIRST","PRESENTER.LAST","PRESENTER.INSTITUTION","PRESENTER.CITY","PRESENTER.COUNTRY","ABSTRACT.TITLE","lat","lng")]
write.csv(sampleGeo, file = "AACR_sample_geo_data.csv")


# Get location for all
locationALL<- findLatLong(aacr2016$PRESENTER.CITY, aacr2016$PRESENTER.STATE, aacr2016$PRESENTER.COUNTRY)
aacr2016ALL <- cbind(aacr2016, locationALL)

aacr2016Geo <- aacr2016[,c("CONTROLNUMBER","PRESENTER.FIRST","PRESENTER.LAST","PRESENTER.INSTITUTION","PRESENTER.CITY","PRESENTER.COUNTRY","ABSTRACT.TITLE","lat","lng")]
write.csv(aacr2016Geo, file = "AACR2016_geo_data.csv")

# get 300 random samples
sample300 <- sample(1:5266,300,replace=F)
sample300_data <- aacr2016[sample300,]
location300 <- findLatLong(sample300_data$PRESENTER.CITY, sample300_data$PRESENTER.STATE, sample300_data$PRESENTER.COUNTRY)
sample300_data <- cbind(sample300_data, location300)

pdf("LocationCloud_300.pdf")
loc <- sample300_data$PRESENTER.COUNTRY
isUs <- loc == "United States"
loc[isUs] <- sample300_data$PRESENTER.STATE[isUs]
tmp <- table(loc)
wordcloud(names(tmp), tmp, min.freq=1,random.color=FALSE,colors=rev(brewer.pal(9,"RdGy")))
dev.off()

sample300Geo <- sample300_data[,c("CONTROLNUMBER","PRESENTER.FIRST","PRESENTER.LAST","PRESENTER.INSTITUTION","PRESENTER.CITY","PRESENTER.COUNTRY","ABSTRACT.TITLE","lat","lng")]
write.csv(sample300Geo, file = "AACR2016_sample300_geo_data.csv")