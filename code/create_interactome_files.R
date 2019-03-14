library(readxl)
library(rjson)
library(stringr)
library(org.Hs.eg.db)
library(xlsx)
source("common.R")

# load data
adc_abstracts <- read_excel("../data/adc_2019_v4.xlsx")
smi_abstracts <- read_excel("../data/smi_2019_v4.xlsx")

# create json folder inside interactome - for S3
dir.create(file.path("../interactome/", "json"), showWarnings = FALSE)

############ ADC #############

# export abstracts text to json 
apply(adc_abstracts, 1, function(x){
  id <- trimws(as.character(x[["presentation_number"]]))
  l <- list()
  l[["ID"]] <- id
  l[["title"]] <- x[["abstract_title"]]
  l[["authors"]] <- x[["author_block"]]
  l[['presenter']] <- paste(x[["presenter_firstname"]], x[["presenter_lastname"]],x[["presenter_generation"]],sep=" ")
  l[["text"]] <- x[["abstract_body"]]
  l[["keywords"]] <- gsub(";NA","",paste(x[["keyword1"]],x[["keyword2"]],x[["keyword3"]],x[["keyword4"]],sep=";"))
  l[["organ"]] <- x[["primary_organ"]]
  l[["target"]] <- x[["target"]]
  l[["tumor"]] <- x[["tumor"]]
  l[["sage"]] <- x[["sage_keyword"]]
  l[["pharma"]] <- x[["pharma_academia"]]
  l[["combo"]] <- x[["combination"]]
  l[["model"]] <- x[["model"]]
  fileConn<-file(paste0("../interactome/json/",id,".json"))
  writeLines(toJSON(l), fileConn)
  close(fileConn)
  return(NA)
})


############ SMI #############
apply(smi_abstracts, 1, function(x){
  id <- trimws(as.character(x[["presentation_number"]]))
  l <- list()
  l[["ID"]] <- id
  l[["title"]] <- x[["abstract_title"]]
  l[["authors"]] <- x[["author_block"]]
  l[['presenter']] <- paste(x[["presenter_firstname"]], x[["presenter_lastname"]],x[["presenter_generation"]],sep=" ")
  l[["text"]] <- x[["abstract_body"]]
  l[["keywords"]] <- gsub(";NA","",paste(x[["keyword1"]],x[["keyword2"]],x[["keyword3"]],x[["keyword4"]],sep=";"))
  l[["organ"]] <- x[["primary_organ"]]
  l[["target"]] <- x[["target"]]
  l[["tumor"]] <- x[["tumor"]]
  l[["sage"]] <- x[["sage_keyword"]]
  l[["pharma"]] <- x[["pharma_academia"]]
  l[["combo"]] <- x[["combination"]]
  l[["model"]] <- x[["model"]]
  fileConn<-file(paste0("../interactome/json/",id,".json"))
  writeLines(toJSON(l), fileConn)
  close(fileConn)
  return(NA)
})

# Outputs

df.adc <- adc_abstracts
df.smi <- smi_abstracts

# df.[topic].[pharma_academia].[combination]

# pharma_academia
df.adc.academia.all <- df.adc[df.adc$pharma_academia =="academia",]
df.adc.pharma.all <- df.adc[df.adc$pharma_academia =="pharma",]

df.smi.academia.all <- df.smi[df.smi$pharma_academia =="academia",]
df.smi.pharma.all <- df.smi[df.smi$pharma_academia =="pharma",]

# combination
df.adc.all.y <- df.adc[df.adc$combination =="combo",]
df.adc.all.n <- df.adc[df.adc$combination =="single",]

df.smi.all.y <- df.smi[df.smi$combination =="combo",]
df.smi.all.n <- df.smi[df.smi$combination =="single",]

# [pharma_academia]_[combination]
df.adc.academia.y <- merge(df.adc.academia.all,df.adc.all.y)
df.adc.academia.n <- merge(df.adc.academia.all,df.adc.all.n)
df.adc.pharma.y <- merge(df.adc.pharma.all,df.adc.all.y)
df.adc.pharma.n <- merge(df.adc.pharma.all,df.adc.all.n)

df.smi.academia.y <- merge(df.smi.academia.all,df.smi.all.y)
df.smi.academia.n <- merge(df.smi.academia.all,df.smi.all.n)
df.smi.pharma.y <- merge(df.smi.pharma.all,df.smi.all.y)
df.smi.pharma.n <- merge(df.smi.pharma.all,df.smi.all.n)

# TARGET clustering
# aacr2019_adc_target_[pharma_academia]_[combination].json
root <- list(name="TARGET",title="")

# ADC
tmp <- split(df.adc, factor(df.adc$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_all_all.json")
tmp <- split(df.adc.all.y, factor(df.adc.all.y$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_all_y.json")
tmp <- split(df.adc.all.n, factor(df.adc.all.n$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_all_n.json")

tmp <- split(df.adc.academia.all, factor(df.adc.academia.all$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_academia_all.json")
tmp <- split(df.adc.academia.y, factor(df.adc.academia.y$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_academia_y.json")
tmp <- split(df.adc.academia.n, factor(df.adc.academia.n$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_academia_n.json")

tmp <- split(df.adc.pharma.all, factor(df.adc.pharma.all$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_pharma_all.json")
tmp <- split(df.adc.pharma.y, factor(df.adc.pharma.y$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_pharma_y.json")
tmp <- split(df.adc.pharma.n, factor(df.adc.pharma.n$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_target_pharma_n.json")

# SMI
tmp <- split(df.smi, factor(df.smi$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_all_all.json")
tmp <- split(df.smi.all.y, factor(df.smi.all.y$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_all_y.json")
tmp <- split(df.smi.all.n, factor(df.smi.all.n$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_all_n.json")

tmp <- split(df.smi.academia.all, factor(df.smi.academia.all$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_academia_all.json")
tmp <- split(df.smi.academia.y, factor(df.smi.academia.y$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_academia_y.json")
tmp <- split(df.smi.academia.n, factor(df.smi.academia.n$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_academia_n.json")

tmp <- split(df.smi.pharma.all, factor(df.smi.pharma.all$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_pharma_all.json")
tmp <- split(df.smi.pharma.y, factor(df.smi.pharma.y$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_pharma_y.json")
tmp <- split(df.smi.pharma.n, factor(df.smi.pharma.n$target))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_target_pharma_n.json")

# TUMOR clustering
root <- list(name="TUMOR",title="")

# ADC
adc.tumor <- strsplit(df.adc$tumor, split = "_")
df.adc.id.temp <- data.frame(control_number= rep(df.adc$control_number, sapply(adc.tumor, length)), 
                             tumor = unlist(adc.tumor),stringsAsFactors = FALSE)

df.adc.temp <- df.adc
df.adc.temp$tumor <- NULL

df.adc.tumor <- merge(df.adc.id.temp,df.adc.temp,by = "control_number")

 # pharma_academia
df.adc.temp <- df.adc.academia.all
df.adc.temp$tumor <- NULL
df.adc.tumor.academia.all <- merge(df.adc.id.temp,df.adc.temp,by = "control_number")

df.adc.temp <- df.adc.pharma.all
df.adc.temp$tumor <- NULL
df.adc.tumor.pharma.all <- merge(df.adc.id.temp,df.adc.temp,by = "control_number")

 # combination
df.adc.temp <- df.adc.all.y
df.adc.temp$tumor <- NULL
df.adc.tumor.all.y <- merge(df.adc.id.temp,df.adc.temp,by = "control_number")

df.adc.temp <- df.adc.all.n
df.adc.temp$tumor <- NULL
df.adc.tumor.all.n <- merge(df.adc.id.temp,df.adc.temp,by = "control_number")

# [pharma_academia]_[combination]
df.adc.tumor.academia.y <- merge(df.adc.tumor.academia.all,df.adc.tumor.all.y)
df.adc.tumor.academia.n <- merge(df.adc.tumor.academia.all,df.adc.tumor.all.n)
df.adc.tumor.pharma.y <- merge(df.adc.tumor.pharma.all,df.adc.tumor.all.y)
df.adc.tumor.pharma.n <- merge(df.adc.tumor.pharma.all,df.adc.tumor.all.n)

 # make cluster
tmp <- split(df.adc.tumor, factor(df.adc.tumor$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_all_all.json")
tmp <- split(df.adc.tumor.all.y, factor(df.adc.tumor.all.y$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_all_y.json")
tmp <- split(df.adc.tumor.all.n, factor(df.adc.tumor.all.n$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_all_n.json")

tmp <- split(df.adc.tumor.academia.all, factor(df.adc.tumor.academia.all$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_academia_all.json")
tmp <- split(df.adc.tumor.academia.y, factor(df.adc.tumor.academia.y$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_academia_y.json")
tmp <- split(df.adc.tumor.academia.n, factor(df.adc.tumor.academia.n$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_academia_n.json")

tmp <- split(df.adc.tumor.pharma.all, factor(df.adc.tumor.pharma.all$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_pharma_all.json")
tmp <- split(df.adc.tumor.pharma.y, factor(df.adc.tumor.pharma.y$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_pharma_y.json")
tmp <- split(df.adc.tumor.pharma.n, factor(df.adc.tumor.pharma.n$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_adc_tumor_pharma_n.json")

# SMI
smi.tumor <- strsplit(df.smi$tumor, split = "_")
df.smi.id.temp <- data.frame(control_number= rep(df.smi$control_number, sapply(smi.tumor, length)), 
                             tumor = unlist(smi.tumor),stringsAsFactors = FALSE)

df.smi.temp <- df.smi
df.smi.temp$tumor <- NULL

df.smi.tumor <- merge(df.smi.id.temp,df.smi.temp,by = "control_number")

# pharma_academia
df.smi.temp <- df.smi.academia.all
df.smi.temp$tumor <- NULL
df.smi.tumor.academia.all <- merge(df.smi.id.temp,df.smi.temp,by = "control_number")

df.smi.temp <- df.smi.pharma.all
df.smi.temp$tumor <- NULL
df.smi.tumor.pharma.all <- merge(df.smi.id.temp,df.smi.temp,by = "control_number")

# combination
df.smi.temp <- df.smi.all.y
df.smi.temp$tumor <- NULL
df.smi.tumor.all.y <- merge(df.smi.id.temp,df.smi.temp,by = "control_number")

df.smi.temp <- df.smi.all.n
df.smi.temp$tumor <- NULL
df.smi.tumor.all.n <- merge(df.smi.id.temp,df.smi.temp,by = "control_number")

# [pharma_academia]_[combination]
df.smi.tumor.academia.y <- merge(df.smi.tumor.academia.all,df.smi.tumor.all.y)
df.smi.tumor.academia.n <- merge(df.smi.tumor.academia.all,df.smi.tumor.all.n)
df.smi.tumor.pharma.y <- merge(df.smi.tumor.pharma.all,df.smi.tumor.all.y)
df.smi.tumor.pharma.n <- merge(df.smi.tumor.pharma.all,df.smi.tumor.all.n)

# make cluster
tmp <- split(df.smi.tumor, factor(df.smi.tumor$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_all_all.json")
tmp <- split(df.smi.tumor.all.y, factor(df.smi.tumor.all.y$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_all_y.json")
tmp <- split(df.smi.tumor.all.n, factor(df.smi.tumor.all.n$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_all_n.json")

tmp <- split(df.smi.tumor.academia.all, factor(df.smi.tumor.academia.all$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_academia_all.json")
tmp <- split(df.smi.tumor.academia.y, factor(df.smi.tumor.academia.y$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_academia_y.json")
tmp <- split(df.smi.tumor.academia.n, factor(df.smi.tumor.academia.n$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_academia_n.json")

tmp <- split(df.smi.tumor.pharma.all, factor(df.smi.tumor.pharma.all$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_pharma_all.json")
tmp <- split(df.smi.tumor.pharma.y, factor(df.smi.tumor.pharma.y$tumor))
categoryCluster2(root, tmp, "../interactome/json/aacr2019_smi_tumor_pharma_y.json")
tmp <- split(df.smi.tumor.pharma.n, factor(df.smi.tumor.pharma.n$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_pharma_n.json")


# Sage clustering
root <- list(name="SAGE Category",title="")

# ADC
tmp <- split(df.adc, factor(df.adc$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_all_all.json")
tmp <- split(df.adc.all.y, factor(df.adc.all.y$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_all_y.json")
tmp <- split(df.adc.all.n, factor(df.adc.all.n$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_all_n.json")

tmp <- split(df.adc.academia.all, factor(df.adc.academia.all$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_academia_all.json")
tmp <- split(df.adc.academia.y, factor(df.adc.academia.y$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_academia_y.json")
tmp <- split(df.adc.academia.n, factor(df.adc.academia.n$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_academia_n.json")

tmp <- split(df.adc.pharma.all, factor(df.adc.pharma.all$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_pharma_all.json")
tmp <- split(df.adc.pharma.y, factor(df.adc.pharma.y$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_pharma_y.json")
tmp <- split(df.adc.pharma.n, factor(df.adc.pharma.n$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_sage_pharma_n.json")

# SMI
tmp <- split(df.smi, factor(df.smi$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_all_all.json")
tmp <- split(df.smi.all.y, factor(df.smi.all.y$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_all_y.json")
tmp <- split(df.smi.all.n, factor(df.smi.all.n$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_all_n.json")

tmp <- split(df.smi.academia.all, factor(df.smi.academia.all$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_academia_all.json")
tmp <- split(df.smi.academia.y, factor(df.smi.academia.y$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_academia_y.json")
tmp <- split(df.smi.academia.n, factor(df.smi.academia.n$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_academia_n.json")

tmp <- split(df.smi.pharma.all, factor(df.smi.pharma.all$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_pharma_all.json")
tmp <- split(df.smi.pharma.y, factor(df.smi.pharma.y$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_pharma_y.json")
tmp <- split(df.smi.pharma.n, factor(df.smi.pharma.n$sage_keyword))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_sage_pharma_n.json")

# Model clustering
root <- list(name="MODEL",title="")

# ADC
tmp <- split(df.adc, factor(df.adc$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_all_all.json")
tmp <- split(df.adc.all.y, factor(df.adc.all.y$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_all_y.json")
tmp <- split(df.adc.all.n, factor(df.adc.all.n$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_all_n.json")

tmp <- split(df.adc.academia.all, factor(df.adc.academia.all$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_academia_all.json")
tmp <- split(df.adc.academia.y, factor(df.adc.academia.y$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_academia_y.json")
tmp <- split(df.adc.academia.n, factor(df.adc.academia.n$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_academia_n.json")

tmp <- split(df.adc.pharma.all, factor(df.adc.pharma.all$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_pharma_all.json")
tmp <- split(df.adc.pharma.y, factor(df.adc.pharma.y$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_pharma_y.json")
tmp <- split(df.adc.pharma.n, factor(df.adc.pharma.n$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_model_pharma_n.json")

# SMI
tmp <- split(df.smi, factor(df.smi$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_all_all.json")
tmp <- split(df.smi.all.y, factor(df.smi.all.y$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_all_y.json")
tmp <- split(df.smi.all.n, factor(df.smi.all.n$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_all_n.json")

tmp <- split(df.smi.academia.all, factor(df.smi.academia.all$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_academia_all.json")
tmp <- split(df.smi.academia.y, factor(df.smi.academia.y$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_academia_y.json")
tmp <- split(df.smi.academia.n, factor(df.smi.academia.n$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_academia_n.json")

tmp <- split(df.smi.pharma.all, factor(df.smi.pharma.all$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_pharma_all.json")
tmp <- split(df.smi.pharma.y, factor(df.smi.pharma.y$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_pharma_y.json")
tmp <- split(df.smi.pharma.n, factor(df.smi.pharma.n$model))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_model_pharma_n.json")
