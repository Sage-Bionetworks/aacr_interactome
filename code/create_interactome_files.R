library(readxl)
library(rjson)
library(stringr)
library(org.Hs.eg.db)
library(xlsx)
source("common.R")

# load data
adc_abstracts <- read_excel("../data/adc_2019_v4.xlsx")
smi_abstracts <- read_excel("../data/smi_2019_v5.xlsx")

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
tmp <- split(df.adc, factor(df.adc$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_all_all.json")
tmp <- split(df.adc.all.y, factor(df.adc.all.y$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_all_y.json")
tmp <- split(df.adc.all.n, factor(df.adc.all.n$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_all_n.json")

tmp <- split(df.adc.academia.all, factor(df.adc.academia.all$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_academia_all.json")
tmp <- split(df.adc.academia.y, factor(df.adc.academia.y$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_academia_y.json")
tmp <- split(df.adc.academia.n, factor(df.adc.academia.n$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_academia_n.json")

tmp <- split(df.adc.pharma.all, factor(df.adc.pharma.all$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_pharma_all.json")
tmp <- split(df.adc.pharma.y, factor(df.adc.pharma.y$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_pharma_y.json")
tmp <- split(df.adc.pharma.n, factor(df.adc.pharma.n$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_adc_tumor_pharma_n.json")

# SMI
tmp <- split(df.smi, factor(df.smi$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_all_all.json")
tmp <- split(df.smi.all.y, factor(df.smi.all.y$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_all_y.json")
tmp <- split(df.smi.all.n, factor(df.smi.all.n$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_all_n.json")

tmp <- split(df.smi.academia.all, factor(df.smi.academia.all$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_academia_all.json")
tmp <- split(df.smi.academia.y, factor(df.smi.academia.y$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_academia_y.json")
tmp <- split(df.smi.academia.n, factor(df.smi.academia.n$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_academia_n.json")

tmp <- split(df.smi.pharma.all, factor(df.smi.pharma.all$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_pharma_all.json")
tmp <- split(df.smi.pharma.y, factor(df.smi.pharma.y$tumor))
categoryCluster(root, tmp, "../interactome/json/aacr2019_smi_tumor_pharma_y.json")
tmp <- split(df.smi.pharma.n, factor(df.smi.pharma.n$tumor))
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
