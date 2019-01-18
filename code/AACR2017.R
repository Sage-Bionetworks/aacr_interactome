library(readxl)
library(rjson)
library(stringr)
library(org.Hs.eg.db)
source("code/common.R")
source("code/geoFuncs.R")

options(java.parameters = "-Xmx4g" )

adc <- read_excel("data/adc_2017_v4.xlsx")
smi <- read_excel("data/small_2017_v4.xlsx")
info <- read_excel("data/control_presentation.xlsx",col_types = c("numeric","text"))

# # double check if all abstracts were selected
# # adc
# table(adc$CONTROLNUMBER %in% info$CONTROLNUMBER)
# adc$CONTROLNUMBER[!adc$CONTROLNUMBER %in% info$CONTROLNUMBER]
# write.table(adc[which(!adc$CONTROLNUMBER %in% info$CONTROLNUMBER),],file = "adc_2017_v4_notFound.tsv",sep = "\t",row.names = FALSE,na = "")
# 
# foo <- merge(adc,info)
# bar <- foo[which(is.na(foo$PRESENTATIONNUMBER)),]
# write.table(bar, file="adc_2017_v4_noPresentNum.tsv", sep = "\t",row.names = FALSE,na = "")
# 
# # smi
# table(smi$CONTROLNUMBER %in% info$CONTROLNUMBER)
# smi$CONTROLNUMBER[!smi$CONTROLNUMBER %in% info$CONTROLNUMBER]
# write.table(smi[which(!smi$CONTROLNUMBER %in% info$CONTROLNUMBER),],file = "smi_2017_v4_notFound.tsv",sep = "\t",row.names = FALSE,na = "")
# 
# foo <- merge(smi,info)
# bar <- foo[which(is.na(foo$PRESENTATIONNUMBER)),]
# write.table(bar, file="smi_2017_v4_noPresentNum.tsv", sep = "\t",row.names = FALSE,na = "")

# Abstracts with presentation # only
adc <- merge(adc,info)
adc <- adc[which(!is.na(adc$PRESENTATIONNUMBER)),]

smi <- merge(smi,info)
smi <- smi[which(!is.na(smi$PRESENTATIONNUMBER)),]

# Get description of Categrory
adc$TOPIC1 <- sapply(adc$TOPIC1,function(x){
  sub(".{4}-\\d{2} ","", x)
})

smi$TOPIC1 <- sapply(smi$TOPIC1,function(x){
  sub(".{4}-\\d{2} ","", x)
})

# export abstract text to json 
apply(adc, 1, function(x){
  id <- sub(" ", "",x[["PRESENTATIONNUMBER"]])
  l <- list()
  l[["ID"]] <- id
  l[["institution"]] <- x[["INSTITUTIONNAME"]]
  l[["authors"]] <- paste(x[["AUTHORFIRSTNAME"]], x[["AUTHORLASTNAME"]],sep=" ",collapse=", ")
  l[["text"]] <- x[["AbstractAbstract1"]]
  l[["keywords"]] <- paste(x[["KEYWORD1"]],x[["KEYWORD3"]],x[["KEYWORD3"]],x[["KEYWORD4"]],sep=";")
  l[["organ"]] <- x[["PRIMARYORGAN1"]]
  l[["topic"]] <- x[["TOPIC1"]]
  l[["target"]] <- x[["TARGET"]]
  l[["tumor"]] <- x[["TUMOR"]]
  l[["combo"]] <- x[["COMBINATION"]]
  l[["sage"]] <- x[["SAGE_keyword"]]
  l[["pharma"]] <- x[["PHARMA_ACADEMIA"]]
  fileConn<-file(paste("./json/",id,".json",sep=""))
  writeLines(toJSON(l), fileConn)
  close(fileConn)
  return(NA)
})

apply(smi, 1, function(x){
  id <- sub(" ", "",x[["PRESENTATIONNUMBER"]])
  l <- list()
  l[["ID"]] <- id
  l[["institution"]] <- x[["INSTITUTIONNAME"]]
  l[["authors"]] <- paste(x[["AUTHORFIRSTNAME"]], x[["AUTHORLASTNAME"]],sep=" ",collapse=", ")
  l[["text"]] <- x[["AbstractAbstract1"]]
  l[["keywords"]] <- paste(x[["KEYWORD1"]],x[["KEYWORD3"]],x[["KEYWORD3"]],x[["KEYWORD4"]],sep=";")
  l[["organ"]] <- x[["PRIMARYORGAN1"]]
  l[["topic"]] <- x[["TOPIC1"]]
  l[["target"]] <- x[["TARGET"]]
  l[["tumor"]] <- x[["TUMOR"]]
  l[["combo"]] <- x[["COMBINATION"]]
  l[["sage"]] <- x[["SAGE_keyword"]]
  l[["pharma"]] <- x[["PHARMA_ACADEMIA"]]
  fileConn<-file(paste("./json/",id,".json",sep=""))
  writeLines(toJSON(l), fileConn)
  close(fileConn)
  return(NA)
})

# Outputs
df.adc <- adc
rownames(df.adc) <- adc$PRESENTATIONNUMBER
df.adc.academia <- df.adc[df.adc$PHARMA_ACADEMIA =="academia",]
df.adc.pharma <- df.adc[df.adc$PHARMA_ACADEMIA =="pharma",]

df.smi <- smi
rownames(df.smi) <- smi$PRESENTATIONNUMBER
df.smi.academia <- df.smi[df.smi$PHARMA_ACADEMIA =="academia",]
df.smi.pharma <- df.smi[df.smi$PHARMA_ACADEMIA =="pharma",]

# TARGET clustering
root <- list(name="TARGET",title="")

tmp <- split(df.adc, factor(df.adc$TARGET))
categoryCluster(root, tmp, "aacr2017_adc_target_all.json")
tmp <- split(df.adc.academia, factor(df.adc.academia$TARGET))
categoryCluster(root, tmp, "aacr2017_adc_target_academia.json")
tmp <- split(df.adc.pharma, factor(df.adc.pharma$TARGET))
categoryCluster(root, tmp, "aacr2017_adc_target_pharma.json")

tmp <- split(df.smi, factor(df.smi$TARGET))
categoryCluster(root, tmp, "aacr2017_smi_target_all.json")
tmp <- split(df.smi.academia, factor(df.smi.academia$TARGET))
categoryCluster(root, tmp, "aacr2017_smi_target_academia.json")
tmp <- split(df.smi.pharma, factor(df.smi.pharma$TARGET))
categoryCluster(root, tmp, "aacr2017_smi_target_pharma.json")
# #adc
#  # all_all
#  # all_academia
#  # all_pharma
# tmp <- split(df.adc, factor(df.adc$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_all_all.json")
# tmp <- split(df.adc.academia, factor(df.adc.academia$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_all_academia.json")
# tmp <- split(df.adc.pharma, factor(df.adc.pharma$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_all_pharma.json")
#  # target-tumor_all
#  # target-tumor_academia
#  # target-tumor_pharma
# df.adc.target.tumor <- df.adc[df.adc$SAGE_keyword =="target-tumor",]
# tmp <- split(df.adc.target.tumor, factor(df.adc.target.tumor$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_target-tumor_all.json")
# df.adc.target.tumor.a <- merge(df.adc.target.tumor,df.adc.academia)
# tmp <- split(df.adc.target.tumor.a, factor(df.adc.target.tumor.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_target-tumor_academia.json")
# df.adc.target.tumor.p <- merge(df.adc.target.tumor,df.adc.pharma)
# tmp <- split(df.adc.target.tumor.p, factor(df.adc.target.tumor.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_target-tumor_pharma.json")
#  # payload_all
#  # payload_academia
#  # payload_pharma
# df.adc.payload <- df.adc[df.adc$SAGE_keyword =="payload",]
# tmp <- split(df.adc.payload, factor(df.adc.payload$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_payload_all.json")
# df.adc.payload.a <- merge(df.adc.payload, df.adc.academia)
# tmp <- split(df.adc.payload.a, factor(df.adc.payload.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_payload_academia.json")
# df.adc.payload.p <- merge(df.adc.payload, df.adc.pharma)
# tmp <- split(df.adc.payload.p, factor(df.adc.payload.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_payload_pharma.json")
#  # technology_all
#  # technology_academia
#  # technology_pharma
# df.adc.technology <- df.adc[df.adc$SAGE_keyword =="technology",]
# tmp <- split(df.adc.technology, factor(df.adc.technology$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_technology_all.json")
# df.adc.technology.a <- merge(df.adc.technology, df.adc.academia)
# tmp <- split(df.adc.technology.a, factor(df.adc.technology.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_technology_academia.json")
# df.adc.technology.p <- merge(df.adc.technology, df.adc.pharma)
# tmp <- split(df.adc.technology.p, factor(df.adc.technology.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_technology_pharma.json")
#  # linker_all
#  # linker_academia
#  # linker_pharma
# df.adc.linker <- df.adc[df.adc$SAGE_keyword =="linker",]
# tmp <- split(df.adc.linker, factor(df.adc.linker$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_linker_all.json")
# df.adc.linker.a <- merge(df.adc.linker, df.adc.academia)
# tmp <- split(df.adc.linker.a, factor(df.adc.linker.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_linker_academia.json")
# df.adc.linker.p <- merge(df.adc.linker, df.adc.pharma)
# tmp <- split(df.adc.linker.p, factor(df.adc.linker.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_linker_pharma.json")
#  # other_all
#  # other_academia
#  # other_pharma
# df.adc.other <- df.adc[df.adc$SAGE_keyword =="other",]
# tmp <- split(df.adc.other, factor(df.adc.other$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_other_all.json")
# df.adc.other.a <- merge(df.adc.other, df.adc.academia)
# tmp <- split(df.adc.other.a, factor(df.adc.other.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_other_academia.json")
# df.adc.other.p <- merge(df.adc.other, df.adc.pharma)
# tmp <- split(df.adc.other.p, factor(df.adc.other.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_adc_target_other_pharma.json")
# 
# #smi
#  # all_all
#  # all_academia
#  # all_pharma
# tmp <- split(df.smi, factor(df.smi$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_all_all.json")
# tmp <- split(df.smi.academia, factor(df.smi.academia$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_all_academia.json")
# tmp <- split(df.smi.pharma, factor(df.smi.pharma$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_all_pharma.json")
#  # immune_effector_all
#  # immune_effector_academia
#  # immune_effector_pharma
# df.smi.immune_effector <- df.smi[df.smi$SAGE_keyword =="immune_effector",]
# tmp <- split(df.smi.immune_effector, factor(df.smi.immune_effector$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_immune_effector_all.json")
# df.smi.immune_effector.a <- merge(df.smi.immune_effector,df.smi.academia)
# tmp <- split(df.smi.immune_effector.a, factor(df.smi.immune_effector.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_immune_effector_academia.json")
# df.smi.immune_effector.p <- merge(df.smi.immune_effector,df.smi.pharma)
# tmp <- split(df.smi.immune_effector.p, factor(df.smi.immune_effector.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_immune_effector_pharma.json")
#  # cancer_cell_all
#  # cancer_cell_academia
#  # cancer_cell_pharma
# df.smi.cancer_cell <- df.smi[df.smi$SAGE_keyword =="cancer_cell",]
# tmp <- split(df.smi.cancer_cell, factor(df.smi.cancer_cell$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_cancer_cell_all.json")
# df.smi.cancer_cell.a <- merge(df.smi.cancer_cell,df.smi.academia)
# tmp <- split(df.smi.cancer_cell.a, factor(df.smi.cancer_cell.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_cancer_cell_academia.json")
# df.smi.cancer_cell.p <- merge(df.smi.cancer_cell,df.smi.pharma)
# tmp <- split(df.smi.cancer_cell.p, factor(df.smi.cancer_cell.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_cancer_cell_pharma.json")
#  # immunosuppressive_all
#  # immunosuppressive_academia
#  # immunosuppressive_pharma
# df.smi.immunosuppressive <- df.smi[df.smi$SAGE_keyword =="immunosuppressive",]
# tmp <- split(df.smi.immunosuppressive, factor(df.smi.immunosuppressive$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_immunosuppressive_alll.json")
# df.smi.immunosuppressive.a <- merge(df.smi.immunosuppressive,df.smi.academia)
# tmp <- split(df.smi.immunosuppressive.a, factor(df.smi.immunosuppressive.a$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_immunosuppressive_academia.json")
# df.smi.immunosuppressive.p <- merge(df.smi.immunosuppressive,df.smi.pharma)
# tmp <- split(df.smi.immunosuppressive.p, factor(df.smi.immunosuppressive.p$TARGET))
# categoryCluster(root, tmp, "aacr2017_smi_target_immunosuppressive_pharma.json")

# TUMOR clustering
root <- list(name="TUMOR",title="")

# #adc
# # all_all
# # all_academia
# # all_pharma
# tmp <- split(df.adc, factor(df.adc$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_all_all.json")
# tmp <- split(df.adc.academia, factor(df.adc.academia$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_all_academia.json")
# tmp <- split(df.adc.pharma, factor(df.adc.pharma$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_all_pharma.json")
# # target-tumor_all
# # target-tumor_academia
# # target-tumor_pharma
# df.adc.target.tumor <- df.adc[df.adc$SAGE_keyword =="target-tumor",]
# tmp <- split(df.adc.target.tumor, factor(df.adc.target.tumor$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_target-tumor_all.json")
# df.adc.target.tumor.a <- merge(df.adc.target.tumor,df.adc.academia)
# tmp <- split(df.adc.target.tumor.a, factor(df.adc.target.tumor.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_target-tumor_academia.json")
# df.adc.target.tumor.p <- merge(df.adc.target.tumor,df.adc.pharma)
# tmp <- split(df.adc.target.tumor.p, factor(df.adc.target.tumor.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_target-tumor_pharma.json")
# # payload_all
# # payload_academia
# # payload_pharma
# df.adc.payload <- df.adc[df.adc$SAGE_keyword =="payload",]
# tmp <- split(df.adc.payload, factor(df.adc.payload$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_payload_all.json")
# df.adc.payload.a <- merge(df.adc.payload, df.adc.academia)
# tmp <- split(df.adc.payload.a, factor(df.adc.payload.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_payload_academia.json")
# df.adc.payload.p <- merge(df.adc.payload, df.adc.pharma)
# tmp <- split(df.adc.payload.p, factor(df.adc.payload.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_payload_pharma.json")
# # technology_all
# # technology_academia
# # technology_pharma
# df.adc.technology <- df.adc[df.adc$SAGE_keyword =="technology",]
# tmp <- split(df.adc.technology, factor(df.adc.technology$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_technology_all.json")
# df.adc.technology.a <- merge(df.adc.technology, df.adc.academia)
# tmp <- split(df.adc.technology.a, factor(df.adc.technology.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_technology_academia.json")
# df.adc.technology.p <- merge(df.adc.technology, df.adc.pharma)
# tmp <- split(df.adc.technology.p, factor(df.adc.technology.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_technology_pharma.json")
# # linker_all
# # linker_academia
# # linker_pharma
# df.adc.linker <- df.adc[df.adc$SAGE_keyword =="linker",]
# tmp <- split(df.adc.linker, factor(df.adc.linker$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_linker_all.json")
# df.adc.linker.a <- merge(df.adc.linker, df.adc.academia)
# tmp <- split(df.adc.linker.a, factor(df.adc.linker.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_linker_academia.json")
# df.adc.linker.p <- merge(df.adc.linker, df.adc.pharma)
# tmp <- split(df.adc.linker.p, factor(df.adc.linker.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_linker_pharma.json")
# # other_all
# # other_academia
# # other_pharma
# df.adc.other <- df.adc[df.adc$SAGE_keyword =="other",]
# tmp <- split(df.adc.other, factor(df.adc.other$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_other_all.json")
# df.adc.other.a <- merge(df.adc.other, df.adc.academia)
# tmp <- split(df.adc.other.a, factor(df.adc.other.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_other_academia.json")
# df.adc.other.p <- merge(df.adc.other, df.adc.pharma)
# tmp <- split(df.adc.other.p, factor(df.adc.other.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_adc_tumor_other_pharma.json")
# 
# #smi
# # all_all
# # all_academia
# # all_pharma
# tmp <- split(df.smi, factor(df.smi$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_all_all.json")
# tmp <- split(df.smi.academia, factor(df.smi.academia$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_all_academia.json")
# tmp <- split(df.smi.pharma, factor(df.smi.pharma$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_all_pharma.json")
# # immune_effector_all
# # immune_effector_academia
# # immune_effector_pharma
# df.smi.immune_effector <- df.smi[df.smi$SAGE_keyword =="immune_effector",]
# tmp <- split(df.smi.immune_effector, factor(df.smi.immune_effector$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_immune_effector_all.json")
# df.smi.immune_effector.a <- merge(df.smi.immune_effector,df.smi.academia)
# tmp <- split(df.smi.immune_effector.a, factor(df.smi.immune_effector.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_immune_effector_academia.json")
# df.smi.immune_effector.p <- merge(df.smi.immune_effector,df.smi.pharma)
# tmp <- split(df.smi.immune_effector.p, factor(df.smi.immune_effector.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_immune_effector_pharma.json")
# # cancer_cell_all
# # cancer_cell_academia
# # cancer_cell_pharma
# df.smi.cancer_cell <- df.smi[df.smi$SAGE_keyword =="cancer_cell",]
# tmp <- split(df.smi.cancer_cell, factor(df.smi.cancer_cell$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_cancer_cell_all.json")
# df.smi.cancer_cell.a <- merge(df.smi.cancer_cell,df.smi.academia)
# tmp <- split(df.smi.cancer_cell.a, factor(df.smi.cancer_cell.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_cancer_cell_academia.json")
# df.smi.cancer_cell.p <- merge(df.smi.cancer_cell,df.smi.pharma)
# tmp <- split(df.smi.cancer_cell.p, factor(df.smi.cancer_cell.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_cancer_cell_pharma.json")
# # immunosuppressive_all
# # immunosuppressive_academia
# # immunosuppressive_pharma
# df.smi.immunosuppressive <- df.smi[df.smi$SAGE_keyword =="immunosuppressive",]
# tmp <- split(df.smi.immunosuppressive, factor(df.smi.immunosuppressive$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_immunosuppressive_alll.json")
# df.smi.immunosuppressive.a <- merge(df.smi.immunosuppressive,df.smi.academia)
# tmp <- split(df.smi.immunosuppressive.a, factor(df.smi.immunosuppressive.a$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_immunosuppressive_academia.json")
# df.smi.immunosuppressive.p <- merge(df.smi.immunosuppressive,df.smi.pharma)
# tmp <- split(df.smi.immunosuppressive.p, factor(df.smi.immunosuppressive.p$TUMOR))
# categoryCluster(root, tmp, "aacr2017_smi_tumor_immunosuppressive_pharma.json")

tmp <- split(df.adc, factor(df.adc$TUMOR))
categoryCluster(root, tmp, "aacr2017_adc_tumor_all.json")
tmp <- split(df.adc.academia, factor(df.adc.academia$TUMOR))
categoryCluster(root, tmp, "aacr2017_adc_tumor_academia.json")
tmp <- split(df.adc.pharma, factor(df.adc.pharma$TUMOR))
categoryCluster(root, tmp, "aacr2017_adc_tumor_pharma.json")

tmp <- split(df.smi, factor(df.smi$TUMOR))
categoryCluster(root, tmp, "aacr2017_smi_tumor_all.json")
tmp <- split(df.smi.academia, factor(df.smi.academia$TUMOR))
categoryCluster(root, tmp, "aacr2017_smi_tumor_academia.json")
tmp <- split(df.smi.pharma, factor(df.smi.pharma$TUMOR))
categoryCluster(root, tmp, "aacr2017_smi_tumor_pharma.json")

# COMBINATION clustering
root <- list(name="COMBINATION",title="")

# #adc
# # all_all
# # all_academia
# # all_pharma
# tmp <- split(df.adc, factor(df.adc$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_all_all.json")
# tmp <- split(df.adc.academia, factor(df.adc.academia$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_all_academia.json")
# tmp <- split(df.adc.pharma, factor(df.adc.pharma$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_all_pharma.json")
# # target-tumor_all
# # target-tumor_academia
# # target-tumor_pharma
# df.adc.target.tumor <- df.adc[df.adc$SAGE_keyword =="target-tumor",]
# tmp <- split(df.adc.target.tumor, factor(df.adc.target.tumor$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_target-tumor_all.json")
# df.adc.target.tumor.a <- merge(df.adc.target.tumor,df.adc.academia)
# tmp <- split(df.adc.target.tumor.a, factor(df.adc.target.tumor.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_target-tumor_academia.json")
# df.adc.target.tumor.p <- merge(df.adc.target.tumor,df.adc.pharma)
# tmp <- split(df.adc.target.tumor.p, factor(df.adc.target.tumor.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_target-tumor_pharma.json")
# # payload_all
# # payload_academia
# # payload_pharma
# df.adc.payload <- df.adc[df.adc$SAGE_keyword =="payload",]
# tmp <- split(df.adc.payload, factor(df.adc.payload$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_payload_all.json")
# df.adc.payload.a <- merge(df.adc.payload, df.adc.academia)
# tmp <- split(df.adc.payload.a, factor(df.adc.payload.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_payload_academia.json")
# df.adc.payload.p <- merge(df.adc.payload, df.adc.pharma)
# tmp <- split(df.adc.payload.p, factor(df.adc.payload.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_payload_pharma.json")
# # technology_all
# # technology_academia
# # technology_pharma
# df.adc.technology <- df.adc[df.adc$SAGE_keyword =="technology",]
# tmp <- split(df.adc.technology, factor(df.adc.technology$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_technology_all.json")
# df.adc.technology.a <- merge(df.adc.technology, df.adc.academia)
# tmp <- split(df.adc.technology.a, factor(df.adc.technology.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_technology_academia.json")
# df.adc.technology.p <- merge(df.adc.technology, df.adc.pharma)
# tmp <- split(df.adc.technology.p, factor(df.adc.technology.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_technology_pharma.json")
# # linker_all
# # linker_academia
# # linker_pharma
# df.adc.linker <- df.adc[df.adc$SAGE_keyword =="linker",]
# tmp <- split(df.adc.linker, factor(df.adc.linker$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_linker_all.json")
# df.adc.linker.a <- merge(df.adc.linker, df.adc.academia)
# tmp <- split(df.adc.linker.a, factor(df.adc.linker.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_linker_academia.json")
# df.adc.linker.p <- merge(df.adc.linker, df.adc.pharma)
# tmp <- split(df.adc.linker.p, factor(df.adc.linker.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_linker_pharma.json")
# # other_all
# # other_academia
# # other_pharma
# df.adc.other <- df.adc[df.adc$SAGE_keyword =="other",]
# tmp <- split(df.adc.other, factor(df.adc.other$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_other_all.json")
# df.adc.other.a <- merge(df.adc.other, df.adc.academia)
# tmp <- split(df.adc.other.a, factor(df.adc.other.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_other_academia.json")
# df.adc.other.p <- merge(df.adc.other, df.adc.pharma)
# tmp <- split(df.adc.other.p, factor(df.adc.other.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_adc_combo_other_pharma.json")
# 
# #smi
# # all_all
# # all_academia
# # all_pharma
# tmp <- split(df.smi, factor(df.smi$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_all_all.json")
# tmp <- split(df.smi.academia, factor(df.smi.academia$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_all_academia.json")
# tmp <- split(df.smi.pharma, factor(df.smi.pharma$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_all_pharma.json")
# # immune_effector_all
# # immune_effector_academia
# # immune_effector_pharma
# df.smi.immune_effector <- df.smi[df.smi$SAGE_keyword =="immune_effector",]
# tmp <- split(df.smi.immune_effector, factor(df.smi.immune_effector$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_immune_effector_all.json")
# df.smi.immune_effector.a <- merge(df.smi.immune_effector,df.smi.academia)
# tmp <- split(df.smi.immune_effector.a, factor(df.smi.immune_effector.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_immune_effector_academia.json")
# df.smi.immune_effector.p <- merge(df.smi.immune_effector,df.smi.pharma)
# tmp <- split(df.smi.immune_effector.p, factor(df.smi.immune_effector.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_immune_effector_pharma.json")
# # cancer_cell_all
# # cancer_cell_academia
# # cancer_cell_pharma
# df.smi.cancer_cell <- df.smi[df.smi$SAGE_keyword =="cancer_cell",]
# tmp <- split(df.smi.cancer_cell, factor(df.smi.cancer_cell$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_cancer_cell_all.json")
# df.smi.cancer_cell.a <- merge(df.smi.cancer_cell,df.smi.academia)
# tmp <- split(df.smi.cancer_cell.a, factor(df.smi.cancer_cell.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_cancer_cell_academia.json")
# df.smi.cancer_cell.p <- merge(df.smi.cancer_cell,df.smi.pharma)
# tmp <- split(df.smi.cancer_cell.p, factor(df.smi.cancer_cell.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_cancer_cell_pharma.json")
# # immunosuppressive_all
# # immunosuppressive_academia
# # immunosuppressive_pharma
# df.smi.immunosuppressive <- df.smi[df.smi$SAGE_keyword =="immunosuppressive",]
# tmp <- split(df.smi.immunosuppressive, factor(df.smi.immunosuppressive$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_immunosuppressive_all.json")
# df.smi.immunosuppressive.a <- merge(df.smi.immunosuppressive,df.smi.academia)
# tmp <- split(df.smi.immunosuppressive.a, factor(df.smi.immunosuppressive.a$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_immunosuppressive_academia.json")
# df.smi.immunosuppressive.p <- merge(df.smi.immunosuppressive,df.smi.pharma)
# tmp <- split(df.smi.immunosuppressive.p, factor(df.smi.immunosuppressive.p$COMBINATION))
# categoryCluster(root, tmp, "aacr2017_smi_combo_immunosuppressive_pharma.json")

tmp <- split(df.adc, factor(df.adc$COMBINATION))
categoryCluster(root, tmp, "aacr2017_adc_combo_all.json")
tmp <- split(df.adc.academia, factor(df.adc.academia$COMBINATION))
categoryCluster(root, tmp, "aacr2017_adc_combo_academia.json")
tmp <- split(df.adc.pharma, factor(df.adc.pharma$COMBINATION))
categoryCluster(root, tmp, "aacr2017_adc_combo_pharma.json")

tmp <- split(df.smi, factor(df.smi$COMBINATION))
categoryCluster(root, tmp, "aacr2017_smi_combo_all.json")
tmp <- split(df.smi.academia, factor(df.smi.academia$COMBINATION))
categoryCluster(root, tmp, "aacr2017_smi_combo_academia.json")
tmp <- split(df.smi.pharma, factor(df.smi.pharma$COMBINATION))
categoryCluster(root, tmp, "aacr2017_smi_combo_pharma.json")

# Sage clustering
root <- list(name="SAGE Category",title="")

tmp <- split(df.adc, factor(df.adc$SAGE_keyword))
categoryCluster(root, tmp, "aacr2017_adc_sage_all.json")
tmp <- split(df.adc.academia, factor(df.adc.academia$SAGE_keyword))
categoryCluster(root, tmp, "aacr2017_adc_sage_academia.json")
tmp <- split(df.adc.pharma, factor(df.adc.pharma$SAGE_keyword))
categoryCluster(root, tmp, "aacr2017_adc_sage_pharma.json")

tmp <- split(df.smi, factor(df.smi$SAGE_keyword))
categoryCluster(root, tmp, "aacr2017_smi_sage_all.json")
tmp <- split(df.smi.academia, factor(df.smi.academia$SAGE_keyword))
categoryCluster(root, tmp, "aacr2017_smi_sage_academia.json")
tmp <- split(df.smi.pharma, factor(df.smi.pharma$SAGE_keyword))
categoryCluster(root, tmp, "aacr2017_smi_sage_pharma.json")
