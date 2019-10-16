library(dplyr)

## Running this file requires objects created with names
# data_path (filename of data being used), 
# json_dir (where to place the json files), 
# json_path (json_dir + common name of the json files w/o the extension)
# manipulate, a function which takes in a dataframe and returns a dataframe,
#   containing any manipulations one would like to do on the data before
#   creating the interactome
# id_column, column name of id, which should be unique for each row
# filter_columns, a vector of column names to allow users to filter by
# filter_aliases, names we should call these filtered columns in the jsons
# json_cols, vector of column values to include in the json objects
# json_aliases, vector of names to call these values
## So these objects must be defined in the sourced config file
## the path of which should be specified in the only argument 
## in the command line

source(commandArgs(trailingOnly=TRUE))

source("common.R")

if(endsWith(data_path, ".xlsx")) {
  library(readxl)
  tag_data <- read_excel(data_path)
} else if(endsWith(data_path, ".csv")) {
  library(readr)
  tag_data <- read_csv(data_path)
} else{
  print("Data is not in a recognized file type!")
}

# Perform predetermined data manipulations from config file
tag_data <- manipulate(tag_data)

# Create json folder inside interactome, for S3
dir.create(json_dir, showWarnings = FALSE)
#existing_files <- paste(json_dir, list.files(path = json_dir),sep = "/")
#if(length(existing_files) > 1){
#  length(file.remove(existing_files))
#}

# Export abstracts to individual jsons to be displayed
# when the leaves are clicked
apply(tag_data, 1, function(pub){
  info <- lapply(json_cols, function(col){
    pub[[col]]
  })
  names(info) <- json_aliases
  id <- trimws(as.character(pub[[id_column]]))
  fileConn<-file(paste0("../interactome/json/", id,".json"))
  writeLines(toJSON(info), fileConn)
  close(fileConn)
})

# combs_list will contain vectors of strings describing 
# all possible combinations of all levels of the filtering
# variables, as well as an "all" level for each, which 
# does not filter on that variable
combs_list <- list(NULL)
for(col in filter_columns){
  levs <- c("all", pull(tag_data, col) %>% unique)
  temp <- list()
  for(lev in levs){
    temp <- temp %>% append(lapply(combs_list, function(x){
      return(c(x, lev))
    }))
  }
  combs_list <- temp
}

# Create dfs (accessible using get(), each being a different filtering of our
# data based on a combination of levels of the filter_columns)
for(comb in combs_list){
  temp <- tag_data
  for(i in 1:length(comb)){
    if(comb[i] != "all"){
      temp <- temp[c(temp[, filter_columns[i]] == comb[i]), ]
    }
  }
  assign(paste0(comb, collapse=""), temp)
}

# We also may want to keep these dfs in a list 
combination_dfs <- lapply(combs_list, function(x){
  get(paste0(x, collapse=""))
  })

# 
for(group_col in grouping_cols){
  root <- list(name=group_col)
  for(comb in combs_list){
    comb_df <- get(paste0(comb, collapse=""))
    tmp <- split(comb_df, factor(pull(comb_df, group_col)))
    categoryCluster(root, tmp, 
                    paste0(paste0(c(json_path, group_col, comb), collapse="_"), 
                              ".json", collapse=""),
                    json_cols=json_cols, json_aliases=json_aliases)
  }
}

