# Full path and filename for the data file to be used
data_path <- "../data/smi.xlsx"

# Directory to contain json folder + name of folder
json_dir <- file.path("../interactome", "json")
# Name of desired json files up until the labels for filtering columns.
# Do not include file extension.
json_path <- paste0(json_dir, "/aacr2019_smi")

# Any additional manipulations to be done to the data frame read from 
# data_path (e.g. turn the keyword columns into one column).
# This is optional! Just need to make sure the column names later line up
# with the true column names in the data frame
manipulate <- function(x){
  # x will be the data frame
  x <- x %>% mutate(keywords=gsub(";NA", "", paste(keyword1, keyword2,
                                                   keyword3, keyword4,
                                                   sep=";")))
  return(x)
}

# Column which will be unique for each row
id_column <- "presentation_number"

# Which columns will we allow users to filter the data on?
# Aliases will be the name of that column in the visualization,
# and if NULL, we will just change underscores to spaces, and 
# capitalize words
filter_columns <- c("pharma_academia", "combination")
filter_aliases <- c("Academia or Pharma", "Combination")

# Which columns will we allow users to group results
# based on in our visualization?
# Aliases will be the name of that column in the visualization,
# and if NULL, we will just change underscores to spaces, and 
# capitalize words
grouping_cols <- c("tumor", "target", "sage_keyword", "model")
grouping_aliases <- NULL

# This vector, json_columns will include the names of columns we would 
# like to include in the json file for each presentation/publication
json_cols <- c("presentation_number", "presentation_number", 
               "abstract_title", "author_block", "presenter_firstname", 
               "presenter_lastname", "abstract_body", "keywords", 
               "primary_organ", "target", "tumor", "sage_keyword", 
               "pharma_academia", "combination", "model")
# json_aliases will contain the names of those fields in the json file,
# in order. There can be multiple json aliases mapped to the same column
# name, just make sure to duplicate the column name in json_cols
json_aliases <- c("id", "name", "title", "authors", "presenterFirst", 
                  "presenterLast", "text", "keywords", "organ", "target", 
                  "tumor", "sage", "pharma", "combo", "model")