library(data.table)


source("./functions.R")
s_path_data <- "./data/output/ponteEnCarrera.csv"
s_path_family <- "./data/output/familia.csv"
dt_data <- makePonteEncarreraETL( s_path_data, s_path_family, b_save_result=T)
