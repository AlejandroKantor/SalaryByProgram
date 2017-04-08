library(data.table)


source("./functions.R")
s_path_data <- "./data/output/ponteEnCarrera.csv"
s_path_family <- "./data/output/familia.csv"
s_path_color <- "./data/output/color.csv"
dt_data <- makePonteEncarreraETL( s_path_data,
                                  s_path_family,
                                  s_path_color,
                                  b_save_result=T)
