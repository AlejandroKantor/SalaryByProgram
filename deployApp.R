v_s_app_files <- c("app.R"  ,
                   "data/output/DtData.RData"  ,
                   "functions.R",
                   "rsconnect/shinyapps.io/alejandrokantor/shinyapp.dcf" )
rsconnect::deployApp( getwd(),appFiles = v_s_app_files)