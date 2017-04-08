library(data.table)
library(RColorBrewer)
library(plotly)

makePonteEncarreraETL <- function( s_path_data, s_path_family, s_path_color, b_save_result=FALSE){
  
  dt_data <- fread(s_path_data,na.strings = c("","ND"))
  dt_fam <- fread(s_path_family,na.strings = c("","ND"))
  dt_color <- fread(s_path_color)

  
  v_new_names = c("familia_carrera", "carrera", "institucion", "ubicacion", "tipo_institucion" , "tipo_gestion", 
                  "duracion", "rango_costo", "porc_ingresantes", "ingreso_promedio", "credito_universitario", 
                  "becas_educativas", "num_ingresantes", "num_postulantes", "num_matriculados")
  setnames(dt_data, v_new_names)
  
  dt_data[ , ingreso_promedio := as.numeric(ingreso_promedio)]
  
  dt_data[ is.na(rango_costo ) & tipo_gestion == "PUBLICA" , rango_costo :="0"]
  dt_data[ , costo_min:= as.numeric( gsub("-.*$","" , gsub(",","",rango_costo ) ))]
  dt_data[ , costo_max:= as.numeric( gsub("^.*-","" ,gsub(",","",rango_costo )  ) )]
  dt_data[ , text_detalle := paste0(institucion , '<br>',carrera) ]
  
  dt_data <- merge(dt_data, dt_fam, all.x=TRUE, by= "familia_carrera")
  dt_data <- merge(dt_data, dt_color, all.x=TRUE, by= "categoria")
  
  if(b_save_result == TRUE){
    save(file= "./data/output/DtData.RData",  dt_data)
    message("Data saved in ./data/output/DtData.RData")
  }
  
  return(dt_data)
  
}


makeCostIncomeGraph <- function(dt_data, s_cost_type = "min", s_color_pal= "Set1"){

  dt_data <- dt_data[ , .( costo_min,
                           costo_max, 
                           institucion, 
                           familia_carrera,
                           tipo_institucion, 
                           tipo_gestion,
                           ingreso_promedio,
                           categoria,
                           text_detalle,
                           color)]
  
  if(s_cost_type == "min" ){
    dt_data[ , costo :=  costo_min ]
    s_x_axis <- "Costo mínimo"
  } else if (s_cost_type == "max" ){
    dt_data[ , costo :=  costo_max ]
    s_x_axis <- "Costo máximo"
  } else if (s_cost_type == "mid range" ){
    dt_data[ , costo := (costo_min + costo_max)/2 ]
    s_x_axis <- "Marca de clase entre costo máximo y mínimo"
  } else {
    stop( "s_cost_type must be one of the following: 'max', 'min', 'mid range'")
  }
  
  i_num_categories <- dt_data[, length(unique(categoria))]
  
  p_costo_ingreso <- plot_ly(data = dt_data, 
                             x = ~costo, 
                             y = ~ingreso_promedio, 
                             mode = "markers", 
                             color = ~I(color),#~factor(categoria),
                             split = ~factor(categoria),
                             text = ~text_detalle,
                             hoverinfo = "text")
  p_costo_ingreso <- p_costo_ingreso %>% layout( xaxis = list(  title = s_x_axis, rangemode="tozero"),
                                                 yaxis = list(  title = "Ingreso promedio", rangemode="tozero"),
                                                 legend= list( bgcolor= "#F0F0F0"),
                                                 hovermode='closest'
                                                 )
  
  
  return(p_costo_ingreso)
}

seperateData <- function(dt_data){
  l_data <- list()
  l_data[["dt_with_income_cost"]] <- dt_data[ !( is.na(ingreso_promedio)  | is.na(costo_min))]
  
  l_data[["dt_with_missing"]] <- dt_data[  is.na(ingreso_promedio)  | is.na(costo_min)]
  return(l_data)
}

selectInputByDataCol <- function( dt_data, s_col, s_input_id, s_label){
  v_s_opt <- unique(dt_data[[s_col]])
  v_s_opt <- v_s_opt[ order(v_s_opt)]
  v_s_opt
  selectInput(inputId = s_input_id, label = s_label,
              choices = v_s_opt,
              selected = NULL,
              multiple = TRUE)
  
}

filterTypes <- function(dt_data, input){
  
  # tipo_institucion
  v_s_tipo_inst <- input$v_s_tipo_inst
  if(!is.null(v_s_tipo_inst)){
    dt_data <- dt_data[ tolower(tipo_institucion) %in% tolower( stri_trans_general(input$v_s_tipo_inst,"Latin-ASCII"))  ]
  }
  
  # tipo_gestion
  v_s_tipo_ges <- input$v_s_tipo_ges
  if(!is.null(v_s_tipo_ges)){
    dt_data <- dt_data[ tolower(tipo_gestion) %in% tolower( stri_trans_general(input$v_s_tipo_ges,"Latin-ASCII"))  ]
  }
  
  
  return(dt_data)
  
  
  
}


filterSpecific <- function(dt_data, v_values, s_col){
  
  if(!is.null(v_values)){
    dt_data <- dt_data[ dt_data[[s_col]] %in% v_values  ]
  }
  
  return(dt_data)
  
  
  
}

