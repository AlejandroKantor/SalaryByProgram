
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
  dt_data[ , text_detalle := paste0(institucion , '<br>',
                                    carrera, '<br>',
                                    "Rango costo: ",prettyNum(rango_costo, ","), '<br>',
                                    "Ingreso promedio: ", prettyNum(ingreso_promedio, big.mark = ",")
  ) ]
  
  dt_data <- merge(dt_data, dt_fam, all.x=TRUE, by= "familia_carrera")
  dt_data <- merge(dt_data, dt_color, all.x=TRUE, by= "categoria")
  
  if(b_save_result == TRUE){
    save(file= "./data/output/DtData.RData",  dt_data)
    message("Data saved in ./data/output/DtData.RData")
  }
  
  return(dt_data)
  
}


makeCostIncomeGraph <- function(dt_data, s_cost_type = "min"){
  
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
                             type = "scatter",
                             x = ~costo, 
                             y = ~ingreso_promedio, 
                             mode = "markers", 
                             color = ~I(color),
                             split = ~factor(categoria),
                             text = ~text_detalle,
                             hoverinfo = "text")
  p_costo_ingreso <- p_costo_ingreso %>% layout( xaxis = list(  title = s_x_axis, rangemode="tozero"),
                                                 yaxis = list(  title = "Ingreso promedio", rangemode="tozero"),
                                                 #legend= list( bgcolor= "#F0F0F0"),
                                                 legend = list(orientation = 'h'),
                                                 hovermode='closest'
  )
  
  
  return(p_costo_ingreso) 
}

seperateData <- function(dt_data){
  l_data <- list()
  l_data[["dt_with_income_cost"]] <- dt_data[ !( is.na(ingreso_promedio)  | is.na(costo_min))]
  
  v_s_cols <- c( "carrera", "institucion", "tipo_institucion","tipo_gestion","rango_costo","ingreso_promedio")
  dt_data <- dt_data[ , v_s_cols, with = FALSE]
  dt_data[ , rango_costo := prettyNum(rango_costo, big.mark = ",")]
  dt_data[ grep("^\\s*NA$",rango_costo), rango_costo := ""]
  
  dt_data[ , ingreso_promedio := prettyNum(ingreso_promedio, big.mark = ",")]
  dt_data[ grep("^\\s*NA$",ingreso_promedio), ingreso_promedio := ""]
  
  l_data[["dt_data"]] <- dt_data
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

dataWithMissingToDatatable <- function(dt_data){
  dt_data_local <- copy(dt_data)
  dt_data_local[ tipo_institucion == "UNIVERSIDAD",  tipo_institucion := "Universidad"] 
  dt_data_local[ tipo_institucion == "INSTITUTO",  tipo_institucion := "Instituto"] 
  dt_data_local[ tipo_gestion == "PUBLICA",  tipo_gestion := "Pública"] 
  dt_data_local[ tipo_gestion == "PRIVADA",  tipo_gestion := "Privada"] 
  
  v_s_cols <- c( "carrera", "institucion", "tipo_institucion","tipo_gestion","rango_costo","ingreso_promedio")
  v_s_new_cols <- c( "Carrera", "Institución", "Tipo institución", "Tipo gestión", "Rango costo", "Ingreso promedio") 
  setnames(dt_data_local, v_s_cols, v_s_new_cols)
  return(dt_data_local)
}

getDataTableOptions <- function(){
  
  l_paginate <- list(first = "Primero" ,
                     last= "Último",
                     s_next = "Siguiente",
                     previous = "Anterior"
  )
  names(l_paginate)[ names(l_paginate) == "s_next"] <-  "next"
  
  l_dattab_options <- list(pageLength=10, 
                           language = list(info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                                           emptyTable = "Ningún dato disponible en esta tabla", 
                                           search = "Buscar:",
                                           infoEmpty = "Mostrando registros del 0 al 0 de un total de 0 registros",
                                           lengthMenu = "Mostrar _MENU_ registros",
                                           paginate = l_paginate
                           ) 
  )
  
  return(l_dattab_options)
}