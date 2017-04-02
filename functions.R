

makePonteEncarreraETL <- function( s_path_data, s_path_family, b_save_result=FALSE){
  library(data.table)
  
  dt_data <- fread(s_path_data,na.strings = c("","ND"))
  dt_fam <- fread(s_path_family,na.strings = c("","ND"))

  
  v_new_names = c("familia_carrera", "carrera", "institucion", "ubicacion", "tipo_institucion" , "tipo_gestion", 
                  "duracion", "rango_costo", "porc_ingresantes", "ingreso_promedio", "credito_universitario", 
                  "becas_educativas", "num_ingresantes", "num_postulantes", "num_matriculados")
  setnames(dt_data, v_new_names)
  
  dt_data[ , ingreso_promedio := as.numeric(ingreso_promedio)]
  
  dt_data[ is.na(rango_costo ) & tipo_gestion == "PUBLICA" , rango_costo :="0"]
  dt_data[ , costo_min:= as.numeric( gsub("-.*$","" , gsub(",","",rango_costo ) ))]
  dt_data[ , costo_max:= as.numeric( gsub("^.*-","" ,gsub(",","",rango_costo )  ) )]
  dt_data[ , text_detalle := paste0(institucion , '<br>',familia_carrera) ]
  
  dt_data <- merge(dt_data, dt_fam, all.x=T, by= "familia_carrera")
  if(b_save_result == T){
    save(file= "./data/output/DtData.RData",  dt_data)
    message("Data saved in ./data/output/DtData.RData")
  }
  
  return(dt_data)
  
}


makeCostIncomeGraph <- function(dt_data, s_cost_type = "min", s_color_pal= "Set1"){
  library(data.table)
  library(RColorBrewer)
  library(plotly)
  dt_data <- dt_data[ , .( costo_min,
                           costo_max, 
                           institucion, 
                           familia_carrera,
                           tipo_institucion, 
                           tipo_gestion,
                           ingreso_promedio,
                           categoria,
                           text_detalle)]
  dt_data <- dt_data[ ingreso_promedio > 0  ]
  
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
                             color = ~factor(categoria),
                             text = ~text_detalle,
                             hoverinfo = "text",
                             colors  = brewer.pal(i_num_categories,s_color_pal))
  p_costo_ingreso <- p_costo_ingreso %>% layout( xaxis = list(  title = s_x_axis, rangemode="tozero"),
                                                 yaxis = list(  title = "Ingreso promedio", rangemode="tozero"),
                                                 legend= list( bgcolor= "#F0F0F0"),
                                                 hovermode='closest',
                                                 margin= list(l=90, r=80, t=40, b=50),
                                                 plot_bgcolor= "#fdfdfd",
                                                 paper_bgcolor  ="#fdfdfd" )
  
  
  return(p_costo_ingreso)
}

makeInstitutionIncomeGraph <- function(dt_data, v_s_institutions=NULL, s_color_pal= "Set1"){
  library(data.table)
  library(RColorBrewer)
  library(plotly)
  dt_data <- dt_data[ , .( institucion, 
                           familia_carrera,
                           tipo_institucion, 
                           tipo_gestion,
                           ingreso_promedio,
                           categoria,
                           text_detalle)]
  
  dt_data <- dt_data[ institucion %in%  v_s_institutions &
                      ingreso_promedio > 0  ]
  
  i_num_categories <- dt_data[, length(unique(categoria))]
  p_institu_ingreso <- plot_ly(data = dt_data, 
                             x = institucion, 
                             y = ingreso_promedio, 
                             mode = "markers", 
                             color = factor(categoria),
                             text = text_detalle,
                             hoverinfo = "text",
                             colors  = brewer.pal(i_num_categories,s_color_pal))
  p_institu_ingreso <- p_institu_ingreso %>% layout( xaxis = list(  title = "Institución"),
                                                 yaxis = list(  title = "Ingreso Promedio", rangemode="tozero"),
                                                 legend= list( bgcolor= "#F0F0F0"),
                                                 hovermode='closest',
                                                 margin= list(l=90, r=80, t=40, b=50),
                                                 plot_bgcolor= "#fdfdfd",
                                                 paper_bgcolor  ="#fdfdfd" )
  return(p_institu_ingreso)
}


selectInputByDataCol <- function( v_s_opt, s_col, s_input_id, s_label){

  selectInput(inputId = s_input_id, label = s_label,
              choices = v_s_opt,
              selected = NULL,
              multiple = TRUE)
  
}

makeData <- function(dt_data, input){
  dt_data <- dt_data[ ingreso_promedio > 0  ]
  
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
  # institucion
  v_s_inst <- input$v_s_inst
  if(!is.null(v_s_inst)){
    if(! "[Todas]" %in% v_s_inst){
      dt_data <- dt_data[ institucion %in% v_s_inst  ]
    }
  }
  
  # # carrera
  # v_s_car <- input$v_s_car
  # if(!is.null(v_s_car)){
  #   if(! "[Todas]" %in% v_s_car){
  #     dt_data <- dt_data[ institucion %in% v_s_car  ]
  #   }
  # }
  
  
  return(dt_data)
  
   
  
}