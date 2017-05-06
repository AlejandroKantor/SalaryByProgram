correctLocationNames <- function(v_s_location){
  l_names <- list( "Ancash" = "Áncash", 
                   "Apurimac" = "Apurímac" , 
                   "Huanuco" = "Huánuco",
                   "Junin" = "Junín", 
                   "San Martin", "San Martín"
  )
  v_s_names <- names(l_names)
  for(s_name in v_s_names){
    s_correction <- l_names[[s_name]]
    v_s_location[ v_s_location == s_name] <- s_correction
  }
  return(v_s_location)
}

makePonteEncarreraETL <- function( s_path_data, s_path_family, s_path_color, b_save_result=FALSE){
  
  dt_data <- fread(s_path_data,na.strings = c("","ND"))
  dt_fam <- fread(s_path_family,na.strings = c("","ND"))
  dt_color <- fread(s_path_color)
  
  
  v_new_names = c("familia_carrera", "carrera", "institucion", "ubicacion", "tipo_institucion" , "tipo_gestion", 
                  "duracion", "rango_costo", "porc_ingresantes", "ingreso_promedio", "credito_universitario", 
                  "becas_educativas", "num_ingresantes", "num_postulantes", "num_matriculados")
  setnames(dt_data, v_new_names)
  
  dt_data[ , ingreso_promedio := as.numeric(ingreso_promedio)]
  dt_data[ , ubicacion := correctLocationNames(ubicacion)]
  
  dt_data[ is.na(rango_costo ) & tipo_gestion == "PUBLICA" , rango_costo :="0"]
  dt_data[ , costo_min:= as.numeric( gsub("-.*$","" , gsub(",","",rango_costo ) ))]
  dt_data[ , costo_max:= as.numeric( gsub("^.*-","" ,gsub(",","",rango_costo )  ) )]
  dt_data[ , text_detalle := paste0(institucion ," (",ubicacion,")",  '<br>',
                                    carrera, '<br>',
                                    "Rango costo: ",prettyNum(rango_costo, ",", preserve.width = "none"), '<br>',
                                    "Ingreso promedio: ", prettyNum(ingreso_promedio, big.mark = ",", preserve.width = "none")
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
                                                 plot_bgcolor= "#fdfdfd",
                                                 paper_bgcolor  ="#fdfdfd" ,
                                                   legend = list(y = -0.3, orientation = 'h'),
                                                 hovermode='closest'
  )
  p_costo_ingreso
  
  
  return(p_costo_ingreso) 
}

seperateData <- function(dt_data){
  l_data <- list()
  l_data[["dt_with_income_cost"]] <- dt_data[ !( is.na(ingreso_promedio)  | is.na(costo_min))]
  
  v_s_cols <- c(  "institucion", "carrera","ubicacion", "tipo_institucion","tipo_gestion","rango_costo","ingreso_promedio")
  dt_data <- dt_data[ , v_s_cols, with = FALSE]
  dt_data[ , rango_costo := prettyNum(rango_costo, big.mark = ",", preserve.width = "none")]
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
    dt_data <- dt_data[ tolower(tipo_institucion) %in% tolower( stri_trans_general(v_s_tipo_inst,"Latin-ASCII"))  ]
  }
  
  # tipo_gestion
  v_s_tipo_ges <- input$v_s_tipo_ges
  if(!is.null(v_s_tipo_ges)){
    dt_data <- dt_data[ tolower(tipo_gestion) %in% tolower( stri_trans_general(v_s_tipo_ges,"Latin-ASCII"))  ]
  }
  
  # ubicacion 
  v_s_ubicacion <- input$v_s_ubicacion
  if(!is.null(v_s_ubicacion)){
    dt_data <- dt_data[ ubicacion %in%  v_s_ubicacion ]
  }
  return(dt_data)
  
  
  
}


filterSpecific <- function(dt_data, v_values, s_col){
  
  if(!is.null(v_values)){
    dt_data <- dt_data[ dt_data[[s_col]] %in% v_values  ]
  }
  
  return(dt_data)
  
  
  
}

dataToDatatable <- function(dt_data){
  dt_data_local <- copy(dt_data)
  dt_data_local[ tipo_institucion == "UNIVERSIDAD",  tipo_institucion := "Universidad"] 
  dt_data_local[ tipo_institucion == "INSTITUTO",  tipo_institucion := "Instituto"] 
  dt_data_local[ tipo_gestion == "PUBLICA",  tipo_gestion := "Pública"] 
  dt_data_local[ tipo_gestion == "PRIVADA",  tipo_gestion := "Privada"] 
  
  v_s_cols <- c(  "institucion", "carrera","ubicacion", "tipo_institucion","tipo_gestion","rango_costo","ingreso_promedio")
  v_s_new_cols <- c( "Institución","Carrera",  "Ubicación","Tipo institución", "Tipo gestión", "Rango costo", "Ingreso promedio") 
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


getDataSourceInformation <- function(){
  tagList(
    tags$p("Todos los datos provienen de la fuente oficial del gobierno ", a("ponteencarrera.pe/.", href = "http://www.ponteencarrera.pe/", target="_blank"),  
           "No obstante, para obtener información oficial de los programas, por favor consultar con la institución educativa.",
           br(),
           "Para los programas públicos sin información de costo se ha asumido que su costo es de S/ 0."),
    tags$p("A continuación se cita las fuentes y definiciones detalladas en" , a("ponteencarrera.pe/", href = "http://www.ponteencarrera.pe/", target="_blank") ),
    tags$blockquote(
      tags$h4("Oferta educativa"),
      tags$b("Fuente: "), tags$br(),
      "MINEDU - DIGESUTPA - Sistema de Recojo de Información", 
      tags$br(),
      "Información del 2014 ",
      tags$br(),
      tags$b("Notas: "),
      tags$ul(
        tags$li("La información presentada en este portal fue reportada por cada institución educativa, de acuerdo con lo solicitado mediante los documentos: Oficio N°057-2014-MINEDU/DIGESUTP remitido el 30 de setiembre del 2014, Oficio Múltiple N°007-2015-MINEDU/VMGP-DIGESUTPA remitido el 13 de abril del 2015, Oficio Múltiple N°003-2015-MINEDU/VMGP-DIGESU, remitido el 27 de mayo del 2015."),
        tags$li("La información publicada considera lo reportado a diciembre del 2015."),
        tags$li("De presentarse alguna duda o consulta por parte de las instituciones educativas respecto al reporte de información, comunicarse al correo electrónico ofertaeducativa@minedu.gob.pe o a la central (01) 615-5855."),
        tags$li("De acuerdo a los comunicados emitidos por la Superintendencia Nacional de Educación Superior Universitaria – SUNEDU el 27/12/2016 y el 22/01/2017, además de la Resolución de Superintendencia N° 0014-2017-SUNEDU, se ha actualizado la información de la oferta educativa, eliminando las carreras que no cuentan con autorización. Para ver la lista de carreras retiradas del portal da clic aquí.")),
      tags$h4("Ingreso promedio"),
      tags$b("Fuente: "),tags$br(),
      "MTPE - OGETIC - OE- Planilla Electrónica 2015.",tags$br(),
      "Información al 52% de los trabajadores: sector privado (69%) y sector público (35%).",tags$br(),
      tags$b("Notas:"),
      tags$ul(
        tags$li("Se considera la remuneración bruta promedio antes de los descuentos de ley."),
        tags$li("Las remuneraciones han sido calculadas considerando a los trabajadores del sector privado y del sector público (regímenes D.L. Nº 728 y D.L. Nº1057)"),
        tags$li("Se considera jóvenes a los trabajadores de 18 a 29 años."),
        tags$li("Se considera solo a las combinaciones de familias de carreras e instituciones con 25 casos a más."),
        tags$li("Se considera solo a los trabajadores con jornada laboral completa."),
        tags$li("Se excluyen a los jóvenes con remuneraciones mayores al percentil 98 según familia de carreras."))))
  
}