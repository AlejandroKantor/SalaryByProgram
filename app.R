library(data.table)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(plotly)
library(stringi)
library(shinyBS)

load("./data/output/DtData.RData")
source("./functions.R")
v_s_ges_choices <- c("Pública", "Privada")
v_s_ubic <- dt_data[ , unique(ubicacion)]
v_s_ubic <- v_s_ubic[ order(v_s_ubic)]
ui <- fluidPage(theme = "http://resultados.pe/assets/css/styles_feeling_responsive.css",
                fluidRow(
                  column( width = 3,
                          box(
                            width = NULL,
                            h3("Filtros"),
                            radioButtons(inputId = "s_cost_type", label = "Costo",choices = c("min", "max")),
                            checkboxGroupInput(inputId = "v_s_tipo_ges", label = "Tipo de gestión" ,
                                               choices = v_s_ges_choices, selected = v_s_ges_choices),
                            checkboxGroupInput(inputId = "v_s_tipo_inst", 
                                               label = "Tipo de instución",
                                               choices = c("Universidad","Instituto"),
                                               selected = "Universidad") ,
                            selectInput(inputId = "v_s_ubicacion", label = "Ubicación",
                                        choices = v_s_ubic,
                                        selected = NULL,
                                        multiple = TRUE),
                            uiOutput("select_inst"),
                            uiOutput("select_car")
                          ),
                          box(
                            width = NULL,
                            h3("Tabla completa de carreras"),
                            actionButton("tabBut", "Ver carreras")),
                          box(
                            width = NULL,
                            h3("Fuentes y definiciones"),
                            actionButton("fuentDef", "Ver fuentes y definiciones"),
                            br(),
                            br(),
                            a("Fuente original: ponteencarrera.pe/", href = "http://www.ponteencarrera.pe/", target="_blank")
                          )
                          
                  ),
                  column( width = 9,
                          box(
                            height = "600px",
                            width = NULL,
                            h3("Ingreso mensual por carrera y costo*"),
                            plotlyOutput("plot", height = "480px"), 
                            tags$sub("* solo incluye casos que tienen datos de ingreso promedio y de costos. Lista completa en 'Tabla completa de carreras'.")
                          )
                  ),
                  
                  bsModal("missingTable", "Carreras sin ingreso promedio o costo", "tabBut", size = "large",
                          dataTableOutput("formated_table")),
                  bsModal("fuenteDefiniciones", "Fuentes y definiciones", "fuentDef", size = "large",
                          getDataSourceInformation())
                  
                )
)
# 
# ui <- dashboardPage(
#   dashboardHeader(disable = TRUE),
#   dashboardSidebar(disable = TRUE),
#   body
# )


server <- function(input, output) {
  
  l_data <- seperateData(dt_data)
  dt_with_income_cost <- l_data[["dt_with_income_cost"]]
  dt_data_formated <- l_data[["dt_data"]]
  getDataWithTypeFilter <- reactive({filterTypes(dt_with_income_cost, input)})
  
  getDataWithTypeAndInstFilter <- reactive({
    filterSpecific(getDataWithTypeFilter(), input$v_s_inst, "institucion")
  })
  getDataFullFilters <- reactive({
    filterSpecific(getDataWithTypeAndInstFilter(), input$v_s_car, "carrera")
  })
  
  
  output$plot <- renderPlotly({
    makeCostIncomeGraph(getDataFullFilters(), s_cost_type = input$s_cost_type)
  })
  
  
  output$select_inst <- renderUI({
    selectInputByDataCol(getDataWithTypeFilter(), "institucion", "v_s_inst", "Institución" )
  })
  output$select_car <- renderUI({
    selectInputByDataCol(getDataWithTypeAndInstFilter(), "carrera", "v_s_car", "Carrera" )
  })
  
  
  
  output$formated_table <- shiny::renderDataTable({
    dt_data_formated_local <- dataToDatatable( dt_data_formated)
    
    return(dt_data_formated_local)
  }, options = getDataTableOptions())
  
}

shinyApp(ui, server)