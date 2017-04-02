library(shiny)
library(shinydashboard)
library(plotly)
library(stringi)

load("./data/output/DtData.RData")
source("./functions.R")


v_s_ges_choices <- c("Pública", "Privada")
body <- dashboardBody(
  fluidRow(
    box(
      height = "500px",
      width = 10,
      h3("Ingresos por carrera y costo de programa"),
      plotlyOutput("plot")
    ),
    box(
      height = "100px",
      width = 2,
      radioButtons(inputId = "s_cost_type", label = "Prueba",choices = c("min", "max"))
    ),
    box(
      height = "100px",
      width = 2,
      checkboxGroupInput(inputId = "v_s_tipo_ges", label = "Tipo de gestión", choices = v_s_ges_choices, selected = v_s_ges_choices)
    ),
    box(
      height = "100px",
      width = 2,
      checkboxGroupInput(inputId = "v_s_tipo_inst", label = "Tipo de instución",
                         choices = c("Universidad","Instituto") ,
                         selected = "Universidad")
    ),
    box(
      height = "300px",
      width = 5,
      uiOutput("select_inst")
    )#,
    # box(
    #   height = "300px",
    #   width = 5,
    #   uiOutput("select_car")
    # ) 
    
  )
)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  body
)

# 
# ui <- fluidPage(
#   plotlyOutput("plot"),
#   verbatimTextOutput("event")
# )

server <- function(input, output) {
  
  makeDataReactive <- reactive({makeData(dt_data, input)})
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    makeCostIncomeGraph(makeDataReactive(), s_cost_type = input$s_cost_type)
  })
  
  v_s_opt <- reactive({
    dt_data_local <- makeDataReactive()
    v_s_opt <- unique(dt_data_local[["institucion"]])
    v_s_opt <- v_s_opt[ order(v_s_opt)]
    v_s_opt <- c("[Todas]",v_s_opt)
    v_s_opt
  })
  
  
  output$select_inst <- renderUI({({
      selectInputByDataCol(v_s_opt(), "institucion", "v_s_inst", "Institución" )
    }) 
  })
 #  output$select_car <- renderUI({isolate({
 #     selectInputByDataCol(makeDataReactive(), "carrera", "v_s_car", "Carrera" )
 #   }) 
 # })
  
}

shinyApp(ui, server)