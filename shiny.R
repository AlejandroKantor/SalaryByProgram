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
      h3("Ingresos por carrera y costo"),
      plotlyOutput("plot")
    ),
    box(
      height = "100px",
      width = 2,
      radioButtons(inputId = "s_cost_type", label = "Costo",choices = c("min", "max"))
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
    ),
    box(
      height = "300px",
      width = 5,
      uiOutput("select_car")
    )
    
  )
)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  body
)


server <- function(input, output) {
  
  l_data <- seperateData(dt_data)
  dt_with_income_cost <- l_data[["dt_with_income_cost"]]
  dt_with_missing <- l_data[["dt_with_missing"]]
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
  
}

shinyApp(ui, server)