library(shiny)
library(shinydashboard)
library(plotly)
library(stringi)
library(shinyBS)

load("./data/output/DtData.RData")
source("./functions.R")
v_s_ges_choices <- c("Pública", "Privada")
body <- dashboardBody(
  fluidRow(
    column( width = 8,
            box(
              height = "500px",
              width = NULL,
              h3("Ingreso mensual por carrera y costo"),
              plotlyOutput("plot") 
            ),  
            box(
              width = NULL,
              column( width = 4, 
                      radioButtons(inputId = "s_cost_type", label = "Costo",choices = c("min", "max"))),
              column( width = 4,  
                      checkboxGroupInput(inputId = "v_s_tipo_ges", label = "Tipo de gestión" ,
                                                     choices = v_s_ges_choices, selected = v_s_ges_choices)),
              column( width = 4,       
                      checkboxGroupInput(inputId = "v_s_tipo_inst", 
                                                          label = "Tipo de instución",
                                                          choices = c("Universidad","Instituto"),
                                                          selected = "Universidad")
                      )
              
            )
            
    ),
    
    box(
      width = 4,
      h4("Carreras sin ingreso promedio o costo"),
      actionButton("tabBut", "Ver casos"),
      br(),
      br(),
      uiOutput("select_inst"),
      uiOutput("select_car")
    )
    ,
    box(
      width = 4,
      h4("Fuentes y definiciones"),
      actionButton("fuentDef", "Ver fuentes y definiciones")
    ),
 
    bsModal("missingTable", "Carreras sin ingreso promedio o costo", "tabBut", size = "large",
            dataTableOutput("missing_table")),
    bsModal("fuenteDefiniciones", "Fuentes y definiciones", "fuentDef", size = "large",
            p("Información de fuentes"))

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
  
  
  
  output$missing_table <- renderDataTable({
    dt_with_missing_filt <- dataWithMissingToDatatable( dt_with_missing)
    
    return(dt_with_missing_filt)
  }, options = getDataTableOptions())
    
}

shinyApp(ui, server)