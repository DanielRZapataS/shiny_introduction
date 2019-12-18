# Carga de paquetes ----
library(shiny)
library(quantmod)

# Carga fichero helpers ----
source("helpers.R")

# UI ----
ui <- fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Selecciona un índice de cotización. 
               La información se recogerá de Google finance."),
      
      textInput("siglas", "Siglas", "SPY"),
      
      dateRangeInput("fechas", 
                     "Rango de fechas",
                     start = "2013-01-01", 
                     end = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("log", "Dibuja el eje y en escala logarítmica", 
                    value = FALSE),
      
      checkboxInput("ajuste", 
                    "Ajusta los precios a la inflación", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot"))
  )
)

# Server ---

server <- function(input, output) {
  
  data <- reactive({
    cat("Llamada a Google finance \n")
    getSymbols(input$siglas, src = "google",
               from = input$fechas[1],
               to = input$fechas[2],
               auto.assign = FALSE)
  })
  
  
  dataAjustada <- reactive({
    
    if(!input$ajuste) return(data())
    cat("Cálculo de Ajuste \n")
    adjust(data())
    
    })

  
  
  
  
  output$plot <- renderPlot({
    
    chartSeries(dataAjustada(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
    
  })
  
}

# Run the app
shinyApp(ui, server)
