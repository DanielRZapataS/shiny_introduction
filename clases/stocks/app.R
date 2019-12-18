# Carga de paquetes ----
library(shiny)
library(quantmod)

# Carga fichero helpers ----
source("clases/stocks/helpers.R")

# UI ----
ui <- fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Selecciona un índice de cotización. 
               La información se recogerá de Google finance."),
      
      textInput("siglas", "Siglas", "AAPL"),
      
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
  
  output$plot <- renderPlot({
    data <- getSymbols(input$siglas, src = "yahoo",
                       from = input$fechas[1],
                       to = input$fechas[2],
                       auto.assign = FALSE)
    
    
    chartSeries(data, theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
  
}

# Run the app
shinyApp(ui, server)

runGitHub("heroesdeldato-cursoShiny-Ejemplo", "davidmanero")

