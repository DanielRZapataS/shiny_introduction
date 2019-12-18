library(shiny)

ui <- fluidPage(
  # panel con titulo
  titlePanel("¡Hola shiny!"),
  
  # layout de tipo sidebar
  sidebarLayout(
    
    # siderbarpanel
    sidebarPanel(
      # h5("Panel lateral"),
      width = 4,
      sliderInput(inputId = "contenedores",
                  label = "Número de contenedores",
                  min = 1,
                  max = 50, 
                  value = 25)
    ),
    
    # mainPanle
    mainPanel(
      # h5("Panel principal"),
      # grafica de histograma 
      plotOutput(outputId = "plot")
      
    )
  )
  
)

server <- function(input, output){
  output$plot <- renderPlot(
    # expresión que genera el plot
    {
      # browser()
      x <- faithful$waiting
      contenedores <- seq(min(x), max(x), length.out = input$contenedores + 1)
      hist(
        x,
        breaks = contenedores,
        col = "#75AADB",
        border = "white",
        xlab = "Tiempo de espera entre erupciones (en minutos)",
        main = "Histograma del tiempo de espera"
      )
    }
  )
}

shinyApp(ui, server)
