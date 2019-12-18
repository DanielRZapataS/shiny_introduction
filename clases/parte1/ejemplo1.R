library(shiny)

# runExample("01_hello")

ui <- fluidPage(
  
  # Panel con titulo
  titlePanel("Hola Shiny!"),
  
  # Utilizamos un layout de tipo sidebar
  sidebarLayout(
    
    # SidebarPanel
    sidebarPanel(
      width = 4,
      sliderInput( inputId = 'contenedores',
                   label = 'Numero de contenedores',
                   min = 1,
                   max = 50,
                   value = 25)
    ),
    
    # mainPanel
    mainPanel(
      
      # grafica de histograma
      plotOutput(outputId = "plot")
      
    )
  )
  
  
)



server <- function(input, output){
  
  output$plot <- renderPlot(
    #expresion que genera el plot
    {
      #browser()
      x <- faithful$waiting
      contenedores <- seq(min(x), max(x), length.out = input$contenedores + 1)
      
      hist(
        x,
        breaks = contenedores,
        col = "#75AADB",
        border = "white",
        xlab = " Tiempo de espera entre erupciones (en minutos)",
        main = "Histograma del tiempo de espera"
      )
    }
    
    
  )
  
}


shinyApp(ui = ui, server = server)

