library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Dashboard Básico",
                  titleWidth = 200,
                  disable = F),
  dashboardSidebar(width = 200, 
                   disable = F,
                   collapsed = F,
                   sliderInput(inputId = "rango",
                               label = "Rango de datos",
                               min = 1,
                               max = dim(faithful)[1],
                               value = c(1,dim(faithful)[1])),
                   hr(),
                   sliderInput(inputId = "bins",
                               label = "Numero de contenedores:",
                               min = 1,
                               max = 50,
                               value = 30)),
  dashboardBody(
    fluidRow(
      box(title = "Scatter",
          status = "success",
          solidHeader = T, 
          background = "orange",
          plotOutput(outputId = "scatPlot")
      

          ),
      box(title = "Histograma",
          status = "danger",
          solidHeader = T,
          
          plotOutput(outputId = "distPlot")

          )
    ),
    fluidRow(
      valueBoxOutput("observaciones",
                     width = 4),
      valueBoxOutput("max_waiting",
                     width = 4),
      valueBoxOutput("max_eruptions",
                     width = 4)
    )
  )
  
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$scatPlot <- renderPlot({
    
    data <- faithful[c(seq(input$rango[1], input$rango[2])),]
    plot(data$eruptions, data$waiting )
    
  })
  
  output$observaciones <- renderValueBox({
    
    valueBox(
      value = dim(faithful)[1],
      subtitle = "Observaciones",
      icon = icon("search", lib = "glyphicon"),
      color = "olive"
    )
    
  })
  
  output$max_waiting <- renderValueBox({
    
    valueBox(
      value = max(faithful$waiting),
      subtitle = "Máximo Tiempo Espera",
      icon = icon("time", lib = "glyphicon"),
      color = "red"
    )
    
  })
  
  output$max_eruptions <- renderValueBox({
    
    valueBox(
      value = max(faithful$eruptions),
      subtitle = "Máximo Tiempo Erup.",
      icon = icon("flash", lib = "glyphicon"),
      color = "teal"
    )
    
  })
  
}


shinyApp(ui, server)