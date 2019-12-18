library(shiny)

ui <- fluidPage(
  
  sidebarLayout(position = "left",
    
    sidebarPanel(
      
      sliderInput("obs", "Introduce el número de observaciones:",
                  min = 10, max = 1000, value = 500)
      
    ),
    
    mainPanel(
      
      plotOutput("distPlot")
      
    )
    
  )
  
)

server <- function(input, output){
  
  output$distPlot <- renderPlot({
    
    hist(rnorm(input$obs))
    
  })
  
}

shinyApp(ui, server)
