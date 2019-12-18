library(shiny)

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("numero", "Seleciona un número",
                   min = 3, max = 6, value = 3),
      # selectInput("combo", "Selecciona una de las opciones",
      #             choices = c(seq(1:3)), selected = 2)
      uiOutput("UIcombo")
      
    ),
    mainPanel(
      
      
      
    )
    
  )
  
)

server <- function(input, output){
  
  output$UIcombo <- renderUI({
    
    selectInput("combo", "Selecciona una de las opciones",
                choices = c(seq(1:input$numero)), selected = 2)
    
  })
  
}

shinyApp(ui, server)
