
library(shiny)

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("texto", "Introduce un texto"),
      uiOutput("UIcombo")
      
    ),
    mainPanel(
      
      
      
    )
    
  )
  
)

server <- function(input, output){
  
  output$UIcombo <- renderUI({
    
    lista_palabras <- unlist(strsplit(input$texto, " "))
    
    selectInput("combo", "Selecciona una de las opciones",
                choices = lista_palabras, selected = lista_palabras[1])
    
  })
  
}

shinyApp(ui, server)