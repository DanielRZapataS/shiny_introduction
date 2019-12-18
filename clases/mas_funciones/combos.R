library(shiny)

ui <- fluidPage(
  
  numericInput(inputId = "numero",
               label = "Selecciona el rango de números:",
               value = 3, 
               min = 3,
               max = 5),
  
  selectInput(inputId = "combo",
              label = "Selecciona un número",
              choices = c(1,2,3), selected = 1),
  
  actionButton(inputId = "boton",
               label = "Procesar",
               icon = icon("remove", lib = "glyphicon"))
  
)

server <- function(input, output, session){
  
  observeEvent(
    input$boton,
    {
      
      updateSelectInput(session = session,
                        inputId = "combo",
                        choices = c(seq(1,input$numero)),
                        selected = max(c(seq(1,input$numero)))
                        )
      
      updateActionButton(session = session,
                         inputId = "boton",
                         label = "Guardar",
                         icon = icon("ok", lib= "glyphicon"))
    }
  )
  
}

shinyApp (ui, server)