library(shiny)

ui <- fluidPage(
  
  actionButton(inputId = "boton",
               label = "Procesar",
               icon = icon("remove", lib = "glyphicon"))
  
)

server <- function(input, output, session){
  
  observeEvent(
    input$boton,
    {
      updateActionButton(session = session,
                         inputId = "boton",
                         label = "Guardar",
                         icon = icon("ok", lib= "glyphicon"))
    }
  )
  
}

shinyApp (ui, server)
