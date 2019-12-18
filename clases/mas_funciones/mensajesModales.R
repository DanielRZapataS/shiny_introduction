# Hay 3 partes fundamentales en los diálogos
#1. El HTML del dialogo modal
#2. La llamada al comando showModal()
#3. Un "observer" que junte esas dos partes

library(shiny)

ui <- basicPage(
  
  actionButton("mostrar", "Muestra el diálogo modal")
  
)

server <- function(input, output) {
  
  observeEvent(input$mostrar,{
    
    showModal(modalDialog(
      
      title = "Mensaje Importante",
      "¡¡¡Esto es un mensaje importante!!!",
      #footer = "Esto es un pie de página",
      size = "m",
      easyClose = F,
      fade = T
      
    ))
    
  })
  
}

shinyApp(ui, server)
