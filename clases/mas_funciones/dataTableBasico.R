# La librería DT se tenía que cargar antes de la versión 1.10.2 de Shiny
# Pero desde hace años, shiny lo contempla en su referencia
# library(DT)

library(shiny)

ui <- basicPage(
  
  h2("Los datos de mtcars"),
  dataTableOutput("tabla")
  
)

server <- function(input, output) {
  
  output$tabla <- renderDataTable({
    
    mtcars
    
  })
  
}

shinyApp(ui, server)