
library(shiny)

ui <- fluidPage(
  
  titlePanel(" Ejemplo Menú de Navegación (superior) "),
  
  navbarPage("Mi App",

    tabPanel("Componente 1", h4("COMPONENTE 1")),
    tabPanel("Componente 2", h4("COMPONENTE 2")),
    
    navbarMenu("Más",
      
      tabPanel("Componente 3", h4("COMPONENTE 3")),
      tabPanel("Componente 4", h4("COMPONENTE 4")),
      tabPanel("Componente 5", h4("COMPONENTE 5"))
      
    )
    
  )
  
)


server <- function(input, output){}

shinyApp(ui, server)