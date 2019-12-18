library (shiny)

ui <- fluidPage(
  
  titlePanel("censoVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText( "Crear un mapa demográfico con
                información del Censo de EE.UU. de 2010."),
      selectInput("var",
                  label = "Elige la variable a visualizar",
                  choices = list("Porcentaje Blancos",
                                 "Porcentaje Negros",
                                 "Porcentaje Latinos",
                                 "Porcentaje Asiáticos"),
                  selected = "Porcentaje Blanco"),
      sliderInput("rango",
                  label = "Rango de Interés:",
                  min = 0,
                  max = 100,
                  value = c(0,100))
    ),
    mainPanel()
  )
  
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)