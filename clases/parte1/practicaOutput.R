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
    mainPanel(
      
      #Establecer un output de texto con el valor de la variable var
      textOutput("var_seleccionada"),
      br(),
      textOutput("rango_seleccionado")
      
    )
  )
  
)

server <- function(input, output){
  
  #Tenemos que renderizar la variable de texto var_seleccionada
  output$var_seleccionada <- renderText(
    
    paste("Has seleccionado:", input$var)
    
  )
  
  output$rango_seleccionado <- renderText(
    
    paste("Has seleccionado desde", input$rango[1], "hasta", input$rango[2])
    
  )
  
}

shinyApp(ui = ui, server = server)