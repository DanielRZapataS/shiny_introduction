library(shiny)

# preprocesado 

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automático", "Manual"))

# Interfaz de usuario
ui <- pageWithSidebar(
  
  # titulo de la App
  headerPanel(" Millas por Galón"),
  
  # creamos un sidebar panel para las entradas 
  sidebarPanel(
    
    #  Input: seleccionar variable a visualizar 
    selectInput("var", "variable:", 
                choice = c("Cilindros" = "cyl",
                           "Trasmisores" = "am",
                           "Motores" = "gear")),
    # check box
    checkboxInput("outliers", "Mostrar los outliers", TRUE)
  ),
  
  # el main panel visualizará los resultados 
  mainPanel(
    
    # output 1: texto formateado de la variable output$texto
h3(textOutput(("texto"))),    
  # output2: boxplot de la variabble 
  plotOutput("mpgPlot")

  )
)

# servidor 
server <- function(input, output){
  
  # calcular el texto de la formula 
  # es una expresión reactiva  que generará dos vartiables de salida 
  # output texto 
  formulaTexto <- reactive({
    paste("mpg ~ ", input$var) 
  }
    )
  # el output del titulo es la variable outputtexto
  output$texto  <- renderText({
    
    formulaTexto()
  
  })
  
  # generar plot
  # excluir outliers si se desea 
  output$mpgPlot <- renderPlot(({
    boxplot(as.formula(formulaTexto()),
            data = mpgData, 
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  }))
}


# lanzar la app 

shinyApp(ui = ui, server = server)
