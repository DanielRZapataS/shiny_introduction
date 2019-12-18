
library (shiny)
library(maps)
library(mapproj)

estados <- readRDS("clases/mapas/counties.rds")
source("clases/mapas/helpers.R")

# UI -------------------------

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
                                 "Porcentaje Asiaticos"),  #Para evitar problemas, quitamos la tilde
                  selected = "Porcentaje Blanco"),
      sliderInput("rango",
                  label = "Rango de Interés:",
                  min = 0,
                  max = 100,
                  value = c(0,100))
    ),
    mainPanel(
      
      #Establecer un output de tipo plot para mostrar el mapa
      plotOutput("mapa")
      
    )
  )
  
)



# SERVER -------------------------

server <- function(input, output){
  
  #Tenemos que renderizar el mapa para presentarlo en el mainPanel
  
  output$mapa <- renderPlot({
    
    #browser()
   
    datos <- switch(input$var,
                    "Porcentaje Blancos" = estados$white,
                    "Porcentaje Negros" = estados$black,
                    "Porcentaje Latinos" = estados$hispanic,
                    "Porcentaje Asiaticos" = estados$asian)
    color <- switch(input$var,
                    "Porcentaje Blancos" = "darkgreen",
                    "Porcentaje Negros" = "black",
                    "Porcentaje Latinos" = "darkorange",
                    "Porcentaje Asiaticos" = "darkviolet")
    titulo <- switch(input$var,
                     "Porcentaje Blancos" = "% Blancos",
                     "Porcentaje Negros" = "% Negros",
                     "Porcentaje Latinos" = "% Latinos",
                     "Porcentaje Asiaticos" = "% Asiaticos")
    min <- input$rango[1] # primera entrada del slider (rango)
    max <- input$rango[2] # segunda entrada del slider (rango)
    
    percent_map(datos, color, titulo, min, max)
    
  })
  
}

# LLAMADA shiny -------------------------

shinyApp(ui = ui, server = server)
