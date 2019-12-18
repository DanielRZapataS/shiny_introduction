install.packages("DT")
library(DT)
library(shiny)
library(ggplot2) #Contiene el set de datos diamonds

ui <- fluidPage(
  
  title = "Ejemplos de DataTables",
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(
        
        condition = 'input.dataset === "diamonds"',
        checkboxGroupInput("variables", "Columnas del dataset diamond que mostrar:",
                           names(diamonds), selected = names(diamonds))
        
      ),
      conditionalPanel(
        
        condition = 'input.dataset === "mtcars"',
        helpText("Haz click en el encabezado de la columna para ordenarla.")
        
      ),
      conditionalPanel(
        
        condition = 'input.dataset === "iris"',
        helpText("Muestra 5 filas por defecto.")
        
      ),
      conditionalPanel(
        
        condition = 'input.dataset === "faithful"',
        verbatimTextOutput("seleccionada")
        
        
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        id = 'dataset',
        tabPanel("diamonds", dataTableOutput("tabla1")),
        tabPanel("mtcars", dataTableOutput("tabla2")),
        tabPanel("iris", dataTableOutput("tabla3")),
        tabPanel("faithful", dataTableOutput("tabla4"))
        
      )
      
    )
    
  )
  
)


server <- function(input, output) {
  
  # En la primera pestaña: selecciona las columnas
  diamonds2 <- diamonds[sample(nrow(diamonds), 1000),]
  
  output$tabla1 <- renderDataTable({
    
    datatable(diamonds2[ ,input$variables, drop = F])
    
  })
  
  output$tabla2 <- renderDataTable({
    
    datatable(mtcars, options = list(orderClasses = F))
    
  })
  
  output$tabla3 <- renderDataTable({
    
    datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
  })
  
  output$tabla4 <- renderDataTable({
    
    faithful
    
  },
  escape = FALSE,
  selection = list(mode = "single", selected = 8),
  options = list(searching = T,
                 displayStart = 5,
                 pageLength = 100,
                 dom = 't',
                 scrollY = "500px")
  )
  
  output$seleccionada <- renderText({
    
    paste("La linea seleccionada es:\n",
          input$tabla4_rows_selected, "\n",
          "Erupciones:", faithful$eruptions[input$tabla4_rows_selected], "\n",
          "Tiempo Espera:", faithful$waiting[input$tabla4_rows_selected], "\n")
    
  })
  
  
  
}

shinyApp(ui, server)
