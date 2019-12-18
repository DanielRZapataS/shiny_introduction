library(shiny)

ui <- fluidPage(
  
  titlePanel("Widget Básicos"),
  
  fluidRow(
    column(3,
           h3("Botones"),
           actionButton("accion", "Acción"),
           br(),
           br(),
           submitButton("Ir!", icon = icon("calendar"))
           ),
    column(3,
           h3("Checkbox"),
           checkboxInput("checkbox", "Opción A", value = T)
    ),
    column(3,
           checkboxGroupInput("checkgroup",
                              h3("CheckGroup"),
                              choices = list("Opción 1" = 1,
                                             "Opción 2" = 2,
                                             "Opción 3" = 3),
                              selected = 1)
    ),
    column(3,
           dateInput("fecha",
                     h3("Fecha"),
                     value = "2018-01-01")
           
    )
  ),
  
  fluidRow(
    column(3,
           dateRangeInput("fechas",
                          h3("Rango de Fechas"),
                          language = "es",
                          format = "dd.mm.yyyy",
                          start = "2018-02-01",
                          end = "2018-12-31",
                          separator = " a ")
           ),
    column(3,
           fileInput("fichero", 
                     h3("Ficheros"),
                     buttonLabel = "fichero",
                     placeholder = "selecciona un fichero",
                     multiple = T)
          ),
    column(3,
           h3("Texto de Ayuda"),
           helpText("Nota: el texto de ayuda no es un widget realmente,",
                    "pero permite añadir texto de una manera sencilla",
                    "para acompañar otros widgets")
    ),
    column(3,
           numericInput("numero",
                        h3("Input Numérico"),
                        min = 61,
                        max = 71,
                        step = 2,
                        value = 69)
          )
  ),
  
  fluidRow(
    column(3, 
           radioButtons("radio",
                        h3("Radio"),
                        choices = list("Opción 1" = 1,
                                       "Opción 2" = 2,
                                       "Opción 3" = 3),
                        selected = 1)
           ),
    column(3, 
           selectInput("seleccion", 
                       h3("Selección"),
                       choices = list("Opción A" = 1,
                                      "Opción B" = 3,
                                      "Opción C" = 2),
                       selected = 3
                       )
           ),
    column(3, 
           sliderInput("slider1", 
                       h3("Sliders"),
                       min = 0,
                       max = 100,
                       step = 10,
                       value = 50),
           br(),
           sliderInput("slider2", 
                       "",
                       min = 0,
                       max = 100,
                       round = T,
                       value = c(25,75))
           ),
    column(3, 
           textInput("texto",
                     h3("Input Texto"),
                     placeholder = "Introduce un texto...")
           )
  )
  
  
)


server <- function(input, output){}

shinyApp(ui = ui, server = server)

