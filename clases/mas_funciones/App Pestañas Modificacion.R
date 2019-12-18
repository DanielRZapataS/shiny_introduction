library(shiny)
library(wordcloud)
library(RColorBrewer)
library(DT)

####################
###
##
#
# ZONA DE CÓDIGO GENERAL

# Carga de Ficheros

lista_libros <- read.csv("lista_libros.csv", encoding = "UTF-8", stringsAsFactors = F)
libros <- lista_libros$codigo
names(libros) <- lista_libros$Titulo

stop_words_es <- read.table("stopwords-es.txt", encoding = "UTF-8")
stop_words_es <- as.character(stop_words_es$V1)

# Funciones

leer_texto <- function(libro){
  
  if(!(libro %in% libros))
    stop("Libro Desconocido")
  
  texto <- readLines(sprintf("./libros/%s.txt", libro), 
                     encoding = "UTF-8")
  inicio <- lista_libros[lista_libros$codigo == libro,]$inicio
  final <- lista_libros[lista_libros$codigo == libro,]$final
  
  resultado <- list(texto, inicio, final)
  
  return(resultado)
  
}

procesar_palabras <- function(descripcion)
{
  
  descripcion <- gsub("[[:punct:]]", " ", descripcion) # Eliminamos puntuación
  descripcion <- gsub("[0-9]", " ", descripcion) # Eliminamos números
  descripcion <- trimws(descripcion) # Elimina espacios por delante y por detrás
  descripcion <- unlist(strsplit(descripcion, " ")) # Convierto en vector de palabras
  descripcion <- descripcion[descripcion != ""] # elimino los espacios en blanco
  descripcion <- tolower(descripcion) # Paso a minúsculas
  
}

matriz_palabras <- function(libro) {
  
  parametros_texto <- leer_texto(libro)
  
  texto_entero <- parametros_texto[[1]]
  inicio <- parametros_texto[[2]]
  final <- parametros_texto[[3]]
  texto <- texto_entero[c(seq(inicio + 1, final - 1))]
  
  libroCorpus <- procesar_palabras(texto)
  libroCorpus_stopwords <- libroCorpus[!(libroCorpus %in% stop_words_es)]
  
  tabla <- as.data.frame(table(libroCorpus_stopwords), stringsAsFactors = F)
  tabla <- tabla[order(tabla$Freq, decreasing = T),]
  
  total_palabras <- length(libroCorpus)
  palabras_unicas <- length(unique(libroCorpus))
  palabras_especiales <- dim(tabla)[1]
  
  resultado <- list(
    
    total_palabras = total_palabras, #1
    palabras_unicas = palabras_unicas, #2
    palabras_especiales = palabras_especiales, #3
    tabla = tabla, #4
    texto_entero = texto_entero, #5
    inicio = inicio, #6
    final = final #7
    
  )
  
  return(resultado)
}


####################
###
##
#
# INTERFAZ DE USUARIO

ui <- fluidPage(
  title = "Aplicación Libros Básica",
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("seleccion", "Selecciona un libro:",
                  choices = libros),
      actionButton("cambiar", "Cambiar"),
      hr(),
      sliderInput("freq",
                  "Frecuencia Mínima:",
                  min = 1, max = 50, value = 15),
      sliderInput("max",
                  "Máximo Número de Palabras:",
                  min = 1, max = 300, value = 100)
      
    ),
    mainPanel(
      
      tabsetPanel(type = "pills",
        
        tabPanel("Estudio",
                 
                 fluidRow(
                   column(4,
                          h4("Total Palabras:"),
                          h3(textOutput("total_palabras"))
                   ),
                   column(4,
                          h4("Palabras Únicas:"),
                          h3(textOutput("palabras_unicas"))
                   ),
                   column(4,
                          h4("Palabras Estudio:"),
                          h3(textOutput("palabras_especiales"))
                   )
                 ),
                 fluidRow(
                   
                   h3("Nube de Palabras"),
                   plotOutput("nube")
                   
                 )
                 
                 ),
        
        tabPanel("Modificación",
                 
                 fluidRow(
                   column(6,
                          h4("Primera Línea:"),
                          h3(textOutput("primera"))
                   ),
                   column(6,
                          h4("Última Línea:"),
                          h3(textOutput("ultima"))
                   )
                 ),
                 
                 fluidRow(
                   
                   tabsetPanel( type = "tabs",
                                
                                tabPanel("inicio",
                                         
                                         h4("Inicio del Texto"),
                                         dataTableOutput("tabla_inicio")
                                         
                                         ),
                                tabPanel("final",
                                         
                                         h4("Final del Texto"),
                                         dataTableOutput("tabla_final")
                                         
                                         )
                     
                     
                     
                   )
                   
                   
                   
                 )
                 
                 )
        
      )
    
    )
    
  )
  
)


####################
###
##
#
# SERVIDOR

server <- function(input, output){
  
  libroCorpus <- reactive({
    
    input$cambiar
    
    isolate({
      
      withProgress({
        
        setProgress(message = "Procesando Corpus ...")
        matriz_palabras(input$seleccion)
        
      })
      
    })
    
  })
  
  palabras <- reactive({
    libroCorpus()[[4]]
  })
  
  total_palabras <- reactive({
    libroCorpus()[[1]]
  })
  
  palabras_unicas <- reactive({
    libroCorpus()[[2]]
  })
  
  palabras_especiales <- reactive({
    libroCorpus()[[3]]
  })
  
  texto_entero <- reactive({
    libroCorpus()[[5]]
  })
  
  inicio <- reactive({
    libroCorpus()[[6]]
  })
  
  final <- reactive({
    libroCorpus()[[7]]
  })
  
  output$nube <- renderPlot({
    
    v <- palabras()
    wordcloud(words = v$libroCorpus, 
              freq = v$Freq,
              min.freq = input$freq,
              max.words = input$max,
              colors = brewer.pal(8, "Dark2")
    )
    
  })
  
  output$total_palabras <- renderText(total_palabras())
  
  output$palabras_unicas <- renderText(palabras_unicas())
  
  output$palabras_especiales <- renderText(palabras_especiales())
  
  output$primera <- renderText(inicio())
  
  output$ultima <- renderText(final())
  
  output$tabla_inicio <- renderDataTable({
    
   as.data.frame(texto_entero())
    
  })
  
  output$tabla_final <- renderDataTable({
    
    as.data.frame(texto_entero(), stringsAsFactor = F)
    
  })
  
}


####################
###
##
#
# LANZADOR

shinyApp(ui, server)
