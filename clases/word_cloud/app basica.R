#### Word cloud ####

##### codigo general ####
# paquetes
library(shiny)
library(wordcloud)
library(RColorBrewer)

# archivos
lista_libros <-
  read.csv("clases/word_cloud/libro.csv")
stop_words <-
  read.csv(
    "clases/word_cloud/stopwords-es.txt",
    encoding = "UTF-8",
    stringsAsFactors = F
  )
stop_words <- as.character(stop_words$X0)

names(lista_libros)[1] <- "titulo"
libros <-  lista_libros$codigo
names(libros) <- lista_libros$titulo

# funciones
leer_texto <- function(libro){
  if(!(libro %in% libros))
    stop("Libro Desconocido")
  texto <-
    readLines(sprintf("./clases/word_cloud/libros/%s.txt", libro),
              encoding = "UTF-8")
  return(texto)
  
}

procesar_palabras <- function(descripcion){
  descripcion <- gsub("[[:punct:]]", " ", descripcion)
  descripcion <- gsub("[0-9]", " ", descripcion)
  descripcion <- trimws(descripcion)
  descripcion <- unlist(strsplit(descripcion, " "))
  descripcion <- descripcion[descripcion != ""]
  descripcion <- tolower(descripcion)
}
libro <- "regeneta"
matriz_palabra <- function(libro){
  texto <- leer_texto(libro)
  for(i in 1:length(texto)){
    cat(i, "\n")
    if(substr(texto[i], 1, 12) == "*** START OF"){
      primera_linea <- i
    }
    if(substr(texto[i], 1, 10) == "*** END OF"){
      ultima_linea <- i
    }
    
  }


  
  librocorpus <- procesar_palabras(texto[c(primera_linea:ultima_linea)])
  librocorpus_stopwords <- librocorpus[!(librocorpus %in% stop_words)]
  tabla <- as.data.frame(table(librocorpus_stopwords))
  tabla <- tabla[order(tabla$Freq, decreasing = T),]
  total_palabras <- length(librocorpus)
  palabras_unicas <- length(unique(librocorpus))
  palabras_estudio <- dim(tabla)[1]
  resultado <- list(
    total_palabras = total_palabras,
    palabras_unicas = palabras_unicas,
    palabras_estudio = palabras_estudio, 
    tabla = tabla
  )
  return(resultado)
  
}



##### UI #####
ui <- fluidPage(title = "Aplicación Libros Básica",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("seleccion", "Selecciona un libro:", 
                                choices = libros),
                    actionButton("cambiar", "Cambiar"),
                    hr(),
                    sliderInput(
                      "freq",
                      "Frequencia mínima:",
                      min = 1,
                      max = 50,
                      value = 15
                    ),
                    sliderInput(
                      "max",
                      "Máximo número de palabras:",
                      min = 1,
                      max = 300,
                      value = 100
                    )
                    
                  ), 
                  mainPanel(
                    
                    fluidRow(
                      column(4, h4("Total Palabras:"),
                             h3(textOutput("total_palabras"))
                             ),
                      column(4, h4("Palabras únicas:"),
                             h3(textOutput("palabras_unicas"))
                      ),
                      column(4, h4("Palabras estudio:"),
                             h3(textOutput("palabras_estudio"))
                      )
                    ),
                    fluidRow(
                      h3("Nube de Palabras"),
                      plotOutput("nube") 
                      
                    )
                  )

                )
                )

##### SERVER ####
server <- function(input, output, session) {
  librocorpus <- reactive({
    input$cambiar
    
    isolate({
      withProgress({
        setProgress(message = "Procesando corpus...")
        matriz_palabra(input$seleccion)
        
      })

    })
    
  })
  
  total_palabras <-  reactive({librocorpus()[[1]]})
  palabras_unicas <-  reactive({librocorpus()[[2]]})
  palabras_estudio <-  reactive({librocorpus()[[3]]}) 
  tabla <-  reactive({librocorpus()[[4]]})
  
  output$nube <- renderPlot({
    v <- tabla()
    wordcloud(v$librocorpus, 
              v$Freq,
              min.freq = input$freq,
              max.words = input$max,
              colors = brewer.pal(8, "Dark2"))
  })
  
  output$total_palabras <- renderText(total_palabras())
  output$palabras_unicas <- renderText(palabras_unicas())
  output$palabras_estudio <- renderText(palabras_estudio())
}

shinyApp(ui = ui, server = server)

