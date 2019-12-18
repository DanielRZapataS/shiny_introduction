#### Word cloud ####

##### codigo general ####
# paquetes
library(shiny)
library(wordcloud)
library(RColorBrewer)
library(DT)

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
leer_texto <- function(libro) {
  if (!(libro %in% libros))
    stop("Libro Desconocido")
  texto <-
    readLines(sprintf("./clases/word_cloud/libros/%s.txt", libro),
              encoding = "UTF-8")
  inicio <- lista_libros[lista_libros$codigo == libro,]$inicio
  final <- lista_libros[lista_libros$codigo == libro,]$final
  # texto_entero <- texto
  # texto <- texto[c(seq(inicio + 1, final -1))]
  resultado <- list(texto, inicio, final)
  
  
  return(resultado)
  
}

procesar_palabras <- function(descripcion) {
  descripcion <- gsub("[[:punct:]]", " ", descripcion)
  descripcion <- gsub("[0-9]", " ", descripcion)
  descripcion <- trimws(descripcion)
  descripcion <- unlist(strsplit(descripcion, " "))
  descripcion <- descripcion[descripcion != ""]
  descripcion <- tolower(descripcion)
}
libro <- "quijote"
matriz_palabra <- function(libro) {
  # texto <- leer_texto(libro)
  # for(i in 1:length(texto)){
  #   cat(i, "\n")
  #   if(substr(texto[i], 1, 12) == "*** START OF"){
  #     primera_linea <- i
  #   }
  #   if(substr(texto[i], 1, 10) == "*** END OF"){
  #     ultima_linea <- i
  #   }
  #
  # }
  parametros_texto <- leer_texto(libro)
  texto_entero <- parametros_texto[[1]]
  inicio <- parametros_texto[[2]]
  final <- parametros_texto[[3]]
  texto <- texto_entero[c(seq(inicio + 1, final - 1))]
  
  # librocorpus <- procesar_palabras(texto[c(primera_linea:ultima_linea)])
  librocorpus <- procesar_palabras(texto)
  librocorpus_stopwords <-
    librocorpus[!(librocorpus %in% stop_words)]
  tabla <- as.data.frame(table(librocorpus_stopwords))
  tabla <- tabla[order(tabla$Freq, decreasing = T), ]
  total_palabras <- length(librocorpus)
  palabras_unicas <- length(unique(librocorpus))
  palabras_estudio <- dim(tabla)[1]
  resultado <- list(
    total_palabras = total_palabras,
    palabras_unicas = palabras_unicas,
    palabras_estudio = palabras_estudio,
    tabla = tabla,
    texto_entero = texto_entero,
    inicio = inicio,
    final = final
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
                  mainPanel(tabsetPanel(
                    type = "pills",
                    tabPanel(
                      "Estudio",
                      
                      fluidRow(column(4, h4("Total Palabras:"),
                                      h3(
                                        textOutput("total_palabras")
                                      )),
                               column(
                                 4, h4("Palabras únicas:"),
                                 h3(textOutput("palabras_unicas"))
                               ),
                               column(
                                 4, h4("Palabras estudio:"),
                                 h3(textOutput("palabras_estudio"))
                               )),
                      fluidRow(h3("Nube de Palabras"),
                               plotOutput("nube"))
                    ),
                    
                    tabPanel(
                      "Modificación",
                      fluidRow(column(5, h4("Primera Línea:"),
                                      h3(
                                        textOutput("primera")
                                      )),
                               column(5, h4("Última Línea:"),
                                      h3(
                                        textOutput("ultima")
                                      )),
                               column(4, offset = 1,
                                      actionButton("modificar", "Modificar"))
                               ),
                      fluidRow(tabsetPanel(
                        type = "tabs",
                        tabPanel(
                          "inicio",
                          h4("Inicio del Texto"),
                          dataTableOutput("tabla_inicio")
                        ),
                        tabPanel("final",
                                 h4("Final del Texto"),
                                 dataTableOutput("tabla_final"))
                      ))
                      
                    )
                  ))
                  
                ))

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
  
  parametros_texto <- reactive({
  })
  
  total_palabras <-  reactive({
    librocorpus()[[1]]
  })
  palabras_unicas <-  reactive({
    librocorpus()[[2]]
  })
  palabras_estudio <-  reactive({
    librocorpus()[[3]]
  })
  tabla <-  reactive({
    librocorpus()[[4]]
  })
  texto_entero <- reactive({
    librocorpus()[[5]]
  })
  inicio <- reactive({
    librocorpus()[[6]]
  })
  final <- reactive({
    librocorpus()[[7]]
  })
  
  output$nube <- renderPlot({
    v <- tabla()
    wordcloud(
      v$librocorpus,
      v$Freq,
      min.freq = input$freq,
      max.words = input$max,
      colors = brewer.pal(8, "Dark2")
    )
  })
  
  output$total_palabras <- renderText(total_palabras())
  
  output$palabras_unicas <- renderText(palabras_unicas())
  
  output$palabras_estudio <- renderText(palabras_estudio())
  
  output$primera <- renderText({
    if(is.null(input$tabla_inicio_rows_selected)){
      inicio()
      }else{
        input$tabla_inicio_rows_selected
    }
    })
  
  output$ultima <- renderText({
    if(is.null(input$tabla_final_rows_selected)){
      final()
    }else{
      input$tabla_final_rows_selected
    }
  })
  
  output$tabla_inicio <- renderDataTable({
    datatable(as.data.frame(texto_entero(), stringAsFactor = F),
              escape = FALSE,
              selection = list(mode = 'single', selected = inicio() ),
              options = list(
                searching = T,
                selected = inicio(),
                displayStart = inicio() -5,
                pageLength = 50,
                dom = 't',
                scrolly = "500px"
              ))
  }
  )
  
  output$tabla_final <- renderDataTable({
    datatable(as.data.frame(texto_entero(), stringAsFactor = F),
              escape = FALSE,
              selection = list(mode = 'single', selected = final() ),
              options = list(
                searching = T,
                selected = final(),
                displayStart = final() -5,
                pageLength = 50,
                dom = 't',
                scrolly = "500px"
              ))
  })
  observeEvent(
    input$modificar,
    {
      inicio_mod <- ifelse(is.null(input$tabla_inicio_rows_selected), 
                           inicio(),
                           input$tabla_inicio_rows_selected )
      final_mod <- ifelse(is.null(input$tabla_final_rows_selected), 
                           final(),
                          input$tabla_final_rows_selected )
      lista_libros <-
        read.csv("clases/word_cloud/libro.csv")
      names(lista_libros)[1] <- "titulo"
      libros <-  lista_libros$codigo
      names(libros) <- lista_libros$titulo
      lista_libros[lista_libros$codigo == input$seleccion, ]$inicio <- inicio_mod
      lista_libros[lista_libros$codigo == input$seleccion, ]$final <- final_mod
      write.csv(lista_libros, "clases/word_cloud/lista_libros.csv", fileEncoding = "UTF-8", row.names = FALSE )
    }
    
    
    
  )
}

shinyApp(ui = ui, server = server)
