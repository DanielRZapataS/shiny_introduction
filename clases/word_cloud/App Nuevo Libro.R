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

# archivos

# lista_libros <- read.csv("clases/word_cloud/libro.csv")
# names(lista_libros)[1] <- "titulo"
# libros <- lista_libros$codigo
# names(libros) <- lista_libros$titulo
lista_libros <-
  read.csv("clases/word_cloud/lista_libros.csv")
libros <-  lista_libros$codigo
names(libros) <- lista_libros$titulo

stop_words_es <-
  read.csv(
    "clases/word_cloud/stopwords-es.txt",
    encoding = "UTF-8",
    stringsAsFactors = F
  )
stop_words_es <- as.character(stop_worlds_es$X0)

# Funciones

leer_texto <- function(libro) {
  if (!(libro %in% libros))
    stop("Libro Desconocido")
  
  texto <-
    readLines(sprintf("./clases/word_cloud/libros/%s.txt", libro),
              encoding = "UTF-8")
  inicio <- v$lista_libros[v$lista_libros$codigo == libro,]$inicio
  final <- v$lista_libros[v$lista_libros$codigo == libro,]$final
  
  resultado <- list(texto, inicio, final)
  
  return(resultado)
  
}

procesar_palabras <- function(descripcion)
{
  descripcion <-
    gsub("[[:punct:]]", " ", descripcion) # Eliminamos puntuación
  descripcion <-
    gsub("[0-9]", " ", descripcion) # Eliminamos números
  descripcion <-
    trimws(descripcion) # Elimina espacios por delante y por detrás
  descripcion <-
    unlist(strsplit(descripcion, " ")) # Convierto en vector de palabras
  descripcion <-
    descripcion[descripcion != ""] # elimino los espacios en blanco
  descripcion <- tolower(descripcion) # Paso a minúsculas
  
}

matriz_palabras <- function(libro) {
  parametros_texto <- leer_texto(libro)
  
  texto_entero <- parametros_texto[[1]]
  inicio <- parametros_texto[[2]]
  final <- parametros_texto[[3]]
  texto <- texto_entero[c(seq(inicio + 1, final - 1))]
  
  libroCorpus <- procesar_palabras(texto)
  libroCorpus_stopwords <-
    libroCorpus[!(libroCorpus %in% stop_words_es)]
  
  tabla <-
    as.data.frame(table(libroCorpus_stopwords), stringsAsFactors = F)
  tabla <- tabla[order(tabla$Freq, decreasing = T),]
  
  total_palabras <- length(libroCorpus)
  palabras_unicas <- length(unique(libroCorpus))
  palabras_especiales <- dim(tabla)[1]
  
  resultado <- list(
    total_palabras = total_palabras,
    #1
    palabras_unicas = palabras_unicas,
    #2
    palabras_especiales = palabras_especiales,
    #3
    tabla = tabla,
    #4
    texto_entero = texto_entero,
    #5
    inicio = inicio,
    #6
    final = final #7
    
  )
  
  return(resultado)
}

v <- reactiveValues(
  paginacion_inicial = 5,
  paginacion_final = 5,
  lista_libros = lista_libros
)


####################
###
##
#
# INTERFAZ DE USUARIO

ui <- fluidPage(title = "Aplicación Libros Básica",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("seleccion", "Selecciona un libro:",
                                choices = libros),
                    actionButton("cambiar", "Cambiar"),
                    hr(),
                    sliderInput(
                      "freq",
                      "Frecuencia Mínima:",
                      min = 1,
                      max = 50,
                      value = 15
                    ),
                    sliderInput(
                      "max",
                      "Máximo Número de Palabras:",
                      min = 1,
                      max = 300,
                      value = 100
                    )
                    
                  ),
                  mainPanel(tabsetPanel(
                    type = "pills",
                    
                    tabPanel(
                      "Estudio",
                      
                      fluidRow(column(4,
                                      h4("Total Palabras:"),
                                      h3(
                                        textOutput("total_palabras")
                                      )),
                               column(
                                 4,
                                 h4("Palabras Únicas:"),
                                 h3(textOutput("palabras_unicas"))
                               ),
                               column(
                                 4,
                                 h4("Palabras Estudio:"),
                                 h3(textOutput("palabras_especiales"))
                               )),
                      fluidRow(h3("Nube de Palabras"),
                               plotOutput("nube"))
                      
                    ),
                    
                    tabPanel(
                      "Modificación",
                      
                      fluidRow(
                        column(4,
                               h4("Primera Línea:"),
                               h3(textOutput("primera"))),
                        column(4,
                               h4("Última Línea:"),
                               h3(textOutput("ultima"))),
                        column(3, offset = 1,
                               actionButton("modificar", "Modificar"))
                      ),
                      
                      fluidRow(tabsetPanel(
                        type = "tabs",
                        
                        tabPanel(
                          "inicio",
                          
                          fluidRow(
                            column(4, h4("Inicio del Texto")),
                            column(1, offset = 5,
                                   actionButton("pag_inicial_up", "^")),
                            column(1,
                                   actionButton("pag_inicial_down", "v"))
                          ),
                          fluidRow(dataTableOutput("tabla_inicio"))
                          
                        ),
                        tabPanel(
                          "final",
                          
                          fluidRow(
                            column(4, h4("Final del Texto")),
                            column(1, offset = 5,
                                   actionButton("pag_final_up", "^")),
                            column(1,
                                   actionButton("pag_final_down", "v"))
                          ),
                          fluidRow(dataTableOutput("tabla_final"))
                          
                        )
                        
                        
                        
                      ))
                      
                    ),
                    tabPanel(
                      "Nuevo libro",
                      fluidRow(
                        column(
                          4,
                          fileInput(
                            "fichero",
                            "Selecciona un fichero:",
                            buttonLabel = "Explorar",
                            placeholder = "Selecciona un fichero de libro"
                          )
                        ),
                        column(4,
                               textInput("titulo",
                                         "Introduce el título del Libro")),
                        column(4,
                               uiOutput("UIcodigo"))
                      ),
                      fluidRow(column(
                        1, offset = 10,
                        actionButton("guardar", "Guardar")
                      ))
                    )
                    
                  ))
                  
                ))


####################
###
##
#
# SERVIDOR

server <- function(input, output) {
  ####
  ##
  #
  # Pestaña Estudio
  
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
    wordcloud(
      words = v$libroCorpus,
      freq = v$Freq,
      min.freq = input$freq,
      max.words = input$max,
      colors = brewer.pal(8, "Dark2")
    )
    
  })
  
  output$total_palabras <- renderText(total_palabras())
  
  output$palabras_unicas <- renderText(palabras_unicas())
  
  output$palabras_especiales <- renderText(palabras_especiales())
  
  ####
  ##
  #
  # Pestaña Modificación
  
  output$primera <- renderText({
    if (is.null(input$tabla_inicio_rows_selected)) {
      inicio()
    } else {
      input$tabla_inicio_rows_selected
    }
    
  })
  
  output$ultima <- renderText({
    if (is.null(input$tabla_final_rows_selected)) {
      final()
    } else {
      input$tabla_final_rows_selected
    }
  })
  
  output$tabla_inicio <- renderDataTable({
    datatable(
      as.data.frame(texto_entero(), stringsAsFactor = F),
      escape = FALSE,
      selection = list(mode = "single", selected = inicio()),
      options = list(
        searching = T,
        displayStart = inicio() - v$paginacion_inicial,
        pageLength = 50,
        dom = 't',
        scrollY = "500px"
      )
    )
    
  })
  
  observeEvent(input$pag_inicial_up,
               {
                 v$paginacion_inicial <- v$paginacion_inicial + 20
               })
  
  observeEvent(input$pag_inicial_down,
               {
                 v$paginacion_inicial <- v$paginacion_inicial - 20
               })
  
  output$tabla_final <- renderDataTable({
    datatable(
      as.data.frame(texto_entero(), stringsAsFactor = F),
      escape = FALSE,
      selection = list(mode = "single", selected = final()),
      options = list(
        searching = T,
        displayStart = final() - v$paginacion_final,
        pageLength = 50,
        dom = 't',
        scrollY = "500px"
      )
    )
    
  })
  
  observeEvent(input$pag_final_up,
               {
                 v$paginacion_final <- v$paginacion_final + 20
               })
  
  observeEvent(input$pag_final_down,
               {
                 v$paginacion_final <- v$paginacion_final - 20
               })
  
  observeEvent(input$modificar,
               {
                 inicio_mod <- ifelse(
                   is.null(input$tabla_inicio_rows_selected),
                   inicio(),
                   input$tabla_inicio_rows_selected
                 )
                 final_mod <- ifelse(
                   is.null(input$tabla_final_rows_selected),
                   final(),
                   input$tabla_final_rows_selected
                 )
                 v$lista_libros <-
                   read.csv("lista_libros.csv",
                            encoding = "UTF-8",
                            stringsAsFactors = F)
                 v$lista_libros[v$lista_libros$codigo == input$seleccion,]$inicio <-
                   inicio_mod
                 v$lista_libros[v$lista_libros$codigo == input$seleccion,]$final <-
                   final_mod
                 write.csv(
                   v$lista_libros,
                   "lista_libros.csv",
                   fileEncoding = "UTF-8",
                   row.names = FALSE
                 )
                 showModal(
                   modalDialog(
                     title = "Líneas Modificadas",
                     h6(
                       "Recuerda volver a presionar el botón 'Cambiar' para aplicar el resultado"
                     ),
                     size = "m",
                     easyClose = T,
                     fade = T,
                     footer = tagList(modalButton(label = "Cerrar"))
                     
                   )
                 )
                 
               })
  
  ####
  ##
  #
  # Pestaña Nuevo Libro
  
  # Construcción del combo de código
  output$UIcodigo <- renderUI({
    titulo <- unlist(strsplit(input$titulo, " "))
    titulo <- tolower(titulo)
    palabra_seleccionada <-
      titulo[which(nchar(titulo) == max(nchar(titulo)))]
    
    selectInput(
      "codigo",
      "Escoge un código para el libro",
      choices = titulo,
      selected = palabra_seleccionada
    )
    
  })
  
  observeEvent(input$guardar,
               {
                 libro_nuevo <- readLines(input$fichero$datapath,
                                          encoding = "UTF-8")
                 writeLines(
                   libro_nuevo,
                   paste("./clases/word_cloud/libros/", input$codigo, ".txt", sep = "")
                 )
               })
  
  
  
}


####################
###
##
#
# LANZADOR

shinyApp(ui, server)
