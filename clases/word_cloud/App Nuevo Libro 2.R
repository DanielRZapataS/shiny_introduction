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
  libros <- v$lista_libros$codigo
  
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

inicio_final <- function(texto) {
  primera_linea <- 0
  ultima_linea <- 0
  
  
  for (i in 1:length(texto)) {
    if (substr(texto[i], 1, 12) == "*** START OF") {
      primera_linea <- i
      #break()
    }
    
    if (substr(texto[i], 1, 10) == "*** END OF") {
      ultima_linea <- i
      break()
    }
    
  }
  
  resultado <- list(primera_linea, ultima_linea)
  
  return(resultado)
  
}

procesar_nuevo_libro <- function(fichero) {
  if (is.null(fichero)) {
    return(list(0, 0, 0, ""))
    
  }
  
  texto <- readLines(fichero$datapath)
  
  lineas <- length(texto)
  
  lineas_inicio_final <- inicio_final(texto)
  
  inicio <- lineas_inicio_final[[1]]
  final <- lineas_inicio_final[[2]]
  
  resultado <- list(lineas, # Elemento 1: Total líneas nuevo libro
                    inicio, # Elemento 2: Línea de inicio
                    final,  # Elemento 3: Línea de final
                    texto)  # Elemento 4: Todas las líneas del libro
  
  return(resultado)
  
}

# Variables Reactivas

v <- reactiveValues(
  paginacion_inicial = 5,
  paginacion_final = 5,
  paginacion_nuevo = 5,
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
                  mainPanel(
                    tabsetPanel(
                      id = "pestanas",
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
                                     actionButton(
                                       "pag_inicial_up", "", icon = icon("arrow-up", lib = "glyphicon")
                                     )),
                              column(1,
                                     actionButton(
                                       "pag_inicial_down", "", icon = icon("arrow-down", lib = "glyphicon")
                                     ))
                            ),
                            fluidRow(dataTableOutput("tabla_inicio"))
                            
                          ),
                          tabPanel(
                            "final",
                            
                            fluidRow(
                              column(4, h4("Final del Texto")),
                              column(1, offset = 5,
                                     actionButton(
                                       "pag_final_up", "", icon = icon("arrow-up", lib = "glyphicon")
                                     )),
                              column(1,
                                     actionButton(
                                       "pag_final_down", "", icon = icon("arrow-down", lib = "glyphicon")
                                     ))
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
                        fluidRow(
                          column(
                            8,
                            h5("Selecciona un fichero, escribe un título y selecciona código."),
                            h5(
                              "Haz clic primero en 'Procesar' y cuando aparezca el check, en 'Guardar'"
                            )
                          ),
                          column(1,
                                 actionButton("procesar", "Procesar")),
                          column(1, offset = 1,
                                 actionButton(
                                   "guardar", "Guardar", icon = icon("remove", lib = 'glyphicon')
                                 ))
                        ),
                        fluidRow(
                          column(
                            6,
                            h4("Características"),
                            br(),
                            h5("Título:"),
                            textOutput("titulo_nuevo_libro"),
                            h5("Código:"),
                            textOutput("codigo_nuevo_libro"),
                            hr(),
                            h5("Número de Líneas:"),
                            textOutput("lineas_nuevo_libro"),
                            h5("Primera Línea"),
                            textOutput("inicio_nuevo_libro"),
                            h5("Última Línea:"),
                            textOutput("final_nuevo_libro")
                          ),
                          column(
                            6,
                            fluidRow(
                              column(6, h4("Muestra")),
                              column(1,
                                     actionButton(
                                       "pag_nuevo_up", "", icon = icon("arrow-up", lib = "glyphicon")
                                     )),
                              column(1, offset = 1,
                                     actionButton(
                                       "pag_nuevo_down", "", icon = icon("arrow-down", lib = "glyphicon")
                                     ))
                            ),
                            fluidRow(br(),
                                     textOutput("texto_nuevo_libro"))
                          )
                        )
                      )
                      
                    )
                    
                  )
                  
                ))


####################
###
##
#
# SERVIDOR

server <- function(input, output, session) {
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
      as.data.frame(texto_entero()),
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
                 writeLines(
                   texto_nuevo(),
                   paste("./clases/word_cloud/libros/", input$codigo, ".txt", sep = "")
                 )
                 nuevo_libro <- data.frame(
                   Titulo = input$titulo,
                   codigo = input$codigo,
                   inicio = inicio_nuevo(),
                   final = final_nuevo()
                 )
                 v$lista_libros <-
                   rbind(v$lista_libros, nuevo_libro)
                 write.csv(
                   lista_libros,
                   "lista_libros.csv",
                   row.names = F,
                   fileEncoding = "UTF-8"
                 )
                 
                 libros <- v$lista_libros$codigo
                 names(libros) <- v$lista_libros$Titulo
                 
                 libro_seleccionado <- input$codigo
                 
                 updateSelectInput(
                   session = session,
                   inputId = "seleccion",
                   choices = libros,
                   selected = libro_seleccionado
                 )
                 
                 updateTabsetPanel(session = session,
                                   inputId = "pestanas",
                                   selected = "Estudio")
                 
                 
               })
  
  libro_nuevo <- reactive({
    #browser()
    input$procesar
    
    isolate({
      withProgress({
        setProgress(message = "Procesando Nuevo Libro ...")
        resultado <- procesar_nuevo_libro(input$fichero)
        
        if (resultado[[1]] != 0) {
          updateActionButton(
            session = session,
            inputId = "guardar",
            icon = icon("ok", lib = "glyphicon")
          )
        }
        
        return(resultado)
        
      })
      
    })
    
  })
  
  lineas_nuevo <- reactive({
    libro_nuevo()[[1]]
  })
  
  inicio_nuevo <- reactive({
    libro_nuevo()[[2]]
  })
  
  final_nuevo <- reactive({
    libro_nuevo()[[3]]
  })
  
  texto_nuevo <- reactive({
    libro_nuevo()[[4]]
  })
  
  
  output$titulo_nuevo_libro <- renderText({
    input$titulo
  })
  
  output$codigo_nuevo_libro <- renderText({
    input$codigo
  })
  
  output$lineas_nuevo_libro <- renderText({
    lineas_nuevo()
  })
  
  output$inicio_nuevo_libro  <- renderText({
    inicio_nuevo()
  })
  
  output$final_nuevo_libro  <- renderText({
    final_nuevo()
  })
  
  output$texto_nuevo_libro <- renderText({
    #browser()
    if (lineas_nuevo() == 0) {
      return ("")
    } else {
      texto_nuevo()[c(
        seq(
          inicio_nuevo() - v$paginacion_nuevo,
          inicio_nuevo() - v$paginacion_nuevo + 15
        )
      )]
    }
  })
  
  observeEvent(input$pag_nuevo_up,
               {
                 v$paginacion_nuevo <- v$paginacion_nuevo + 20
               })
  
  observeEvent(input$pag_nuevo_down,
               {
                 v$paginacion_nuevo <- v$paginacion_nuevo - 20
               })
  
}


####################
###
##
#
# LANZADOR

shinyApp(ui, server)
