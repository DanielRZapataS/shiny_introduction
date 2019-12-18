library(shiny)
library(wordcloud)
library(RColorBrewer)
library(DT)
library(shinydashboard)

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
  inicio <- v$lista_libros[v$lista_libros$codigo == libro, ]$inicio
  final <- v$lista_libros[v$lista_libros$codigo == libro, ]$final
  
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
  tabla <- tabla[order(tabla$Freq, decreasing = T), ]
  
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

ui <- dashboardPage(
  dashboardHeader(title = "Libros Proyecto Gutenberg",
                  titleWidth = 300),
  dashboardSidebar(
    width = 300,
    
    sidebarMenu(
      id = "pestanas",
      
      menuItem(
        "Estudio",
        tabName = "estudio",
        icon = icon("stats", lib = "glyphicon"),
        selected = T
      ),
      menuItem(
        "Modificación",
        tabName = "modificacion",
        icon = icon("pencil", lib = "glyphicon")
      ),
      menuItem(
        "Nuevo Libro",
        tabName = "nuevo",
        icon = icon("book", lib = "glyphicon")
      )
      
    ),
    
    hr(),
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
  dashboardBody(fluidPage(tabItems(
    tabItem(
      tabName = "estudio",
      
      fluidRow(
        valueBoxOutput("total_palabras", width = 4),
        valueBoxOutput("palabras_unicas", width = 4),
        valueBoxOutput("palabras_especiales", width = 4)
        
      ),
      fluidRow(h3("Nube de Palabras"),
               plotOutput("nube"))
      
    ),
    
    tabItem(
      tabName = "modificacion",
      
      fluidRow(
        column(4,
               valueBoxOutput("primera", width = 12)),
        column(4,
               valueBoxOutput("ultima", width = 12)),
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
    tabItem(
      tabName = "nuevo",
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
          valueBoxOutput("lineas_nuevo_libro", width = 12),
          valueBoxOutput("inicio_nuevo_libro", width = 12),
          valueBoxOutput("final_nuevo_libro", width = 12)
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
    
  )))
  
)


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
  
  output$total_palabras <- renderValueBox({
    valueBox(
      value = total_palabras(),
      subtitle = "Total Palabras",
      icon = icon("align-justify", lib = "glyphicon"),
      color = "blue"
    )
    
  })
  
  output$palabras_unicas <- renderValueBox({
    valueBox(
      value = palabras_unicas(),
      subtitle = "Palabras Únicas",
      icon = icon("italic", lib = "glyphicon"),
      color = "olive"
    )
    
  })
  
  output$palabras_especiales <- renderValueBox({
    valueBox(
      value = palabras_especiales(),
      subtitle = "Palabras Especiales",
      icon = icon("bold", lib = "glyphicon"),
      color = "light-blue"
    )
    
  })
  
  ####
  ##
  #
  # Pestaña Modificación
  
  output$primera <- renderValueBox({
    valueBox(
      value = ifelse(
        is.null(input$tabla_inicio_rows_selected),
        inicio(),
        input$tabla_inicio_rows_selected
      ),
      subtitle = "Primera Línea",
      icon = icon("align-left", lib = "glyphicon"),
      color = "orange"
    )
    
  })
  
  output$ultima <- renderValueBox({
    valueBox(
      value = ifelse(
        is.null(input$tabla_final_rows_selected),
        final(),
        input$tabla_final_rows_selected
      ),
      subtitle = "Última Línea",
      icon = icon("align-right", lib = "glyphicon"),
      color = "green"
    )
    
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
                 v$lista_libros[v$lista_libros$codigo == input$seleccion, ]$inicio <-
                   inicio_mod
                 v$lista_libros[v$lista_libros$codigo == input$seleccion, ]$final <-
                   final_mod
                 write.csv(
                   v$lista_libros,
                   "clases/word_cloud/lista_libros.csv",
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
                 writeLines(texto_nuevo(),
                            paste("./libros/", input$codigo, ".txt", sep = ""))
                 nuevo_libro <- data.frame(
                   Titulo = input$titulo,
                   codigo = input$codigo,
                   inicio = inicio_nuevo(),
                   final = final_nuevo()
                 )
                 v$lista_libros <- rbind(v$lista_libros, nuevo_libro)
                 write.csv(
                   lista_libros,
                   "./clases/word_cloud/libros/lista_libros.csv",
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
                 
                 updateTabItems(session = session,
                                inputId = "pestanas",
                                selected = "estudio")
                 
                 
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
  
  output$lineas_nuevo_libro <- renderValueBox({
    valueBox(
      value = lineas_nuevo(),
      subtitle = "Total Líneas",
      icon = icon("align-justify", lib = "glyphicon"),
      color = "fuchsia"
    )
    
  })
  
  output$inicio_nuevo_libro  <- renderValueBox({
    valueBox(
      value = inicio_nuevo(),
      subtitle = "Primera Línea",
      icon = icon("align-left", lib = "glyphicon"),
      color = "purple"
    )
    
  })
  
  output$final_nuevo_libro  <- renderValueBox({
    valueBox(
      value = final_nuevo(),
      subtitle = "Última Línea",
      icon = icon("align-right", lib = "glyphicon"),
      color = "maroon"
    )
    
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
