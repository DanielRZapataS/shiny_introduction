library(shiny)

# ui <- fluidPage(
#   
#   titlePanel("Título de la App"),
#   
#   sidebarLayout(
#     position = "right",
#     sidebarPanel("panel lateral", width = 4),
#     mainPanel("panel principal")
#     
#   )
#   
#   
# )

# h1("Mi título")

# ui <- fluidPage(
#   
#   titlePanel("Pruebas de titulares"),
#   
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       h1("Título nivel 1"),
#       h2("Título nivel 2"),
#       h3("Título nivel 3"),
#       h4("Título nivel 4"),
#       h5("Título nivel 5"),
#       h6("Título nivel 6")
#     )
#     
#   )
#   
#   
# )

# ui <- fluidPage(
#   
#   titlePanel("Star Wars App"),
#   
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       h6("Episodio IV", align = "center"),
#       h6("UNA NUEVA ESPERANZA", align = "center"),
#       h5("Nos encontramos en un periodo de ", align = "center"),
#       h4(" guerra civil. Las naves espaciales ", align = "center"),
#       h3("rebeldes atacando desde una base oculta,", align = "center"),
#       h2("han logrado su primera victoria contra el", align = "center"),
#       h1("malvado Imperio Galáctico.", align = "left")
#     )
#     
#   )
#   
#   
# )



# ui <- fluidPage(
# 
#   titlePanel("Pruebas de párrafos"),
# 
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       p("p crea un párrafo de texto"),
#       p("Un nuevo comando p() empieza un nuevo párrafo. Si ponemos un atributo de estilo, cambiará el estilo de todo el párrafo. Por ejemplo, con style = font-family: times, font-si16pt",
#         style = "font-family: 'times'; font-si16pt"),
#       strong("strong() pone el texto en negrita"),
#       em("Con em() ponemos el texto en cursiva"),
#       br(),
#       code("code() <- muestra el texto como código"),
#       br(),
#       div("div() crea segmentos de texto con estilos similares. Esta división será azul porque utilizo style = color:blue",
#           style = "color:blue"),
#       br(),
#       p("span() hace lo mismo que div(), pero funciona con ",
#         span("grupos de palabras", style = "color:red"),
#         "que aparecen dentro del párrafo.")
#     )
# 
#   )
# 
# 
# )


ui <- fluidPage(

  titlePanel("Pruebas con imágenes"),

  sidebarLayout(
    sidebarPanel(),
    mainPanel(

      img(src = "HdD.jpg", height = 400, width = 400)

    )

  )

)





server <- function(input, output){}

shinyApp(ui = ui, server = server)
