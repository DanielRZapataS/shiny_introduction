library(shiny)


library(shiny)

ui <- fluidPage(
    titlePanel("Título de la App"),
    sidebarLayout(
      position = "right",
      sidebarPanel("panel lateral", with = 4),
      mainPanel("panel principal")
    )
    
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)