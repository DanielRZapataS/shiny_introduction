library(shiny)

ui <- fluidPage(
  
  titlePanel(" Tabsets - Pestañas "),
  
  sidebarLayout(position = "left",
                
                sidebarPanel(
                  
                  radioButtons("dist", "Introduce el tipo de Distribución:",
                               choices = c("Normal" = 'normal',
                                           "Uniforme" = 'uniform',
                                           "Log-Normal" = 'log',
                                           "Exponencial" = 'exp'
                               )
                  ),
                  
                  sliderInput("obs", "Introduce el número de observaciones:",
                              min = 10, max = 1000, value = 500)
                  
                ),
                
                mainPanel(
                  
                  tabsetPanel(
                    
                    tabPanel("Plot",
                             plotOutput("distPlot")),
                    tabPanel("Sumario",
                             verbatimTextOutput("sumario")),
                    tabPanel("Tabla",
                             tableOutput("tabla"))
                    
                  )
                  
                )
                
  )
  
)

server <- function(input, output){
  
  dist <- reactive({
    
    switch(input$dist,
           normal = rnorm(as.numeric(input$obs)),
           uniform = runif(as.numeric(input$obs)),
           log = rlnorm(as.numeric(input$obs)),
           exp = rexp(as.numeric(input$obs))
    )
    
  })
  
  output$distPlot <- renderPlot({
    
    hist(dist())
    
  })
  
  output$sumario <- renderText({
    
    summary(dist())
    
  })
  
  output$tabla <- renderTable({
    
    dist()
    
  })
  
}

shinyApp(ui, server)
