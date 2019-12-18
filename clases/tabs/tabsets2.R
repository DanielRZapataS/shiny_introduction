library(shiny)

ui <- fluidPage(
  
  titlePanel(" Tabsets - Pestañas "),
  
  sidebarLayout(position = "left",
                
                sidebarPanel(
                  
                #   radioButtons("dist", "Introduce el tipo de Distribución:",
                #                choices = c("Normal" = 'normal',
                #                            "Uniforme" = 'uniform',
                #                            "Log-Normal" = 'log',
                #                            "Exponencial" = 'exp'
                #                )
                #   ),
                #   
                  sliderInput("obs", "Introduce el número de observaciones:",
                              min = 10, max = 1000, value = 500)

                ),
                
                mainPanel(
                  
                  tabsetPanel(
                    
                    tabPanel("Normal",
                             tabsetPanel(
                               
                               tabPanel("Plot",
                                        plotOutput("normalPlot")),
                               tabPanel("Sumario",
                                        verbatimTextOutput("normalSum")),
                               tabPanel("Tabla",
                                        tableOutput("normalTabla"))
                               
                              )
                             ),
                    tabPanel("Uniforme",
                             tabsetPanel(
                               
                               tabPanel("Plot",
                                        plotOutput("unifPlot")),
                               tabPanel("Sumario",
                                        verbatimTextOutput("unifSum")),
                               tabPanel("Tabla",
                                        tableOutput("unifTabla"))
                               
                             )
                    ),
                    tabPanel("Logarítmica",
                             tabsetPanel(
                               
                               tabPanel("Plot",
                                        plotOutput("logPlot")),
                               tabPanel("Sumario",
                                        verbatimTextOutput("logSum")),
                               tabPanel("Tabla",
                                        tableOutput("logTabla"))
                               
                             )
                    ),
                    tabPanel("Exponencial",
                             tabsetPanel(
                               
                               tabPanel("Plot",
                                        plotOutput("expPlot")),
                               tabPanel("Sumario",
                                        verbatimTextOutput("expSum")),
                               tabPanel("Tabla",
                                        tableOutput("expTabla"))
                               
                             )
                    )
                  )
                  
                )
                
  )
  
)

server <- function(input, output){
  
  dist <- reactive({
    
    data.frame(
      normal = rnorm(as.numeric(input$obs)),
      uniform = runif(as.numeric(input$obs)),
      log = rlnorm(as.numeric(input$obs)),
      exp = rexp(as.numeric(input$obs))
    )
    
  })
  
  histograma <- function(data, color){
    
    hist(data, col = color, border = "white")
    
    }
  
  output$normalPlot <- renderPlot({
    
    histograma(dist()$normal, "blue")
    
  })
  
  output$unifPlot <- renderPlot({
    
    histograma(dist()$uniform, "red")
    
  })
  
  output$logPlot <- renderPlot({
    
    histograma(dist()$log, "green")
    
  })
  
  output$expPlot <- renderPlot({
    
    histograma(dist()$exp, "purple")
    
  })
  
  output$normalSum <- renderText({

    summary(dist()$normal)

  })
  
  output$unifSum <- renderText({
    
    summary(dist()$uniform)
    
  })
  
  output$logSum <- renderText({
    
    summary(dist()$log)
    
  })
  
  output$expSum <- renderText({
    
    summary(dist()$exp)
    
  })

  output$normalTabla <- renderTable({

    dist()$normal

  })
  
  output$unifTabla <- renderTable({
    
    dist()$uniform
    
  })
  
  output$logTabla <- renderTable({
    
    dist()$log
    
  })
  
  output$expTabla <- renderTable({
    
    dist()$exp
    
  })
  
}

shinyApp(ui, server)
