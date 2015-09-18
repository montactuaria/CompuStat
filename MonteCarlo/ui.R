library(shiny)

shinyUI(fluidPage(
  withMathJax(),
  h2("Integrales-Montecarlo"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Integrales:",
                   c("$$\\sqrt(4-x^2)$$" = "res",
                     "$$6/\\sqrt(4-x^2)$$" = "res2")),
      br(),
      
      sliderInput("n", 
                  "Numero de simulaciones:", 
                  value = 500,
                  min = 1, 
                  max = 1000)
    ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Histograma", plotOutput("plot")), 
                  tabPanel("Estimaciones", plotOutput("ploth")), 
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Tabla", tableOutput("table"))
      )
    )
  )
))