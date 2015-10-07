library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel(withMathJax('Intervalos de sesgo corregido y acelerado')),
    sidebarPanel(
      sliderInput('N', 'Número de Bootstraps (B)',
                  value = 1000, min = 500, max = 10000, step = 100),
      sliderInput('alpha', 'Nivel de confianza',
                  value = 0.95, min = 0.05, max = .99, step = 0.1)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Histogram', plotOutput('hist', width = '8in', height = '6in')),
        column(3,
               verbatimTextOutput("L"),
               verbatimTextOutput("z0"),
               verbatimTextOutput("a")
        )
      )
    )
  )
})