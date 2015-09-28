library(shiny)

shinyUI(function(input, output){
  fluidPage(
      headerPanel(withMathJax("$$\\text{Importance Sampling:} m e^{-mx}$$")),
    sidebarPanel(
      sliderInput('N', 'Número de simulaciones',
                  value = 50, min = 1, max = 1000, step = 1),
      sliderInput('n', 'Número eventos en la simulación',
                  value = 100, min = 1, max = 500, step = 1),
      sliderInput('m', 'Constante m', value = 10,
                  min = 1, max = 100, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Histogram', plotOutput('hist')),
        tabPanel('Simulation',plotOutput('sims')),
        tabPanel('Datos MC Crudo', dataTableOutput('mc')),
        tabPanel('Datos Ex.truncada', dataTableOutput('expt'))
      )
    )
  )
})