library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Box-Müller method'),
    sidebarPanel(
      sliderInput('nsim', label = 'Número de simulaciones deseadas', value = 1000, min = 1,max=5000)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Density histogram', plotOutput('plot')),
        tabPanel('Table', dataTableOutput('table'))
      )
    )
  )
})
