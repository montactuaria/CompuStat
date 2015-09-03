library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Inverse Function Method'),
    sidebarPanel(
      numericInput('nsim', label = 'Número de simulaciones deseadas', value = 1000, min = 1),
      numericInput('lambda', label = 'lambda', value = 1, min = 0)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Density histogram', plotOutput('plot')),
        tabPanel('Table', dataTableOutput('table'))
      )
    )
  )
})

