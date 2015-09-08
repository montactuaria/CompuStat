library(shiny)
library(ggplot2)

rExp <- function(nsim, lambda){
  return((-1/lambda)*log(1-runif(nsim)))
}

shinyServer(function(input, output){
  
  dat <- reactive(rExp(input$nsim,input$lambda))
  
  output$table <- renderDataTable(data.frame(value = dat()))
  
  output$plot <- renderPlot(
    qplot(dat(), fill="blue")
          )
    
})
