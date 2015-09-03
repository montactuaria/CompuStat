library(shiny)
library(ggplot2)

rExp <- function(nsim, lambda = 1){
  return((-1/lambda)*log(1-runif(nsim)))
}

shinyServer(function(input, output){
  
  dat <- data.frame(Value=rExp(nsim, lambda))
  
  output$table <- renderDataTable(dat)
  
  output$plot <- renderPlot(
            ggplot(dat, aes(x=Value)) +
            geom_histogram(aes(y=..density..), binwidth= .2, colour="black", fill="white")+
            stat_function(fun = function(x) lambda*exp(-lambda*x),colour = "blue")
            )
  
})
