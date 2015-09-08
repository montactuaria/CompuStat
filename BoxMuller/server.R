library(shiny)
library(ggplot2)

rNorm <- function(nsim){
  x <- numeric(nsim*2)
  for(i in 1:round(nsim/2)){
    u1 <- runif(1)
    u2 <- runif(1)
    theta<-2*pi*u2
    R<-sqrt(2*log(u1)*(-1))
    #cambio de polares
    x[2*(i-1) + 1] <- R*cos(theta)
    x[2*(i-1) + 2] <- R*sin(theta)
  }
  x[1:(nsim/2)]
}


shinyServer(function(input, output){
  dat <- reactive(rNorm(input$nsim))
  output$table <- renderDataTable(data.frame(value = dat()))
  output$plot <- renderPlot(
    qplot(dat(), fill="blue")
  )
  
})


