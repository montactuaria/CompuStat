library(shiny)
library(ggplot2)


montecarlo<-function(nsim,alpha){
  x <-runif(nsim,0,2)
  fi <-2*sqrt(4-(x^2))
  theta <- mean(fi)
  z.a <-qnorm(alpha/2,lower.tail = FALSE)
  S.n <- var(fi)
  lim.sup <- theta + z.a*sqrt(S.n/nsim)
  lim.inf <- theta - z.a*sqrt(S.n/nsim)
  return(list(est=theta))
}


montecarlo1<-function(nsim,alpha){
  x <-runif(nsim,0,1)
  fi <-6/(sqrt(4-(x^2)))
  theta <- mean(fi)
  z.a <-qnorm(alpha/2,lower.tail = FALSE)
  S.n <- var(fi)
  lim.sup <- theta + z.a*sqrt(S.n/nsim)
  lim.inf <- theta - z.a*sqrt(S.n/nsim)
  return(list(est=theta, lim.sup=lim.sup, lim.inf=lim.inf))
}



res<- function(n)  as.numeric(t(sapply(seq(100,10000,by=10), function(x) montecarlo(n, 0.975))))
res2<- function(n)  as.numeric(t(sapply(seq(100,10000,by=10), function(x) montecarlo1(n, 0.975))))



shinyServer(function(input, output) {
  

  data <- reactive({
    dist <- switch(input$dist,
                   res = res,
                   res2 = res2,
                   res)
    
    dist(input$n)
  })
  
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''),col = "blue")
    
  })
  
  output$ploth <- renderPlot({
    dist <- input$dist
    n <- input$n
    plot(data(),type = "l",col = "blue")
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
  
  output$table <- renderTable({
    data.frame(x=data())
  })
  
})