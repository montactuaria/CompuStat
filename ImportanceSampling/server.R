library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)

exp.truncada <- function(phi1, N, g=runif, f=dunif, alpha=0.05){
  results <- lapply(N, function(nsim){
    x <- g(nsim)
    f<- -log(1-(1-exp(-.2))*x)
    gx <- sapply(x, f)
    gx <- sapply(x, dunif)
    Phi <- sapply(x, phi1)
    estim <- mean(Phi/gx)/2
    s2 <- var(Phi/gx)
    lim.sup <- estim + sqrt(s2/nsim)*qnorm(alpha/2, lower.tail = F)
    lim.inf <- estim - sqrt(s2/nsim)*qnorm(alpha/2, lower.tail = F)
    results.table <- data.frame(N=nsim, Estimate=estim, lim.inf=lim.inf, lim.sup=lim.sup)
    return(results.table)
  })
  return(ldply(results) %>% mutate(i = row_number()))
}

mc.intervals <- function(phi, N, g=runif, f=dunif, alpha=0.05){
  results <- lapply(N, function(nsim){
    x <- g(nsim)
    gx <- sapply(x, f)
    Phi <- sapply(x, phi)
    estim <- mean(Phi/gx)
    s2 <- var(Phi/gx)
    lim.sup <- estim + sqrt(s2/nsim)*qnorm(alpha/2, lower.tail = F)
    lim.inf <- estim - sqrt(s2/nsim)*qnorm(alpha/2, lower.tail = F)
    results.table <- data.frame(N=nsim, Estimate=estim, lim.inf=lim.inf, lim.sup=lim.sup)
    return(results.table)
  })
  return(ldply(results) %>% mutate(i = row_number()))
}

gd <- function(x) dunif(x,0,2)
rg <- function(n) runif(n,0,2)
G <- list(list(gd,rg))

shinyServer(function(input, output){
  N <- reactive(rep(input$n, input$N))
  mc <- reactive({
    phi <- function(x){
      input$m*exp(-input$m*x)
    }
    i <- as.numeric(1)
    g <- G[[i]]
    mc.intervals(phi, N(), g[[2]], g[[1]], alpha = 0.05)
  })
  
  output$mc <- renderDataTable(mc())
  output$mc_plot <- renderPlot(
    ggplot(mc(), aes(i, Estimate)) +
      geom_line() +
      geom_ribbon(aes(ymin=lim.inf, ymax=lim.sup))
  )

  expt <- reactive({
    phi1<-function(x) dnorm(x)/(1-exp(-.2))
    i <- as.numeric(100)
    exp.truncada(phi1, N(), runif, dunif, alpha = 0.05)
  })
  
  output$expt <- renderDataTable(expt())
  output$expt_plot <- renderPlot(
    ggplot(expt(), aes(i, Estimate)) +
      geom_line() +
      geom_ribbon(aes(ymin=lim.inf, ymax=lim.sup))
  )
  
  output$hist <- renderPlot({
    dat <- rbind(cbind(method='Monte Carlo crudo',mc()), cbind(method='exponencial truncada',expt()))
    ggplot(dat) +
      geom_density(aes(Estimate, fill=method), alpha=0.5) 
  
    })
  output$sims <- renderPlot({
    dat_mc <- mc() %>% mutate(i = row_number(), method='Monte Carlo crudo')
    dat_expt <- expt() %>% mutate(i = row_number(), method='exponencial truncada')
    dat <- rbind(dat_mc, dat_expt)
    ggplot(dat) +
      geom_ribbon(aes(i,ymin=lim.inf,ymax=lim.sup, fill=method), alpha=0.5) +
      geom_line(aes(i,Estimate)) +
      facet_wrap(~method, nrow = 2)
  })
  
})
