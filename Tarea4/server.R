library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(bootstrap)
library(boot)
data(spatial)

########para el paquete boot
fun<-function(x) sum((x - mean(x)) ^ 2) / length(x)
my.function<-function(x,i) fun (x[i]) 
########para el paquete bootstrap
varSesgada <- function(x) sum((x - mean(x)) ^ 2) / length(x)


shinyServer(function(input, output){

  N <- reactive(rep(input$N))
  alpha <- reactive(rep(input$alpha))
  
  results <- reactive({
              boot(data=spatial[, 1], my.function, R=N())  
            })
  
  BCa<-reactive({
        bcanon(x = spatial[, 1], nboot = N(), theta = varSesgada)
      })
  
  ci_H<-reactive({
  confidence_interval_H = boot.ci(results(), index = 1, conf = alpha(), type = 'bca')
  as.data.frame(confidence_interval_H$bca[ , c(4, 5)])
  })
  
  
  
    output$hist <- renderPlot({
      confidence_interval_H <- boot.ci(results(), index = 1, conf = alpha(), type = 'bca')    
       ci_H <- confidence_interval_H$bca[ , c(4, 5)]
      hist(results()$t[,1], main = 'Coefficient of Determination: Height', xlab = 'RSquared',
           col = 'grey', prob = T)
      lines(density(results()$t[,1]), col = 'blue')
      abline(v = ci_H, col = 'red')
      abline(v=results()$t0)
    })
  
    output$L <- renderText({
      paste("Intervalos de confianza:", as.character(ci_H()[1]))
    })
    
    output$z0 <- renderText({
      paste("1ª Factor de corrección z0:", as.character(BCa()$z0))
    })
    
    output$a <- renderText({
      paste("2ª Factor de corrección-aceleración:", as.character(BCa()$acc))
    })
    
})