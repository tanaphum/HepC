#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(Rcpp)
library(deSolve)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    setwd("C:/hepc/web")
    sourceCpp('p1.cpp')
    
   
    #times3 <- seq(0,10,0.01)
    times <- seq(0, 40, by = 0.1)
    
    
    #out <-ode(yini3,times= times3,func = mysystem3,parms = pars3,method = "rk4")
    #plot(out,select = c("S"))
    out <- ode( y = inits,times =  times, func = PanHepC, parms = parms, method = "rk4")
    plot(out,select = c("F1"))
  })
  
})
