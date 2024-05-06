#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram


  library(shiny)
  library(UsingR)
  library(compareGroups)
  library(tidyverse)
  library(knitr)
  library(NHANES)
  require(mlbench)
  library(DT)
  library(survival)
  library(riskCommunicator)
  library(corrplot)
  
  data(regicor)
  data(framingham)
  
  data(PimaIndiansDiabetes2)
  
  #BreastCancerImages <- read.csv('https://www.dropbox.com/s/vp44yozebx5xgok/bdiag.csv?dl=1')
  #BreastCancerImages <- BreastCancerImages[,-1]
  #data(BreastCancerImages)
  
  predimed <- readRDS("www/newPredimed.R")
  UCI <- readRDS("www/uci.R")
  cardio <- readRDS("www/cardio.R")
  
  # Define UI for application that draws a histogram
  shinyUI(fluidPage(
    column(3,
           h3("Select data and variables"),
           hr(),
           selectInput('datos','Select a data set',c('fat','regicor',"predimed","UCI","NHANES","birthwt","cardio","framingham")),
           uiOutput('SelVarDep'),
           uiOutput('SelVarPredictors')),
    column(9,
           navbarPage('Linear Regression',inverse = TRUE,
               
               tabPanel('Data base description',
                        htmlOutput('TextHelp')),
               
               tabPanel('Descriptive',
                        tabsetPanel(
                          tabPanel('Data',
                                   DTOutput('DataTable')),
                          tabPanel('Correlations',
                                   h4("Correlation matrix"),
                                   hr(),
                                   plotOutput('TableCorrelations'))
                        )
                        
               ),
               tabPanel('Linear Model',
                        h4('Linear model fit'),
                        verbatimTextOutput('LinearModelFit'))),       
               
           )
  )
)
