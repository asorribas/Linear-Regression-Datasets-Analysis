#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tools)
library(gbRd)
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

#predimed <- predimed %>% mutate(codeEvent=factor(event,labels=c(1,0)))

predimed <- readRDS("www/newPredimed.R")
UCI <- readRDS("www/uci.R")
cardio <- readRDS("www/cardio.R")
data(birthwt)
birthwt$race <- factor(birthwt$race,labels = c('white','black','other'))
birthwt$smoke <- factor(birthwt$smoke,labels = c('No','Yes'))
data(regicor)
regicor <- regicor %>% dplyr::select(-c(cv,tocv,id))
NHANES <- NHANES %>% dplyr::select(Diabetes,everything())
cardio <- cardio %>% dplyr::select(target,everything())
predimed <- predimed %>% dplyr::select(event,everything())
UCI <- UCI %>% dplyr::select(ESTADO,everything())
UCI$GSODIO <- relevel(UCI$GSODIO,ref='Normal')
UCI$TAMR <- relevel(UCI$TAMR,ref='Normal')
framingham <- framingham %>% dplyr::select(CVD,everything())
framingham <- framingham %>% mutate_at(c("SEX","CURSMOKE","DIABETES","BPMEDS","educ","PREVCHD",
                                         "PREVAP","PREVMI","PREVSTRK","PREVHYP","PERIOD",
                                         "DEATH","ANGINA","HOSPMI","MI_FCHD","ANYCHD","STROKE",
                                         "HYPERTEN"),factor)

get_vignette_link <- function(...) {
  x <- vignette(...)
  if (nzchar(out <- x$PDF)) {
    ext <- tools::file_ext(out)
    port <- if (tolower(ext) == "html") 
      tools::startDynamicHelp(NA)
    else 0L
    if (port > 0L) {
      out <- sprintf("http://127.0.0.1:%d/library/%s/doc/%s", 
                     port, basename(x$Dir), out)
      return(out)
    }
  }
  stop("no html help found")
}


function(input, output, session) {

  output$SelVarDep <- renderUI({
    mydata <- get(input$datos)
    varSelectInput('VarDep','Select a variable to predict',mydata %>% select_if(~ is.numeric(.)))
  })
  
  output$SelVarPredictors <- renderUI({
    mydata <- get(input$datos)
    varSelectInput('VarPredictor','Select predictors',mydata %>% select_if(~ is.numeric(.)),multiple=TRUE)
  })
  
  output$DataTable <- renderDT({
    mydata <- get(input$datos)
    datatable(mydata,options(list(pageLength=25, server = TRUE)))
  })
  
  output$TableCorrelations <- renderPlot({
    mydata <- get(input$datos)
    x <- c(as.character(input$VarDep),as.character(input$VarPredictor))
    d <-mydata[,x,drop=F]
     
    matriz_cor <- cor(d)
    corrplot(matriz_cor, method = "color", type = "upper", 
             addCoef.col = "black", number.cex =1.5, tl.col = "black", tl.srt = 45, diag = T)
  })
  
  output$TextHelp <- renderText({
    y <- input$datos
    if (y =='predimed') return(HTML('Sorry, no description availabe at this moment for this data set.
                                            These data come from a csv file and are not included in a package
                                           with an appropriate description. <br><br> The dataset includes 
                                           data from the PREDIMED study'))
    if (y =='UCI') return(HTML('Sorry, no description availabe at this moment for this data set.
                                            These data come from a csv file and are not included in a package
                                           with an appropriate description. <br><br> The dataset includes 
                                           data from UCI patients'))
    if (y =='cardio') return(HTML("See a <a href='https://www.kaggle.com/datasets/jocelyndumlao/cardiovascular-disease-dataset?resource=download' 
                                    target='blank'>Cardiovascular dataset explanation</a> in this link."))
    temp = Rd2HTML(Rd_fun(paste(y)),out = tempfile("docs"))
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  output$LinearModelFit <- renderPrint({
    mydata <- get(input$datos)
    y <- input$VarDep
    x <- input$VarPredictor
    res <- lm(as.formula(paste(y,'~',paste(x,collapse='+'))),data=mydata)
    summary(res)
  })

}
