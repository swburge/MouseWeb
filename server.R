
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)
#geneLogFoldData <- readRDS("data/geneLogFoldData.rds")
#Rs26.geneLogFoldData <- readRDS("data/Rs26.geneLogFoldData.rds")
# TS_GFP.geneLogFoldData <- readRDS("data/TS_GFP.geneLogFoldData.rds")
all.geneLogFoldData<- readRDS("data/all.geneLogFoldData.rds")           

shinyServer(function(input, output) {
  
  output$value <- renderPrint({ input$cellType })
  output$text1 <-renderText({paste("Gene(s) chosen: ", input$geneSymbol)})
  output$plot1 <- renderPlot ({
    
    gS<-unlist(strsplit(input$geneSymbol, split=" "))
    
    validate(
      need(length(gS)<20, "You have too many genes. Please use less than 20")
    )
    
    cell<-input$cellType
    cell<-gsub("1","all",cell)
    cell<-gsub("2","Rs26",cell)
    cell<-gsub("3","TS_GFP",cell)
      data<-all.geneLogFoldData[all.geneLogFoldData$geneID %in% gS&all.geneLogFoldData$cellType %in% cell,]
      ggplot(data,
             aes(x=Day,y=value,color=geneID,group=interaction(geneID,cellType)),
             )+geom_point(aes(shape=factor(cellType)))+stat_smooth(se=FALSE)
    
  })
  
  
 

})
