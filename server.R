
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)
geneLogFoldData <- readRDS("data/geneLogFoldData.rds")
Rs26.geneLogFoldData <- readRDS("data/Rs26.geneLogFoldData.rds")
TS_GFP.geneLogFoldData <- readRDS("data/TS_GFP.geneLogFoldData.rds")
#waz.plot<- ggplot(waz.data,
#                  aes(x=Day,y=value,color=geneID,group=interaction(geneID)),environment = environment())
#waz.plot+theme_bw()+stat_smooth(se=FALSE)             

shinyServer(function(input, output) { 
  output$value <- renderPrint({ input$cellType })
  output$text1 <-renderText({paste("Gene(s) chosen: ", input$geneSymbol)})
  output$plot1 <- renderPlot ({
    gS<-unlist(strsplit(input$geneSymbol, split=" "))
    if (input$cellType == 1) {
    data<-geneLogFoldData[geneLogFoldData$geneID %in% gS,]
    ggplot(data,
           aes(x=Day,y=value,color=geneID,group=interaction(geneID)),
    )+stat_smooth(se=FALSE)+labs(y="logFoldChange")
    } else if (input$cellType == 2) {
      data<-Rs26.geneLogFoldData[Rs26.geneLogFoldData$geneID %in% gS,]
      ggplot(data,
             aes(x=Day,y=value,color=geneID,group=interaction(geneID)),
      )+stat_smooth(se=FALSE)
    } else if (input$cellType == 3) {
      data<-TS_GFP.geneLogFoldData[TS_GFP.geneLogFoldData$geneID %in% gS,]
      ggplot(data,
             aes(x=Day,y=value,color=geneID,group=interaction(geneID)),
             )+stat_smooth(se=FALSE)
    }
  })
  
  
 

})
