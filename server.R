
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)
library(RCircos)

all.geneLogFoldData<- readRDS("data/all.geneLogFoldData.rds")    
all.geneNormalizedCountData<- readRDS("data/all.geneNormalizedCountData.rds")
mm10.genes<- readRDS("data/mm10.Gene.Label.Data.rds")
top20Up.gn<- readRDS("data/top20Up.rds")
top20.gn<-readRDS("data/top20.superstem.rds")

shinyServer(function(input, output, session) {
  
  getNames<-reactive({
    unlist(strsplit(input$geneSymbol, "\\,\\s|\\,|\\s"))
  })
  
  observe ({
   if( input$precompiled ==2) {
     updateTextInput(session, "geneSymbol", value = top20.gn)
   } else if (input$precompiled ==3) {
     updateTextInput(session, "geneSymbol", value = top20Up.gn)
   }
  })
  
  output$value <- renderPrint({ input$cellType })
  #output$text1 <-renderText({paste("Gene(s) chosen: ", gS() )})#input$geneSymbol)})
  output$plot1 <- renderPlot ({
    gS<-getNames()  
    
   validate(
    need(length(gS)<21, "You have too many genes. Please use less than 20")
    )
  
    cell<-input$cellType
    cell<-gsub("1","all",cell)
    cell<-gsub("2","Rs26",cell)
    cell<-gsub("3","TS_GFP",cell)
    
    currentData<-reactive({ 
      
      if (input$dataType == "1" ) {
        data<- all.geneLogFoldData
      }
      else if (input$dataType == "2") {
        data<- all.geneNormalizedCountData
      }
      
    })
    
    data<-currentData()
    data<-data[data$geneID %in% gS &data$cellType %in% cell,]
    
    output$text <- renderText({  
        paste("You have selected:",data)
    }) 
      
    ggplot(data,
        aes(x=Day,y=value,color=geneID,group=interaction(geneID,cellType)),
        )+geom_point(aes(shape=factor(cellType)))+stat_smooth(se=FALSE)
    
  })
  
  circosPlot<-eventReactive(input$goButton,{
    gS<-getNames()
    RCircos.Set.Plot.Area()
    outfile<-tempfile(fileext = '.svg')
    svg(outfile)
    plot.new()
    plot.window(c(-2.5,2.5),c(-2.5,2.5))
    RCircos.Chromosome.Ideogram.Plot()
    name.col<-4
    side<-"in"
    track.num<-1
    RCircos.Gene.Connector.Plot(mm10.genes[mm10.genes$Gene %in% gS,],track.num,side)
    track.num<-2
    RCircos.Gene.Name.Plot(mm10.genes[mm10.genes$Gene %in% gS,],name.col,track.num,side)
    dev.off()
    list(src=outfile,alt="Please wait...")
  })
  
  output$circosImage <- 
    renderImage({circosPlot()
    },deleteFile = TRUE
  )


})
