
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)
library(RCircos)
library(dalliancR)
library(rjson)

#Set up data:
all.geneLogFoldData<- readRDS("data/all.geneLogFoldData.rds")    
all.geneNormalizedCountData<- readRDS("data/all.geneNormalizedCountData.rds")
mm10.genes<- readRDS("data/mm10.Gene.Label.Data.rds")
top20Up.gn<- readRDS("data/top20Up.rds")
top20.gn<-readRDS("data/top20.superstem.rds")
data(UCSC.Mouse.GRCm38.CytoBandIdeogram)
#ChIPSeqMetaData<-as.data.frame(list(number=c(1:3),name=c("Tet1" ,"7C Tet1 KO","Tet1 WT/KO Overlaps")))
ChIPSeqMetaData<-list("No data" = 100, "Tet1" = 1,"7C Tet1 KO" = 2,"Tet1 WT/KO Overlaps" = 3)
Master.Day1vsDay0.Heatmap<-readRDS("data/Master.Day1vsDay0.lFC.heatmap.rds")
rnaseq.data<-fromJSON(file="data/rnaseq_data.json")

#Set up Circos:
cyto.info<-UCSC.Mouse.GRCm38.CytoBandIdeogram
chr.exclude<-NULL
RCircos.Set.Core.Components(cyto.info,chr.exclude,tracks.inside =5 ,tracks.outside = 1)
rcircos.position<-RCircos.Get.Plot.Positions()
rcircos.cyto<-RCircos.Get.Plot.Ideogram()
rcircos.params<-RCircos.Get.Plot.Parameters()
rcircos.params$track.background<-"white"
rcircos.params$tile.color<-"pink"
RCircos.Reset.Plot.Parameters(rcircos.params)

shinyServer(function(input, output, session) {
 
  #Function to process gene names provided by user: 
  getNames<-reactive({
    unlist(strsplit(input$geneSymbol, "\\,\\s|\\,|\\s"))
  })
  
  #Code here to pass data back to the UI for use in e.g. Javascript.
  #Partly redundant by widgetification of Dalliance
  #Originally used to hand gene name list back to feed into Dalliance search:
  observe({
    names<-getNames()
    session$sendCustomMessage(type = 'testmessage',
                              message = list(first = names[1],all=names))
  
    })
  
  #Set up precompiled data:
  observe ({
    
    if( input$precompiled ==2) {
      updateTextInput(session, "geneSymbol", value = top20.gn)
    } else if (input$precompiled ==3) {
      updateTextInput(session, "geneSymbol", value = top20Up.gn)
    }
  })
 
  #Now get and validate gene names:
  dataForSession<-reactive ({
    gS<-getNames()  
    validate(
      need(length(gS)<21, "You have too many genes. Please use less than 20")
    )
    
  #Get cell types.  
  cell<-input$cellType
  #I know, I know.... very ugly but functional:
  cell<-gsub("1","all",cell)
  cell<-gsub("2","Rs26",cell)
  cell<-gsub("3","TS_GFP",cell)
  
  #Get data sources based on user requesting LFC or normalized counts:
  
  currentData<-reactive({ 
    if (input$dataType == "1" ) {
      data<- all.geneLogFoldData
    }
    else if (input$dataType == "2") {
      data<- all.geneNormalizedCountData
    }
      
    })
    
  #Filter data based on gene request and cell type:
  data<-currentData()
  data<-data[data$geneID %in% gS &data$cellType %in% cell,]
    
  })
  
  #Get data for genome browser:
   browserData<-reactive({
      if (input$tabs == "RNASEQ") {
        bData<-rnaseq.data
      } else {
        
      }
    })
#Outputs: from here on this is all about making things look pretty:
  
  #Plot RNAseq data over time series:
  output$plotRNASeq <-renderPlot ({
    ggplot(dataForSession(),
           aes(x=Day,y=value,color=geneID,group=interaction(geneID,cellType)),
    )+geom_point(aes(shape=factor(cellType)))+stat_smooth(se=FALSE)
  })
  
  #Self explanitory:  
  output$downloadData <- downloadHandler(
    filename = function() { paste('shinydata', '.csv', sep='') },
    content = function(file) {
      write.csv(dataForSession(), file)
    }
  )
  
  #Code to produce the Dalliance HTML widget
  #Gene name provided by text at the moment, need to wire this in to user choices:
  
  output$dalliance<-renderDalliancR(dalliancR("Ascl2",browserData()))
  #output$dallianceCHIP<-renderDalliancR(dalliancR())

  circosPlot<-eventReactive(input$goButton,{
    gS<-getNames()
    outfile<-tempfile(fileext = '.svg')
    svg(outfile)

    RCircos.Set.Plot.Area()
    
    RCircos.Chromosome.Ideogram.Plot()
    name.col<-4
    side<-"in"
    track.num<-1
    RCircos.Gene.Connector.Plot(mm10.genes[mm10.genes$Gene %in% gS,],track.num,side)
    track.num<-2
    RCircos.Gene.Name.Plot(mm10.genes[mm10.genes$Gene %in% gS,],name.col,track.num,side)
    if (input$checkHeat) {
      track.num<-1
      RCircos.Heatmap.Plot(Master.Day1vsDay0.Heatmap,data.col = 5 ,track.num,side)
    }
    dev.off()
    list(src=outfile,alt="Please wait...")
  })
  observe({
    x<-input$chipdata
    newChoices<-ChIPSeqMetaData[ChIPSeqMetaData != x]
    updateSelectInput(session,"chipdata2",choices = newChoices)
  })
  circosChIPPlot<-eventReactive(input$goChIPButton,{
    chipDataList<-function(n) 
      if (n == 1) { d1 <-readRDS("data/R26_Tet1_ChIPSeq.rds")}
      else if (n == 2) { d1 <- readRDS("data/7C_Tet1_KO_ChIPSeq.rds")}
      else if (n == 3) { d1 <- readRDS("data/Tet1_WT_KO.overlaps.rds")}
      else {d1 <-NULL}
    
    currentChIPData1<- reactive ({
      chipDataList(input$chipdata)
    })
    currentChIPData2<- reactive({
      chipDataList(input$chipdata2)
    })

    
  #gS<-getNames()
    outfile<-tempfile(fileext = '.svg')
    svg(outfile)
    
    RCircos.Set.Plot.Area()
    chr.exclude<-NULL
    #chr.exclude<-c("chr9", "chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chrX","chrY")
    #chr.exclude<-c("chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chrX","chrY")
    RCircos.Set.Core.Components(cyto.info,chr.exclude,tracks.inside =5 ,tracks.outside = 1)
    rcircos.position<-RCircos.Get.Plot.Positions()
    rcircos.cyto<-RCircos.Get.Plot.Ideogram()
    rcircos.params<-RCircos.Get.Plot.Parameters()
    rcircos.params$track.background<-"white"
    rcircos.params$tile.color<-"blue"
    RCircos.Reset.Plot.Parameters(rcircos.params)
    RCircos.Chromosome.Ideogram.Plot()
     side<-"in"
     track.num<-1
    RCircos.Tile.Plot(currentChIPData1(),track.num,side)
    if (input$chipdata2 != 100) {
      track.num<-2
      RCircos.Tile.Plot(currentChIPData2(),track.num,side)
    }
    name.col<-4
    side<-"out"
    track.num<-1
    RCircos.Gene.Connector.Plot(mm10.genes[mm10.genes$Gene %in% currentChIPData1()$symbol,],track.num,side)
    track.num<-2
    RCircos.Gene.Name.Plot(mm10.genes[mm10.genes$Gene %in% currentChIPData1()$symbol,],name.col,track.num,side)
    dev.off()
    list(src=outfile,alt="Please wait...")
  })
  
  output$circosImage <- 
    renderImage({circosPlot()
    },deleteFile = TRUE
  )
  output$circosChIPImage <-
    renderImage({circosChIPPlot()
      },deleteFile = TRUE
  )


})
