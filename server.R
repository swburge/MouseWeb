
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)
library(dalliancR)
library(rjson)
library(ChIPpeakAnno)

#Set up data:
#General data:
mm10.genes<- readRDS("data/mm10.Gene.Label.Data.rds")

#Experiment data:
TimeSeries.data<-fromJSON(file="data/TimeSeries.json")

ChIPSeqMetaData<-list("No data" = 100, "Tet1" = 1,"7C Tet1 KO" = 2,"Tet1 WT/KO Overlaps" = 3)

Tet1.data<-fromJSON(file="data/Tet1.json")

shinyServer(function(input, output, session) {
  
  #Set some values with null defaults (r is used for RNAseq data):
  r<-reactiveValues(data=NULL,browser=NULL,geneSymbols=NULL)
  c<-reactiveValues(data=NULL,browser=NULL,index=NULL,set1=NULL,set2=NULL)
  
  #Now set up browser data for the RNAseq tab:
  #Could modify this to also set ChIPseq data
  #but that is more complicated as ChIPseq browser data not static (depends on user selected experiment:)
  
  #Now wait for user to select logfold vs normalized counts:
  observeEvent(input$dataType,{
    if (input$dataType == "1"){
      r$data <- readRDS(TimeSeries.data$geneLogFoldData)
    } else if (input$dataType == "2") {
      r$data <- readRDS(TimeSeries.data$geneNormalizedCountData)
    }
  })
  
  #Does the user want to use any precompiled data?:
  observe ({
    if( input$precompiled ==2) {
       updateTextInput(session, "geneSymbol", value = readRDS(TimeSeries.data$top20Superstem.gn))
    } else if (input$precompiled ==3) {
       updateTextInput(session, "geneSymbol", value = readRDS(TimeSeries.data$top20Up.gn))
    }
  })
  
  #Now set up the required data.
  dataForPlot<-reactive ({
    #First, get user-defined gene symbols if not using precompiled list:
    gS<-unlist(strsplit(input$geneSymbol, "\\,\\s|\\,|\\s"))
    
    validate(
      need(length(gS)<21, "You have too many genes. Please use less than 20")
    )
    
    #Get cell types:
    cell<-input$cellType
    #I know, I know.... very ugly but functional:
    cell<-gsub("1","all",cell)
    cell<-gsub("2","Rs26",cell)
    cell<-gsub("3","TS_GFP",cell)
  
    #Here I've used x as a shorthand to avoid all the $ signs when using the full name:
    x<-r$data
    x<-x[x$geneID %in% gS &x$cellType %in% cell,]
    
  })
  #end dataForPlot, and most RNASeqtab stuff
  
  #Now for the ChIPseq panel:
  getChIPBrowserData<-reactive({
    chipIndex<-c(input$chipdata,input$chipdata2)
    x<-chipIndex[chipIndex!=100]
    #data<-chipseq.data[as.numeric(x)]
    data<-Tet1.data$`ChIPSeq bigWig data tracks`[as.numeric(x)]
  })
  
  observeEvent(input$chipdata,{
     c$set1<-as.numeric(input$chipdata)
     #print(c$set1)
   })
  observeEvent(input$chipdata2,{
    c$set2<-as.numeric(input$chipdata2)
    #print(c$set2)
  })
 
  
  #Populate second drop down; remove the option chosen in the first drop down so you don't compare the same thing:
  observe({
    x<-input$chipdata
    newChoices<-ChIPSeqMetaData[ChIPSeqMetaData != x]
    updateSelectInput(session,"chipdata2",choices = newChoices)
  })
  
  #Run peak comparison (this awaits user click to start as it is time consuming)
  comparedPeaks<-eventReactive(input$goChIPButton, {
    foo<-findOverlapsOfPeaks(reduce(readRDS(Tet1.data$`ChIPSeq peak lists`[[1]]$genomicRanges)),reduce(readRDS(Tet1.data$`ChIPSeq peak lists`[[2]]$genomicRanges)))
    foo<-foo$peaklist[[3]]
    foo<-as.data.frame(foo)
    return(foo)
  })
    
  #Reactive statement to dynamically build genome browser data:
  browserData<-reactive({
     if (input$tabs == "Timeseries_tabs") { 
      if (input$Timeseries_tabs == "RNASEQ") {
         # bData<-r$browser
          bData<-TimeSeries.data$BrowserTracks
        } else if (input$Timeseries_tabs == "CHIPSEQ") {
          bData<-getChIPBrowserData()
        }
     } else if (input$tabs == "Tet1") { 
      if( input$Tet1_tabs == "Tet1_RNASeq") {
          bData<-Tet1.data$`RNASeq tracks`
      } else if (input$Tet1_tabs == "Tet1_ChIPSeq") {
          bData<-getChIPBrowserData()
        }
      }
    })
   
  #Generate table data for different situations
  #First there are individual functions to generate 
  #the data based on the experiment chosen.
  #Then tableData is used to decide on which experiment is being used
  
  #Tet RNASeq table data:
  tetDataTable<-reactive({
    x<-input$tetData
    f<-Tet1.data$`RNASeq comparison table data`[[as.numeric(x)]]$file
    return(readRDS(f))
  })
  #Now the ChIPSeq data:
  tetChIPTable<-reactive({
    #Default is plain old data for which no comparison has been made:
    if(input$goChIPButton == 0){
      #Tet1_wt.chip[,c(1,2,3,4,12,13,16)]
      t<-readRDS(Tet1.data$`ChIPSeq peak lists`[[1]]$dataFrame)
      t[,c(1,2,3,4,12,13,16)]
      } else {
      comparedPeaks()
    }
  })
  
  #Reactive statement to dynamically generate table data:
  tableData<- reactive({if (input$tabs == "Timeseries_tabs") { 
    if (input$Timeseries_tabs == "RNASEQ") {
      #insert table here if needed
    } else if (input$Timeseries_tabs == "CHIPSEQ") {
      #insert table here if needed
    }
  } else if (input$tabs == "Tet1") { 
    if( input$Tet1_tabs == "Tet1_RNASeq") {
      tData<-tetDataTable()
    } else if (input$Tet1_tabs == "Tet1_ChIPSeq") {
      tData<-tetChIPTable()
    }
  }})
   
#Outputs: from here on this is all about making things look pretty:
  
  #Plot RNAseq data over time series:
  output$plotRNASeq <-renderPlot ({
     ggplot(dataForPlot(),
            aes(x=Day,y=value,color=geneID,group=interaction(geneID,cellType)),
     )+geom_point(aes(shape=factor(cellType)))+stat_smooth(se=FALSE)
   })
  
  #Download handlers:  
  output$downloadData <- downloadHandler(
    filename = function() { paste('shinydata', '.csv', sep='') },
    content = function(file) {
      write.csv(dataForPlot(), file)
    }
  )
  
  output$downloadDataTetRNA<-downloadHandler(
    filename = function(){paste ('shinydata','.csv',sep='')},
    content = function(file){
      write.csv(tetDataTable(),file)
    }
  )
  
  #Code to produce the Dalliance HTML widget
  #Gene name provided by text at the moment, need to wire this in to user choices:
  
  output$dalliance<-renderDalliancR(dalliancR("Ascl2",browserData()))
  
  #Table outputs.
  #The cascading of function to Table1, Table2 etc is to avoid Shiny crashing
  #if you reuse an output name. Have to do this instead of just having
  # an output$Table; can't reuse output$Table in two tabsets in the ui.R
  output$Table1<-output$Table2<-renderDataTable({
    tableData()
  },options = list(lengthMenu  = c(5,10,20),pageLength=5))
  
  output$noData<-renderText({"There doesn't seem to be any data associated with this panel. If you think there should be, please get in touch."})
})
