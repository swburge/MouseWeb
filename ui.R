
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dalliancR)

shinyUI(navbarPage("Hemberger Lab Portal", id = "tabs",
                   
                              tabPanel("RNASeq Timeseries", value = "Timeseries_tabs",
                                       tabsetPanel("RNAseq_Timeseries",id = "Timeseries_tabs",
                                                   tabPanel("RNASeq", value = "RNASEQ",
                                                            sidebarLayout(
                                                              sidebarPanel( 
                                                                textInput("geneSymbol", "Gene Symbol:"),
                                                                helpText("Enter up to 20 standard gene Symbols. To enter more than one, place a space between each gene symbol."),
                                                                selectInput("precompiled", label=h4("Some precompiled lists:"),
                                                                            choices = list(" "= 1,"Top 20 Superstem" = 2, "Top 20 Upregulated" = 3)
                                                                ),
                                                                radioButtons("dataType", label = h4("Data type"),
                                                                             choices = list("Log fold change" = 1, "Normalized counts" = 2),selected = 1
                                                                ),
                                                                checkboxGroupInput("cellType", 
                                                                                   label = h4("Cell types"), 
                                                                                   choices = list("All" = 1, "TS_Rs26" = 2, "TS_EGFP" = 3), selected = 1
                                                                ),
                                                                downloadButton('downloadData', 'Download current data')
                                          
                                                              ),
                                                              mainPanel(
                                                                plotOutput("plotRNASeq"),
                                                                
                                                                #Annoying hack to stop window scroll bars appearing:
                                                                tags$style(type="text/css", ".tab-content { overflow: visible; }")
                                                              )
                                                            )
                                                            
                                                   ),
                                                  
                                                   tabPanel("ChIPSeq", value = "CHIPSEQ",
                                                            
                                                              textOutput("noData")
                                                            
                                                            #                                                             
                                                           
                                                   ),#end of RNAseq timesereis Chipseq tab
                                                  
                                       tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 80%; /* or 950px */}"))
                                       
                                       )
                                       
                                       ),
                              
                              tabPanel("Tet1", value = "Tet1",
                                       tabsetPanel("Tet1",id = "Tet1_tabs",
                                                   tabPanel("TetRNASeq",value = "Tet1_RNASeq",
                                                            sidebarLayout(
                                                              sidebarPanel(
                                                                selectInput("tetData",label=h4("Choose your comparison:"),
                                                                            choices = list("Control 20% vs Tet1 KO 20%" = 1,
                                                                                           "Control 20% vs Tet1/2 DKO 20%" = 2,
                                                                                           "Control 20% vs (Tet1 KO + Tet1/2 DKO 20%)" = 3,
                                                                                           "Control 20% vs 5%" = 4,
                                                                                           "(Control 20% vs 5%) vs (Tet1 KO 20% vs Tet1 KO 5%)" = 5,
                                                                                           "(Control 20% vs 5%) vs (Tet1/2 DKO 20% vs Tet1/2 DKO 5%)" = 6)
                                                              ),
                                                              downloadButton('downloadDataTetRNA',"Download current data")),
                                                              mainPanel(
                                                                dataTableOutput('TetRNASeqTable')
                                                              )
                                                            )
                                                            ),
                                                   tabPanel("TetChIPSeq",value = "Tet1_ChIPSeq",
                                                            sidebarLayout(
                                                              sidebarPanel(
                                                                selectInput("chipdata",label=h4("Choose your experiment:"),
                                                                            choices = list("Tet1" = 1, "7C Tet1 KO" = 2, "Tet1 WT/KO Overlaps" = 3)),
                                                                selectInput("chipdata2",label=h4("Chose second dataset:"),
                                                                            choices = list("None" = 100, "Tet1" = 1, "7C Tet1 KO" = 2, "Tet1 WT/KO Overlaps" = 3)),
                                                                radioButtons("dataOperation", label = h4("Display in table:"),
                                                                             choices = list("Overlap" = 1, "Difference" = 2)),
                                                                actionButton("goChIPButton", "Compare peaks"),
                                                                tags$style(type="text/css", ".tab-content { overflow: visible; }")
                                                              ),
                                                              mainPanel(
                                                                dataTableOutput('chipTable'),
                                                                tags$style(type="text/css", ".tab-content { overflow: visible; }")
                                                              )
                                                            ))
                                                   )
                                       )
                   
  
  # Application title
  #titlePanel(HTML(("TSPort&lambda;l - <em>Beta</em>"))),
  
#     tabsetPanel(id = "tabs",
#       
# 
#     
#     
#     tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 80%; /* or 950px */}"))
#     ),
#  dalliancROutput("dalliance",width="100%", height="100%")
,dalliancROutput("dalliance",width="100%", height="100%") 
)
)