
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML(("TSPort&lambda;l - <em>Beta</em>"))),
  
    tabsetPanel(id = "tabs",
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
          #         #Code for allowing users to generate Circos plots, with their genes of interest labelled
          #         #Currently commented out as it's very slow and of debatable utility. 
          #         #,
          #         #actionButton("goButton", "Make ideogram"),
          #         #checkboxInput("checkHeat",label = "Add expression heatmap",value=FALSE)
        ),
        mainPanel(
          plotOutput("plotRNASeq"),
          
          #Annoying hack to stop window scroll bars appearing:
          tags$style(type="text/css", ".tab-content { overflow: visible; }")
          )
      ),
    style = "overflow:hidden; width=50%") ,

    
    tabPanel("ChIPSeq", value = "CHIPSEQ",
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
            
            #imageOutput("circosChIPImage", width = "50%", height = "50%"),
            tags$style(type="text/css", ".tab-content { overflow: visible; }")
          )
        )
      )
    ,tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 80%; /* or 950px */}"))
    ),
  dalliancROutput("dalliance",width="100%", height="100%")
  
))
