
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel(HTML(("TSPort&lambda;l - <em>Beta</em>"))),
#   fluidRow(
#     column(3, wellPanel(
#       selectInput("expt_type", "Experiment type", c("RNASeq","ChIPSeq"),selected="RNASeq"
#       )
#     )),
#     column(3, wellPanel(
#       htmlOutput("selectUI")
#      ))
#   
#   ),
  
#  textInput("geneSymbol", "Gene Symbol:"),
  

  # Sidebar with a slider input for number of bins
  sidebarLayout(
   
    sidebarPanel(
      selectInput(
        "exptType", "Experiment Type",
        c(RNASeq = "rnaseq",
          ChIPSeq = "chipseq")),
      
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.exptType == 'rnaseq'",
        textInput("geneSymbol", "Gene Symbol:"),
        helpText("Enter standard gene Symbols. To enter more than one, place a space between each gene symbol."),
        selectInput("precompiled", label=h4("Some precompiled lists:"),
                    choices = list(" "= 1,"Top 20 Superstem" = 2, "Top 20 Upregulated" = 3)),
        radioButtons("dataType", label = h4("Data type"),
              choices = list("Log fold change" = 1, "Normalized counts" = 2),selected = 1
          ),
        checkboxGroupInput("cellType", 
              label = h4("Cell types"), 
              choices = list("All" = 1, "TS_Rs26" = 2, "TS_EGFP" = 3), selected = 1
          ),
        
        downloadButton('downloadData', 'Download current data'),
        actionButton("goButton", "Make ideogram")
        
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text1"),
      #textOutput("text"),
      plotOutput("plot2"),
      imageOutput("circosImage",width="25%",height="25%")
      
      #mainPanel(textOutput("text"),textOutput("db_select"))
    )
  )
))
