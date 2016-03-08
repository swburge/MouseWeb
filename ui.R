
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("MCH Mouse Data"),
  
  
  textInput("geneSymbol", "Gene Symbol:"),
  

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Enter standard gene Symbols. To enter more than one, place a space between each gene symbol."),
      #sliderInput("bins",
      #            "Number of bins:",
      #            min = 1,
      #            max = 50,
      #            value = 30)
      radioButtons("cellType", label = h3("Radio buttons"),
                   choices = list("All cell types" = 1, "Rs26" = 2,
                                  "TS_GFP" = 3),selected = 1)#,
      #fluidRow(column(3, verbatimTextOutput("value")))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text1"),
      plotOutput("plot1")
    )
  )
))