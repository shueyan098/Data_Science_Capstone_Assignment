#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application of word prediction
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Upcoming Word Prediction", align = "center")),
  
  # Application body
  sidebarLayout(
    sidebarPanel(
      ##h3("Please input a phrase."),
      textInput("text1", "Please input a phrase:", "please give me your"),
      
      actionButton("button1", "Submit"),
      p("Click the Submit button to get result.")
    ),
    
    mainPanel(
      h4("Estimated upcoming word:"),
      textOutput("pred1"),
      br(),
      br(),
      br(),
      p("Please allow some delay due to limited machine's computation power. Thank you for your patience.")
    )
  )
))
