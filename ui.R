
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Text prediction for typing"),
  h3('Input a phrase and press go and the app will anticipate the next word'),
  h3(''),
  
  sidebarLayout(position = "right",
      sidebarPanel(
          h3('The next word is:'),
          textOutput("nextword"),
          br(),
          br(),
          br(),
          h3('Prediction Tally correct/Total:'),
          textOutput("tally"),
          br()
      ),
  mainPanel(
        textInput("iText",label=h3("Enter your phrase here:"),value=("Enter text...")),
        actionButton("predictnow", label = "Submit Phrase"),
        
        h3('Please provide us with feedback: was the prediction correct?'),
        
        radioButtons('Accuracy',"The Prediction was", choices=list("Correct"=1,"Incorrect"=.0),selected=1),
        actionButton("Scorefeedback", label = "Submit Feedback")
        
        
    )
  ) ) )

