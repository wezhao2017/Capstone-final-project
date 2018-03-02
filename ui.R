
library(shiny)


shinyUI(
        fluidPage(
                titlePanel("Predict the Next Word"),
                sidebarLayout(
                        sidebarPanel(
                                helpText(""),
                                hr(),
                                textInput("inputText", "Enter a word to predict the next one",value = ""),
                                hr(),
                                helpText(strong("Synopsis"), 
                                         hr(),
                                         "This application try to predict the next possible word in a sentence.",
                                         hr(),
                                         "Type word(s) on the text box and up to 4 possible next words will display below."),
                                hr()
                                ),
                        mainPanel(
                                h3("Prediction Word(s) at below Box"),
                                verbatimTextOutput("prediction"),
                                 hr(),
                                h3("You enter the word(s)"),
                                strong(code(textOutput('sentence1'))),
                                br(),
                                hr()
                                )
                        )
                )
        )