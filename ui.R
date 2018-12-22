#11810105_Isha Rukhaya
#11810058_Nitin Agarwal
#11810013_Ajay Chopra
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinyWidgets)
library(shiny)
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)
library(stringr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$style(
    HTML(
      "
      .tabs-above > .nav > li[class=active] > a {
      background-color: #000;
      color: #FFF;
      }"
)
    ),

# Application title
titlePanel("NLP using UDPipe"),

#Sidebar with 3 input options : Upload file and model file and checkbox choice for xpos tagging
sidebarLayout(
  sidebarPanel(
    helpText(
      "Lets start text analysis by uploading a text file and specify Language"
    ),
    
    
    ## Language selection radio button
    awesomeRadio(
      inputId = "lang",
      label = "Please select Language of Text ",
      choices = c("en", "other"),
      selected = ("en"),
      inline = TRUE,
      status = "warning"
    ),
    
    
    ### Upload utilities
    
    fileInput("file", "Upload text file(only .txt file)"),
    fileInput("modelfile", "Upload model file for other than English language"),
    
    
    
    
    ## Checkbbox input for pos tag filtering
    
    awesomeCheckboxGroup(
      inputId = "xposchoice",
      label = "Choose Xpos tags: It will filter over Annotation and Cooccurence graph only",
      choices = c("JJ", "NN", "NNP", "RB", "VB"),
      selected = list("JJ", "NN", "NNP")
      
    )
    
  ),
  
  
  # Main Panel
  mainPanel(
    setBackgroundImage(src = "https://png.pngtree.com/thumb_back/fw800/back_pic/03/59/27/6957a41f6e761b3.jpg"),
    tabsetPanel(
      type = "tabs",
      tabPanel(
        title = "Introduction",
        h4(
          'This App displays step by step process of doing NLP using udpipe model.UDPipe provides the standard NLP functionalities of tagging, parsing and dependency evaluations - all within R'
        ),
        br(),
        h4(p("Follow tabs sequence from left to Right")),
        h4(
          p(
            "Please upload Text file and Model File from the left panel and see the file content in this tab
            and then click the next tab."
          ),
          div("This app doesn't require model to be uploaded for English .", style =
                "color:blue")
          ),
        br(),
        uiOutput("text")
    )
    
    ,
    tabPanel("Annotated text", dataTableOutput("anntext"))
    ,
    tabPanel("Nouns extraction", dataTableOutput("phrasextractnoun")),
    tabPanel("Verbs extraction", dataTableOutput("phrasextractverb"))
    ,
    tabPanel(
      title = "Collocations",
      h4(
        "Collocations are Sequence of words or terms that co-occur more often than would be expected by chance."
      ),
      dataTableOutput("collocations")
    )
    ,
    tabPanel(
      "Cooccurrences of Noun and Adjectives",
      dataTableOutput("cooccurrences_noun_adj")
    )
    ,
    tabPanel(
      "Cooccurrences at the file level",
      dataTableOutput("gen_cooccur")
    )
    ,
    tabPanel(
      "Skipgram based Co-occurrences",
      dataTableOutput("cooccur_skips_ngrams")
    )
    ,
    tabPanel("Cooccurence graph", plotOutput("plot1"))
      )
    )
)
    ))
