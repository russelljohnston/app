library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Can I Live In London?"),
  
  sidebarLayout(
    sidebarPanel(
    

        numericInput("minPr", 
                   label = h6("Min Price pcm (£)"), 
                   min = 0, max = 5000, value = 100, step = 250), 
    
        numericInput("maxPr", 
                 label = h6("Max Price pcm (£)"), 
                 min = 0, max = 5000, value = 1000, step = 250), 
        numericInput("minBed", 
                     label = h6("Min Beds"), 
                     min = 0, max = 5, value = 1, step = 1), 
        numericInput("maxBed", 
                     label = h6("Max Beds"), 
                     min = 0, max = 5, value = 1, step = 1), 
        
        radioButtons("furnished",label = h6(""),
                     choices = list("furnished","unfurnished")),
        
        selectInput("searchR", 
                    label = h6("Search Radius (miles)"), 
                               choices = c('1'='1','3'='3','5'='5','10'='10','15'='15','20'='20','30'='30','40'='40')),
        
        actionButton("do", "Click Me")
  ),
  
    
    mainPanel(
      plotOutput("histrent"),
      textOutput("pErr"),
      textOutput("numTxt"),
      textOutput("meanTxt"),
      textOutput("medTxt")
              )
   )
))
