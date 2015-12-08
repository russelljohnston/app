library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Can I Live In London?"),
  
  sidebarLayout(
    sidebarPanel(width=3,
    
        textInput("location",
                  label = h6("Area Interested in"),
                  value = "Oxford"),
        numericInput("minPr", 
                   label = h6("Min Price pcm (£)"), 
                   min = 0, max = 5000, value = 100, step = 250), 
    
        numericInput("maxPr", 
                 label = h6("Max Price pcm (£)"), 
                 min = 0, max = 5000, value = 2000, step = 250), 
        radioButtons("type",label = h6("Property Type"),
                     choices = list("houses","flats")),
        numericInput("minBed", 
                     label = h6("Min Beds"), 
                     min = 0, max = 5, value = 1, step = 1), 
        numericInput("maxBed", 
                     label = h6("Max Beds"), 
                     min = 0, max = 5, value = 2, step = 1), 
        
        radioButtons("furnished",label = h6(""),
                     choices = list("furnished","unfurnished")),
        
        selectInput("searchR", 
                    label = h6("Search Radius (miles)"), 
                               choices = c('1/4'='0.25','1/2'='0.5','1'='1','3'='3','5'='5','10'='10','15'='15','20'='20','30'='30','40'='40')),
        
        actionButton("do", "Click Me")
  ),
  
    
    mainPanel(
      plotOutput("histrent",width = "100%"),
      textOutput("zoopres"),
      textOutput("pErr"),
      textOutput("priceErr"),
      plotOutput("transtable")
              )
   )
))
