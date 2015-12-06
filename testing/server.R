library(shiny)
library(httr)
library(XML)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  observeEvent(input$do, {
    
    
    output$histrent <- renderPlot({
      
      bedMin <- reactive({bMi=input$minBed})
      bedMax <- reactive({bMa=input$maxBed})
      
      furnished       <- reactive({fur=input$furnished}) 
      includeRented   = "true"
      pageSize        = "100"
      priceFreq       = "per_month"
      priceMin        <- reactive({pMi=input$minPr}) 
      priceMax        <- reactive({pMa=input$maxPr}) 
      searchRad       <- reactive({sRad=input$searchR})
      
      # loc="Oxford%20Station%2C%20Oxfordshire"
      loc="Reading%20Station"
       
      url <- "http://www.zoopla.co.uk/to-rent/flats/station/rail/oxford/"
      response <- GET(url,query=list(
        beds_max=bedMax(),
        beds_min=bedMin(),
        furnished_state=furnished(),
        include_rented=includeRented,
        page_size=pageSize,
        price_frequency=priceFreq,
        price_min=priceMin(),
        price_max=priceMax(),
        q=loc,
        radius=searchRad()
      ))
      
      html <- htmlTreeParse(response, useInternalNodes = T)
      rootNode <- xmlRoot(html)
      listing <- xpathSApply(html,"//div[@class='listing-results-right']",xmlValue)
      prices <- gsub("^.*\\£ *(.*) * pcm.*$", "\\1", listing)
      prices <- as.numeric(gsub(",","", prices))
      meanPrice <- mean(prices)
      medPrice <- median(prices)
      
      if(length(prices) < 5) {
        output$pErr <- renderText({ 
          paste("less than 5 properties found in this price range. Try increasing Max Price")
        })
        output$meanTxt <- renderText({paste("")})
        output$medTxt  <- renderText({paste("")})
      } else {
        
        hist(prices,main="Results from Zoopla  (100 maximum entries) ",breaks = 10,col="green",prob=FALSE)
        #     # lines(density(prices))
        abline(v=medPrice,col="blue",lwd = 3)
        abline(v=meanPrice,col="red",lwd = 3)
        output$pErr    <- renderText({paste("")})
        output$numTxt  <- renderText({paste("Total number of properties retrieved from Zoopla = ",length(prices)) })
        output$meanTxt <- renderText({paste("Average Price = ",as.integer(meanPrice)) })
        output$medTxt  <- renderText({paste("Median Price   = ",medPrice)})
      }
    })
    
  })
})
