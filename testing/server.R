library(shiny)
library(httr)
library(XML)
library(ggplot2)
library(grid)
library(gridExtra)
require(gtable)
library(dplyr)

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
       
      url <- "http://www.zoopla.co.uk/to-rent/flats/station/rail/reading/"
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
      hrefs = xpathSApply(html, '//*[@class="listing-results-attr"]/a',xmlGetAttr,'href')
      stations = unique(xpathSApply(html,"//*[@class='nearby_stations_schools_name']",xmlValue))
      nbed =  data.frame(as.integer(xpathSApply(html, '//*[@class="num-icon num-beds"]',xmlValue)))
      colnames(nbed) = 'number_of_bedrooms'
      
      price = xpathSApply(html,"//div[@class='listing-results-right']",xmlValue)
      # price =  xpathSApply(html, '//*[@class="listing-results-price text-price"]',xmlValue)
      price <- gsub("^.*\\£ *(.*) * pcm.*$", "\\1", price)
      price <- as.numeric(gsub(",","", price))
      
      
      if(length(price) < 5) {
        output$pErr <- renderText({ 
          paste("less than 5 properties found in this price range. Try increasing Max Price")
        })
        output$meanTxt <- renderText({paste("")})
        output$medTxt  <- renderText({paste("")})
      } else {
        
        bed.df <- data.frame(price,nbed)
        bed.df$number_of_bedrooms <- gsub('([0-9])',"\\1 bedroom", bed.df$number_of_bedrooms)
        
        countTot<-aggregate(bed.df[, 1], list(bed.df$number_of_bedrooms), function(x) length(unique(x)))
        colnames(countTot)=c('Number of Bedrooms','Total Number of Properties')
        meanPrice<-aggregate(bed.df[, 1], list(bed.df$number_of_bedrooms), mean)
        colnames(meanPrice)=c('Number of Bedrooms','Average Price (£)')
        medPrice<-aggregate(bed.df[, 1], list(bed.df$number_of_bedrooms), median)
        colnames(medPrice)=c('Number of Bedrooms','Median Price (£)')
        allPrice <- merge(meanPrice,medPrice,by='Number of Bedrooms')
        allPrice <- merge(allPrice,countTot,by='Number of Bedrooms')
        
        g<- ggplot(bed.df, aes(price, fill = number_of_bedrooms)) + geom_density(alpha = 0.5) 
        g<- ggplotGrob(g)  
        g<- gtable_add_rows(g, unit(1.5,"in"), 0) 
        g<- gtable_add_grob(g, tableGrob(allPrice, rows = rownames(allPrice), cols = colnames(allPrice)),
                             t = 1, l=4, b=1, r=4)
        g<- gtable_add_rows(g, unit(1,"in"), 0) 
        histtitle = paste("Properties near",loc,"(sourced from Zoopla)",sep=" ")
        title_style <- g$grobs[[8]]$gp
        g <- gtable_add_grob(g, textGrob(histtitle, x=0, hjust=0, gp=title_style),
                              t=1, l=4, b=1, r=4, name="hist-title")
        
        # grid.newpage()
        # grid.draw(g)
        
        output$pErr    <- renderText({paste("")})
        
        #Now Query Google maps for transit times from this area
        
        #get nearby stations to properties 
        stations = unique(xpathSApply(html,"//*[@class='nearby_stations_schools_name']",xmlValue))
        gStations = paste(stations, collapse = '|') 
        
        
        mode="transit"
        transit_mode="rail"
        to="Farringdon"
        from=gStations
        region="uk"
        key="AIzaSyDi5gaiVz3Eq3xMg6ndUC5X3bhQLxY_F4w" 
        url    <- "https://maps.googleapis.com/maps/api/distancematrix/xml"
        response <- GET(url,query=list(
          origins=from,
          destinations=to,
          region=region,
          mode=mode,
          transit_mode=transit_mode,
          key=key
        ))
        doc      <- content(response,type="text/xml")
        status   <- sapply(doc["//row/element/status"],xmlValue)
        if(any(status!="OK")) warning("Error Status on some routes")
        durationText <- sapply(doc["//row/element/duration/text"],xmlValue)
        durationNum <- as.numeric(sapply(doc["//row/element/duration/value"],xmlValue))/60
        
        print(durationText)
        
        df_train<- data.frame(stations,durationText,as.integer(durationNum))
        colnames(df_train) = c("Stations", "Total Transit time", "(mins)") 
        
        tt <- tableGrob(df_train)
        # grid.newpage()
        grid.arrange(g,tt,nrow=2,ncol=1)
        

      }
    }, height = 700, width = 600)
    
#     output$transtable <- renderPlot({
#       
#       
#     })
    
  })
})
