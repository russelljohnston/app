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
    
    bedMin     <- reactive({bMi=input$minBed})
    bedMax     <- reactive({bMa=input$maxBed})
    furnished  <- reactive({fur=input$furnished})
    priceMin   <- reactive({pMi=input$minPr/4.})
    priceMax   <- reactive({pMa=input$maxPr/4.})
    searchRad  <- reactive({sRad=input$searchR})
    loc        <- reactive({locc=input$location})
    propType   <- reactive({pTy=input$type})
  
    
    key <- "fzzfvsuf37rgy8k2vfruxcwd"
    
    listing_status="rent"
    url="http://api.zoopla.co.uk/api/v1/property_listings.xml"
    
    sample <- GET(url,  query = list(
      area = loc(),
      listing_status="rent",
      page_size=100,
      page_number=1,
      property_type=propType(),
      furnished=furnished(),
      minimum_beds=bedMin(),
      maximum_beds=bedMax(),
      minimum_price=priceMin(),
      maximum_price=priceMax(),
      radius=searchRad(),
      summarised="true",
      order_by="age",
      include_rented=1,
      api_key = key))
    
    result   <- content(sample)
    rootNode <- xmlRoot(result)
    price    <- data.frame(as.numeric(xpathSApply(rootNode,"//rental_prices/per_month",xmlValue)))
    colnames(price) = 'price'
    nbed     <- data.frame(as.integer(xpathSApply(rootNode,"//num_bedrooms",xmlValue)))
    colnames(nbed) = 'number_of_bedrooms'
    listUrl  <- xpathSApply(rootNode,"//details_url",xmlValue)
    
    output$histrent <- renderPlot({
      
      validate(
        need(priceMax()-priceMin() >=0., "'Max Price' must be greater than or equal to 'Min Price' "),
        need(bedMax()-bedMin() >=0., "'Max Beds' must be greater than or equal to 'Min Beds' "),
        need(nrow(price)>5, "Less than 5 properties found in this price range. Try increasing Max Price" )
      )
      
      
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
      
      
      histtitle = paste(propType()," near",loc(),"(sourced from Zoopla)",sep=" ")
      title_style <- g$grobs[[8]]$gp
      g <- gtable_add_grob(g, textGrob(histtitle, x=0, hjust=0, gp=title_style),
                           t=1, l=4, b=1, r=4, name="hist-title")
      
      # grid.newpage()
      grid.draw(g)
      # link = response[1]
      
      # output$zoopres <- renderUI({ a(href=link) })
      output$pErr    <- renderText({paste("")})
      
    }, height = 400, width = 600)
    
    #     output$transtable <- renderPlot({
    #       
    #       #Now Query Google maps for transit times from this area
    #       
    #       #get nearby stations to properties 
    #       stations = unique(xpathSApply(html,"//*[@class='nearby_stations_schools_name']",xmlValue))
    #       gStations = paste(stations, collapse = '|') 
    #       
    #       
    #       mode="transit"
    #       transit_mode="rail"
    #       to="Farringdon"
    #       from=gStations
    #       region="uk"
    #       key="AIzaSyDi5gaiVz3Eq3xMg6ndUC5X3bhQLxY_F4w" 
    #       url    <- "https://maps.googleapis.com/maps/api/distancematrix/xml"
    #       response <- GET(url,query=list(
    #         origins=from,
    #         destinations=to,
    #         region=region,
    #         mode=mode,
    #         transit_mode=transit_mode,
    #         key=key
    #       ))
    #       doc      <- content(response,type="text/xml")
    #       status   <- sapply(doc["//row/element/status"],xmlValue)
    #       if(any(status!="OK")) warning("Error Status on some routes")
    #       durationText <- sapply(doc["//row/element/duration/text"],xmlValue)
    #       durationNum <- as.numeric(sapply(doc["//row/element/duration/value"],xmlValue))/60
    #       
    #       print(durationText)
    #       
    #       df_train<- data.frame(stations,durationText,as.integer(durationNum))
    #       colnames(df_train) = c("Stations", "Total Transit time", "(mins)") 
    #       
    #       tt <- tableGrob(df_train)
    #       # grid.newpage()
    #       # grid.arrange(g,tt,nrow=2,ncol=1)
    #       grid.draw(tt)
    #       
    #       
    #     }, height = 200, width = 600)
    
  })
})
