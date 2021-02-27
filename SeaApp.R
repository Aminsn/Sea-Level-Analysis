library(readxl)
library(leaflet)
library(dplyr)
library(lubridate)
library(Jmisc)
library(tidyverse)
library(reshape2)

ui <- fluidPage(
  
  titlePanel("SLC rate in Irish sea (mm/yr)"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Selector for choosing dataset ----
      numericInput(inputId = "date1",
                   label = "From",
                   value = 2000),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "date2",
                   label = "To",
                   value = 2016),
      tableOutput("summary")
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      #verbatimTextOutput("summary"),
      leafletOutput("view"),
      #tableOutput("view"),
      # Output: HTML table with requested number of observations ----
      plotOutput("plot")
      
    )
  )
)

server <- function(input, output) {
  
  
  df <- read_csv("~/Sea Level Analysis/Dublin_Bay_Project-master/IrishSeaComplete.csv")
  df$oriel[which.min(df$oriel)] <- NA
  df$oriel[which.min(df$oriel)] <- NA
  
  #df[,13] <-  df[,13]*1000
  
  points <- read_excel("~/Sea Level Analysis/Dublin_Bay_Project-master/Gaugeslocs.xlsx")
  names(df)[-1] <- points$Location
  points$lng <- points$lng * (-1)
  points$Rate <- rep(1,15)
  
  points$Months <- as.character(rep(0,15))
  
  
  # Generate a summary of the dataset ----
  output$summary <- renderTable({
    
    df2 <- df %>% filter(time >= input$date1 & time <= input$date2)

    for (i in 2:16) {

      x <- unlist(df2[,i])
      y <- unlist(df2[,1])
      
      if (all(is.na(x))){points[i-1 ,4] <- NA}
      else{
      lm.fit <- lm(x ~ y)
      points[i-1 ,4] <- coefficients(lm.fit)[2]
      points[i-1 ,5] <- as.character(length(na.omit(x)))
          } 
     }
        
       points[,3:5]
    
  })
  
  
  # Show the first "n" observations ----
   output$view <- renderLeaflet({
    
     df3 <- df %>% filter(time >= input$date1 & time <= input$date2)
     

     for (i in 2:16) {

       x <- unlist(df3[,i])
       y <- unlist(df3[,1])

       if (all(is.na(x))){points[i-1 ,4] <- NA}
       else{
         lm.fit <- lm(x ~ y)
         points[i-1 ,4] <- coefficients(lm.fit)[2]
       }
     }
     
     points$r <- points$Rate
     points$r[points$r >= 10] <- 10
     points$r[points$r <= -1] <- -1

     colors <- c("green","red")
     pal <- colorNumeric(colors, domain = c(-1:10))
     leaflet() %>% addTiles() %>% setView(-5, 54, zoom = 6) %>%
       addCircleMarkers(data = points, lat = ~ lat, lng = ~ lng , popup = ~Location ,radius = 10,
                        color = ~pal(r))

   })

  
    output$plot <- renderPlot({
      
    #df4 <- df %>% filter(time >= input$date1 & time <= input$date2)  
    loc <- gather(df, Location, value, "Bangor":"Arklow")
    loc <- left_join(loc, points)
    
    loc <- loc %>% filter(lng == input$view_marker_click$lng)
    loc <- loc %>% filter(time >= input$date1 & time <= input$date2 & time >= loc$time[min(which(!is.na(loc$value)))])

          ggplot(loc,aes(time,value)) + geom_line() + theme_bw() + 
        geom_smooth(method='lm', formula= y~x) +
        labs(x = 'Year',
             y = 'Sea Level (mm)') +
        ggtitle(paste0(min(loc$Location)))
    
      }) %>% bindEvent(input$view_marker_click)
  
}

# Run the application
shinyApp(ui = ui, server = server)
