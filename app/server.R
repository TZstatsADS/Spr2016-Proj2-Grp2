library(shiny)
library(shinydashboard)
library(ggvis)
library(ggplot2)
x = c("leaflet","dplyr","data.table")
lapply(x,library,character.only = TRUE)
setwd("~/Documents/stat4249/project2-project2-group2/data")
load("miceData.RData")
load("zrating.RData")
source("../lib/drawMap.R")
shinyServer<-function(input,output,session) {
  
  data <-reactive({miceData #%>%
    #filter(Zip %in% input$text) %>%
    #filter(Date >= input$timeRange[1] & Date <= input$timeRange[2])
  })
  
  datar<-reactive({zip.rating%>% 
      filter(zipcode %in% input$text)%>%
      filter(punishment == 1)%>%
      filter(score == 0)
  })
  
  
  
  
  
  filtData1 <-reactive({
    df <- data()
    subset(df,
           Zip %in% input$text & Year <= input$EndYear & Year >= input$StartYear)
  })
  
  filtData <- reactive({
    df <- data()
    subset(df,
           Date >= input$timeRange[1] & Date <= input$timeRange[2]
    )
  })
  
  output$map<-renderLeaflet({leaflet(filtData1)%>%
      addProviderTiles("CartoDB.DarkMatter")%>%
      addCircleMarkers(lng = filtData()$Longitude,lat = filtData()$Latitude,radius = 3, stroke = FALSE,col = "yellow",clusterOptions = markerClusterOptions())
  })
  
  output$map1<-renderLeaflet( {leaflet(filtData1) %>%
      addProviderTiles("CartoDB.DarkMatter")%>%
      addCircleMarkers(lng = filtData1()$Longitude,lat = filtData1()$Latitude,radius = 3, stroke = FALSE,col = "yellow")
  })
  
  output$timeBox <- renderValueBox({
    df <- filtData()
    valueBox(
      1+as.numeric(
        difftime(input$timeRange[2],input$timeRange[1],
                 units=c("days"))), "Days", icon = NULL,
      color = "black")
  })
  
  output$likeBox <- renderInfoBox({
    infoBox(
      if(input$count == 0){
        "?"
      }else{"Yes"},
      "Likes", icon =icon("thumbs-up", lib = "glyphicon"),
      color = "maroon"
    )})
  
  output$ratnumBox <- renderValueBox({
    df <- filtData()
    valueBox(
      format(nrow(df), format="d", big.mark=","), "Rats", icon = NULL,
      color = "black")
  })
  
  output$KSIBox <- renderValueBox({
    df <- filtData()
    valueBox(
      round(nrow(df)/
              (1+as.numeric(
                difftime(input$timeRange[2],input$timeRange[1],
                         units=c("days")))),digits=2), "Average per day", icon = NULL,
      color = "black")
  })
  
  ratsday<-reactive({ 
    df <- filtData()
    df %>%
      group_by(Date) %>%
      summarise(count = n()) %>% 
      ggvis(~Date, ~count, fill := "black") %>%
      # mutate(Date = reorder(Date, Date)) %>%
      layer_points()%>%
      layer_smooths(span =0.3,stroke := 'yellow')%>%
      #add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      hide_legend("fill") %>% 
      set_options(width="auto")%>%
      add_tooltip(function(filtData)(filtData$Date))
  })
  ratsday %>% bind_shiny("ratsday") 
  
  ratsyear<-reactive({ 
    df <- filtData()
    df %>%
      filter(Year %in% c(2010,2011,2012,2013,2014,2015))%>%
      group_by(Year) %>%
      summarise(count = n()) %>% 
      ggvis(~Year, ~count, fill := "black") %>%
      mutate(Year = reorder(Year, Year)) %>%
      layer_bars(stroke := "white")%>%
      #layer_smooths(span =0.3,stroke := 'yellow')%>%
      #add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      hide_legend("fill") %>% 
      set_options(width="auto")%>%
      add_tooltip(function(filtData)(filtData$Year))
  })
  ratsyear %>% bind_shiny("ratsyear")
  
  ratsmonth<-reactive({ 
    df <- filtData()
    df %>%
      group_by(Month) %>%
      summarise(count = n()) %>% 
      ggvis(~Month, ~count, fill := "black") %>%
      mutate(Month = reorder(Month, Month)) %>%
      layer_bars(stroke := "white")%>%
      #layer_smooths(span =0.3,stroke := 'yellow')%>%
      #add_axis("x", title = "", properties = axis_props(labels=list(angle=270, align="right"))) %>%
      add_axis("y", title = "", format='d') %>%
      hide_legend("fill") %>% 
      set_options(width="auto")%>%
      add_tooltip(function(filtData)(filtData$Month))
  })
  ratsmonth %>% bind_shiny("ratsmonth")
  
  output$txtrankingresult<-renderText({
    paste("Your ranking result is: ",
          datar()$"2016")
  })
  
  output$plot.c<-renderPlot({
    datac<-zip.rating%>% 
      filter(zipcode %in% c(input$text,input$czipcode))%>%
      filter(punishment == input$zipcode.compare)%>%
      filter(score == input$ranking.compare)%>%
      select(c(1:8))
    datac.p<- melt(datac,id.vars='zipcode')
    x=ggplot(datac.p, aes(x = variable, y = value, group = zipcode, colour = zipcode)) + 
      geom_line() 
    print(x)
  })
  
  output$plot.p<-renderPlot({
    if(input$ranking.g == 0){
      
      datap<-zip.rating%>% 
        arrange(zip.rating$"2016") %>%
        filter(punishment == input$zipcode.compare.g)%>%
        filter(score == input$ranking.g)%>%
        slice(1:10)%>%
        select(c(1:8))
      
    }
    
    else{
      datap<-zip.rating%>% 
        arrange(desc(zip.rating$"2016")) %>%
        filter(punishment == input$zipcode.compare.g)%>%
        filter(score == input$ranking.g)%>%
        slice(1:10)%>%
        select(c(1:8))
    }
    
    datap.p<-melt(datap,id.vars='zipcode')
    x=ggplot(datap.p, aes(x = variable, y = value, group = zipcode, colour = zipcode)) + 
      geom_line() 
    print(x)
  })
  
  'output$rankimage<-renderImage({
    src="rat_welcome.png" 
    height = 20
    width = 20
  })'
  
  
}
