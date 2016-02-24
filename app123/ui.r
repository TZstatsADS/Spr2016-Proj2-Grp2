library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggvis)
library(ggplot2)
library(reshape2)

load("miceData.RData")
load("zrating.RData")
load("resColumbia.RData")



header<-dashboardHeader(title = "Rats in New York",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Support",
                                 message = "Welcome to RatsinNewYork!"
                               )
                  ))
sidebar<-dashboardSidebar(
        sidebarMenu(
          menuItem("Overview", tabName = "overview", icon = icon("sitemap")),
          menuItem("Map of rats", icon = icon("map-marker"), tabName = "map"),
          menuItem("Analysis", tabName = "analysis", icon = icon("bar-chart"),
                   textInput("text", label = h4("Zipcode"), 
                             value = "10001"),
                   menuSubItem("Your zip code",tabName="zipcode",icon=icon("paw")),
                   menuSubItem("Restaurants",tabName="restaurants",icon=icon("cutlery"))
                   ),
          menuItem("About us", tabName = "about", icon = icon("reddit-alien"))
          
          
        ))

body<-dashboardBody(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
            tags$title("Rats in New York")),
  
  tabItems(
    tabItem(tabName = "overview",
               box(
                 width = 12,
                img(src="rat_welcome.png", height = 200, width = 400),
                h1("Welcome to city of rats"),
                hr(),
                p("RatsinNewYork is a ",
                  a(href = 'http://shiny.rstudio.com', 'Shiny'),
                  "web application for dsitribution of rats in the New York and analysis.Here you could find the information of rats distribution
                  from 2010 to 2016"
                ),
                p("Click", em("Map of rats"), " in the sidepanel to start exploring the rats distribution in the New York.",br(), 
                    "Click", em("Analysis"), " in the sidepanel to see the your local district situation and the trend of rats distribution"),
                br(),
                br(),
                p("Data Source:",
                  a(href = 'https://data.cityofnewyork.us/Social-Services/Rat-Sightings/3q43-55fe', 'Rat-sighting'),
                br(),"Repo here: ",
                    a(href = "https://github.com/TZstatsADS/project2-project2-group2", icon("github")),
                br(),
                "Copyright 2016 By",
                  span("STAT4249 Project 2 Group 2", style = "color:blue"))
                
                )
              ),
    tabItem(
      tabName = "about",
      box(width = 12,
      h1("About us"),
      hr(),
      fluidRow(
      # A static valueBox
      valueBox(5, "Columbia students", icon = icon("group")),
      valueBox(21,"Days for the project",icon = icon("calendar"),color = "purple"),
      valueBox("Statistics","Major",icon = icon("graduation-cap"),color = "yellow")
      ),
        h1("Contact us"),
        hr(),
        #title = "Team Members", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
        
        
          column(width = 6,
        p("Li, Chi: cl3406@columbia.edu"),
        p("Liu, Mengying: ml3810@columbia.edu"),
        p("Wang, Danmo: dw2700@columbia.edu"),
        p("Zhang, Yimin: yz2827@columbia.edu"),
        p("Zheng, Yanyu: yz2690@columbia.edu")),
      

        
       fluidRow(infoBoxOutput("likeBox"),
      # Clicking this will increment the progress amount
       actionButton("count", "Like!",color="white"))
        )
    ),
    
    tabItem(
      tabName="zipcode",
      tabBox(width = NULL,
             tabPanel("Map Distribution",
                      leafletOutput("map1",height = 800),
                      absolutePanel(
                        top = 125, right = 35, width = 150, height = 200,
                        draggable = TRUE,
                        wellPanel(
                          h4("Time range"),
                          selectInput("StartYear", h5("Start year:"), 
                                      choices = list("2010" = 2010, "2011" = 2011,
                                                     "2012" = 2012,"2013"=2013,"2014"=2014,"2015"=2015,"2016"= 2016), selected = 2010),
                          selectInput("EndYear", h5("End year:"), 
                                      choices = list("2010" = 2010, "2011" = 2011,
                                                     "2012" = 2012,"2013"=2013,"2014"=2014,"2015"=2015,"2016"= 2016), selected = 2016)),
                        style = "opacity: 0.80"
                      )
                     ),
             tabPanel("Ranking",
                     fluidRow(column(width =6,
                           box(
                               title = "Your Ranking", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                               textOutput("txtrankingresult")
                              # imageOutput("rankimage")
                               ), 
                           box(
                             title="Compare With Other Neightborhood", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                             textInput("czipcode", label = p("Input another code "), 
                                       value = "10010"),
                              radioButtons("zipcode.compare", 
                                              label = p("Chose the rank type"), 
                                              choices = list("Weighted" = 1, 
                                                             "Unweighted" = 0),
                                              selected = 1),
                             radioButtons("ranking.compare", 
                                          label = p("Chose the outcome type"), 
                                          choices = list("Ranking" = 0, 
                                                         "Number of rats" = 1),
                                          selected = 0),
                             
                             plotOutput("plot.c")
                             )
                          ),
                          column(width=6,
                                 box(
                                   title="General Ranking",status="success",solidHeader=TRUE,collapsible = TRUE,width = NULL,
                                   radioButtons("zipcode.compare.g", 
                                                label = p("Chose the rank type"), 
                                                choices = list("Weighted" = 1, 
                                                               "Unweighted" = 0),
                                                selected = 1),
                                   radioButtons("ranking.g", 
                                                label = p("Chose the outcome type"), 
                                                choices = list("Ranking" = 0, 
                                                               "Number of rats" = 1),
                                                selected = 0),
                                   selectInput("year.p", p("Year:"), 
                                               choices = list("2010" = 2010, "2011" = 2011,
                                                              "2012" = 2012,"2013"=2013,"2014"=2014,"2015"=2015), selected = 2015),
                                   plotOutput("plot.p")
                                 )
                                 
                                 )
                                  )
                                                 
             ))
     ),
    tabItem (
      tabName="restaurants",
      leafletOutput("mapfood", height = 800)
    )
    ,
    tabItem(
      tabName="map",
      fluidRow(
        column(width = 6,
               box(width = NULL,
                   
                   valueBoxOutput("ratnumBox"),
                   valueBoxOutput("timeBox"),
                   valueBoxOutput("KSIBox"),
                   
                   leafletOutput("map", height = 800))
        ),
        column(width = 6,
               box(width = NULL, 
                   dateRangeInput("Date range", inputId = "timeRange",  
                                  start = "2010-01-01",
                                  end = "2015-12-31",
                                format = "yyyy-mm-dd")),
               tabBox(width = NULL,
                      tabPanel("Days",
                               h5("Rats number vs. Days", style = "color:black", align = "center"),
                               ggvisOutput("ratsday")
                               ),
                      tabPanel("Months",
                               h5( "Rats number vs. Months", style = "color:black", align = "center"),
                               ggvisOutput("ratsmonth")
                               ),
                      tabPanel("Year",
                               h5("Rats number vs. Years", style = "color:black", align = "center"),
                               ggvisOutput("ratsyear")
                               )) )
      ))
      )
    )
    

shingUI<-dashboardPage(skin="black",header,sidebar,body)


