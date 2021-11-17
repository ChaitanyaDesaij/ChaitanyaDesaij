# imports the essential libraries
library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(proj4)
library(plotly)
library(shinydashboard)
library(DT)

# reads the crash data
crash_df <- read.csv('2019_DATA_SA_Crash.csv')


# removing unwanted columns
crash_df$Other.Feat <- NULL
crash_df$Unit.Resp <- NULL
crash_df$UNIQUE_LOC <- NULL
crash_df$Total.Units <- NULL
crash_df$Total.SI <- NULL
crash_df$Total.MI <- NULL
crash_df$Year <- NULL

# converting to character and factor
crash_df$Suburb <- as.character(crash_df$Suburb)
crash_df$Area.Speed <- as.factor(crash_df$Area.Speed)

# setting levels for month and day
crash_df$Month <- factor(crash_df$Month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August' , 'September', 'October', 'November', 'December'))
crash_df$Day <- factor(crash_df$Day, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

# replacing null with N
levels(crash_df$DUI.Involved)[levels(crash_df$DUI.Involved) == ""] <- "N"
levels(crash_df$Drugs.Involved)[levels(crash_df$Drugs.Involved) == ""] <- "N"

# Code spatial points
proj <- "+proj=lcc +lat_1=-28 +lat_2=-36 +lat_0=-32 +lon_0=135 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
points <- proj4::project(crash_df[, c("ACCLOC_X", "ACCLOC_Y")], proj = proj, inverse = TRUE)
crash_df$Longitude <- points$x
crash_df$Latitude <- points$y

# top accident prone suburbs
top_Sub <- crash_df %>%
  group_by(Suburb) %>%
  summarise(count_Sub = n()) %>%
  arrange(desc(count_Sub))
#top 5
top_Sub <- top_Sub[1:5, 1]

# top casualty prone suburbs
cas_sub <- crash_df %>% 
  group_by(Suburb) %>% 
  summarise(Total_Cas = sum(Total.Cas)) %>% 
  arrange(desc(Total_Cas))
#top 5
cas_sub <- cas_sub[1:5, ]

# top fatality prone suburbs
fats_sub <- crash_df %>% 
  group_by(Suburb) %>% 
  summarise(Total_Fats = sum(Total.Fats)) %>% 
  arrange(desc(Total_Fats))
#top 5
fats_sub <- fats_sub[1:5, ]

# creates the ui
ui <- fluidPage(tabsetPanel(
  #first tab
  tabPanel(
    "Accidents",
    fluidRow(h1("Road accidents in South Australia", align="center")),
    # row 2
    fluidRow(
      # heading and descritption row 2 col 1
      column(4, h2("Accidents by Suburb"), 
             p("The plot below shows the five suburbs where the most accidents happened. The most accident prone suburbs are - Adelaide, Mawson Lakes, Prospect, North Adelaide and Gepps Cross. Out of these Adelaide has recorded the most accidents.")),
      # input selectors
      column(8, selectInput("Suburb1", "Choose a suburb", unique(crash_df$Suburb)))),
      
    # row 3
    fluidRow(
      # top5 accident prone suburbs bar chart
      column(4, plotlyOutput("VIS11")),
      # leaflet
      column(8, leafletOutput("MAP1"))),
    
    # row 4
    fluidRow(
      # heading and description row 4 col 1
      column(4, h2("Accident Factors"),
             p("The plot on the right shows the factors that affect accidents. Explore different factors by slecting them from the drop-down menu."),
             selectInput("Factor1", "Choose a factor", c("Area.Speed", "Position.Type", "Horizontal.Align", "Vertical.Align", "Road.Surface", "Moisture.Cond", "Weather.Cond", "DayNight", "Crash.Type", "Entity.Code", "Traffic.Ctrls"))),
      # bar chart for factors vs accidents
      column(8, plotlyOutput("VIS12")))
    ),
  # tab 2
  tabPanel(
    "Casualties",
    fluidRow(h1("Road Casualties in South Australia", align="center")),
    # row 2
    fluidRow(
      # heading and descritption row 2 col 1
      column(4, h2("Casualties by Suburb"), 
             p("The plot below shows the five suburbs where the most casualties happened. The most casualty prone suburbs are - Adelaide, Mawson Lakes, Prospect, North Adelaide and Mount Barker. Out of these Adelaide has recorded the most casualties")),
      # input selectors
      column(8, selectInput("Suburb2", "Choose a suburb", unique(crash_df$Suburb)))),
    
    # row 3
    fluidRow(
      # top5 casualty prone suburbs bar chart
      column(4, plotlyOutput("VIS21")),
      # leaflet
      column(8, leafletOutput("MAP2"))),
    
    # row 4
    fluidRow(
      # heading and description row 4 col 1
      column(4, h2("Casualty Factors"),
             p("The plot on the right shows the factors that affect casualties. Explore different factors by slecting them from the drop-down menu."),
             selectInput("Factor2", "Choose a factor", c("Area.Speed", "Position.Type", "Horizontal.Align", "Vertical.Align", "Road.Surface", "Moisture.Cond", "Weather.Cond", "DayNight", "Crash.Type", "Entity.Code", "Traffic.Ctrls"))),
      # casualty vs factors boxplot
      column(8, plotlyOutput("VIS22")))
  ),
  
  # tab 3
  tabPanel(
    "Fatalities",
    fluidRow(h1("Road Fatalities in South Australia", align="center")),
    # row 2
    fluidRow(
      # heading and descritption row 2 col 1
      column(4, h2("Fatalities by Suburb"), 
             p("The plot below shows the five suburbs where the most fatalities happened. The most fatality prone suburbs are - Truro, Coomandook, Craigmore, Kersbrook and Kudla. Out of these Truro has recorded the most fatalities.")),
      # input selectors
      column(8, selectInput("Suburb3", "Choose a suburb", unique(crash_df$Suburb)))),
    
    # row 3
    fluidRow(
      # top5 fatality prone suburbs bar chart
      column(4, plotlyOutput("VIS31")),
      # leaflet
      column(8, leafletOutput("MAP3"))),
    
    # row 4
    fluidRow(
      # heading and description row 4 col 1
      column(4, h2("Fatality Factors"),
             p("The plot on the right shows the factors that affect fatalities. Explore different factors by slecting them from the drop-down menu."),
             selectInput("Factor3", "Choose a factor", c("Area.Speed", "Position.Type", "Horizontal.Align", "Vertical.Align", "Road.Surface", "Moisture.Cond", "Weather.Cond", "DayNight", "Crash.Type", "Entity.Code", "Traffic.Ctrls"))),
      # fatality vs factors boxplot
      column(8, plotlyOutput("VIS32")))
  ),
  
  #tab 4
  tabPanel(
    "Drugs/DUI",
    fluidRow(h1("The Effect of Drugs and DUI on Accidents", align="center")),
    # row 2
    fluidRow(
      # radio input
      column(4, h2("Drug Influence"), 
             p(" In majority of the accidents drugs were not involved. Median of casualties when drugs were involved = 1 while 0 when drugs were not involved. Fatalities boxplot shows similar distribution for both Y and N in drug involvement."),
             radioButtons("ACF1", "Select:",
                          c("Accidents" = "Accidents",
                            "Casualties" = "Total.Cas",
                            "Fatalities" = "Total.Fats"))),
      # graph for drugs vs input
      column(8, plotlyOutput("VIS41"))),
    # row 3
    fluidRow(
      # radio input
      column(4, h2("DUI Influence"), 
           p(" In majority of the accidents DUI were not involved. Median of casualties when DUI were involved = 1 while 0 when DUI were not involved. Fatalities boxplot shows similar distribution for both Y and N in DUI involvement."),
           radioButtons("ACF2", "Select:",
                        c("Accidents" = "Accidents",
                          "Casualties" = "Total.Cas",
                          "Fatalities" = "Total.Fats"))),
      # graph for DUI vs input
      column(8, plotlyOutput("VIS42"))),
    
    # row 4
    fluidRow(h2("Drugs and DUI versus the Serverity of Accidents"),
             p("The tile maps below show the relation between accident severity level and drugs and DUI involvement in accidents. The number of accidents decreased in severity when no DUI or drug was involved but remained constant when DUI/Drugs were involved.")),
  
    # row 5
    # tile graph for dui vs severity and drugs vs severity
    fluidRow(column(6, plotlyOutput("VIS43")),
      column(6, plotlyOutput("VIS44")))),
  
  # tab 5 - dataset
  tabPanel(
        "Dataset",
        dataTableOutput("my_data")
  )
 )
)

# creates the server
server <- function(input, output) {
  # top 5 accident prone suburb bar chart
  output$VIS11 <- renderPlotly({
    ggplot(data = crash_df[crash_df$Suburb %in% top_Sub$Suburb, ]) +
      geom_bar(mapping = aes(x = Suburb)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Suburb", y = "Accidents", title = "Most accident prone Suburbs")
  })
  
  # leaflet - adding tiles, markers, clustering markers
  output$MAP1 <- renderLeaflet({
    leaflet(data = crash_df[crash_df$Suburb == input$Suburb1, ]) %>% addTiles() %>%
      addMarkers(
        ~Longitude, 
        ~Latitude,
        popup = ~paste("<b>Unit Responsible:</b>", Entity.Code, "<br/>",
                       "<b>Local Government Authority:</b>", LGA.Name, "<br/>",
                       "<b>Position Type:</b>", Position.Type, "<br/>",
                       "<b>Postcode:</b>", Postcode, "<br/>",
                       Month, Day, Time),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # bar chart for factors vs accidents
  output$VIS12 <- renderPlotly({
    ggplot(data = crash_df) +
      theme(axis.text.x = element_text(angle = 90)) +
      geom_bar(mapping = aes(x = .data[[input$Factor1]])) +
      labs(y = "Accidents", title = paste("Accidents by", input$Factor1))
  })
  
  # top 5 casualty prone suburb bar chart 
  output$VIS21 <- renderPlotly({
    ggplot(data = crash_df[crash_df$Suburb %in% cas_sub$Suburb, ]) +
      geom_bar(mapping = aes(x = Suburb)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Suburb", y = "Casualties", title = "Most casuality prone Suburbs")
  })
  
  # leaflet - adding tiles, markers, clustering markers
  output$MAP2 <- renderLeaflet({
    leaflet(data = crash_df[crash_df$Suburb == input$Suburb2, ]) %>% addTiles() %>%
      addMarkers(
        ~Longitude, 
        ~Latitude,
        popup = ~paste("<b>Unit Responsible:</b>", Entity.Code, "<br/>",
                       "<b>Local Government Authority:</b>", LGA.Name, "<br/>",
                       "<b>Position Type:</b>", Position.Type, "<br/>",
                       "<b>Postcode:</b>", Postcode, "<br/>",
                       Month, Day, Time),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # boxplot for factors vs casualties
  output$VIS22 <- renderPlotly({
    ggplot(data = crash_df) +
      geom_boxplot(mapping = aes(x = .data[[input$Factor2]], y = Total.Cas)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(y = "Casualties", title = paste("Casualties by", input$Factor2))
  })
  
  # top 5 fatality prone suburb bar chart 
  output$VIS31 <- renderPlotly({
    ggplot(data = crash_df[crash_df$Suburb %in% fats_sub$Suburb, ]) +
      geom_bar(mapping = aes(x = Suburb)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Suburb", y = "Fatalities", title = "Most fatality prone Suburbs")
  })
  
  # leaflet - adding tiles,  markers, clustering markers
  output$MAP3 <- renderLeaflet({
    leaflet(data = crash_df[crash_df$Suburb == input$Suburb3, ]) %>% addTiles() %>%
      addMarkers(
        ~Longitude, 
        ~Latitude,
        popup = ~paste("<b>Unit Responsible:</b>", Entity.Code, "<br/>",
                       "<b>Local Government Authority:</b>", LGA.Name, "<br/>",
                       "<b>Position Type:</b>", Position.Type, "<br/>",
                       "<b>Postcode:</b>", Postcode, "<br/>",
                       Month, Day, Time),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # boxplot for factors vs fatalities
  output$VIS32 <- renderPlotly({
    ggplot(data = crash_df) +
      geom_boxplot(mapping = aes(x = .data[[input$Factor3]], y = Total.Fats)) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(y = "Fatalities", title = paste("Fatalities by", input$Factor3))
  })
  
  # if user selects accidets he is shown a bar chart
  output$VIS41 <- renderPlotly({
    if (input$ACF1 == "Accidents"){
      ggplot(data = crash_df) +
        geom_bar(mapping = aes(x = Drugs.Involved)) +
        labs(x = "Drugs Involved", y = "Accidents", title = "Drugs invovlvement in accidents")
    }
    # if user selects casualty or fatality he is shown boxplot
    else {
      ggplot(data = crash_df) +
        geom_boxplot(mapping = aes(x = Drugs.Involved, y = .data[[input$ACF1]])) +
        labs(x = "Drugs Involved", title = paste(input$ACF2, "due to drug involvement"))
    }
  })
  
  # if user selects accidets he is shown a bar chart
  output$VIS42 <- renderPlotly({
    if (input$ACF2 == "Accidents"){
      ggplot(data = crash_df) +
        geom_bar(mapping = aes(x = DUI.Involved)) +
        labs(x = "DUI Involved", y = "Accidents", title = "DUI invovlvement in accidents")
    }
    # if user selects casualty or fatality he is shown boxplot
    else {
      ggplot(data = crash_df) +
        geom_boxplot(mapping = aes(x = DUI.Involved, y = .data[[input$ACF2]])) +
        labs(x = "DUI Involved", title = paste(input$ACF2, "due to DUI involvement"))
    }
  })
  
  # tile chart for drugs vs severity
  output$VIS43 <- renderPlotly({
    crash_df %>% 
      count(Drugs.Involved, CSEF.Severity) %>%  
      ggplot(mapping = aes(x = Drugs.Involved, y = CSEF.Severity)) +
      geom_tile(mapping = aes(fill = n))
    
  })
  
  # tile chart for DUI vs severity
  output$VIS44 <- renderPlotly({
    crash_df %>% 
      count(DUI.Involved, CSEF.Severity) %>%  
      ggplot(mapping = aes(x = DUI.Involved, y = CSEF.Severity)) +
      geom_tile(mapping = aes(fill = n))
  })
  
  
  # fifth tab - crash dataset
  output$my_data <- DT::renderDataTable({
    datatable(crash_df)
  })
}

# run the app
shinyApp(ui, server)

