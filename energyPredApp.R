library(shiny)
library(shinydashboard)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(ggmap)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
library(sf)
library(sp)
library(usmap)
library(maps)
library(readxl)

# Shiny App URL: https://pisin.shinyapps.io/energyPredApp/

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Static House Data Review", tabName = "past"),
    menuItem("Energy Usage Hotspot", tabName = "monitor"),
    menuItem("Future Prediction", tabName = "predict")
  )
  # Dash Board Tabs
)

body <- dashboardBody(
  
  # My dashboard
  
  tabItems(
    tabItem(tabName = "past",
            h2("Static House Data Review"),
            
            # First tab, 2 visualizations (histogram, boxplot), one data table
            # Attributes selection with radio buttons
            
            fluidRow(
              
              column(3, radioButtons("radio", h3("Select Your Concern"),
                                     choices = list("Appliance" = 1, "Cooling Setpoint" = 2,"House Geometry" = 3),selected = 1)),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),
              splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2")),
              br(),
              h2("House Attribute Data Table"),
              fluidRow(box(DTOutput('table', width = '100%')))
              
            )
    ),
    tabItem(tabName = "monitor",
            h2("Energy Consumption Hotspot"),
            plotOutput("plot3",click="clickMap")
            
            # Second tab, hotspot map visualization
    ),
    tabItem(tabName = "predict",
            h2("Future Prediction"),
            
            # Third tab, parameter modification with sliders and drop down menu
            # visualization with ploygon map
            # interaction: click on map show specific infor that used interested in
            
            box(
              "Please use slider bar to change parameters", br(),"Energy Prediction Unit: kWh",
              sliderInput("temperature_slider", "Temperature Change", -10, 10, 0),
              selectInput("house_setup","Attributes:",choices=c("Original Preference","Energy Saver")),
              sliderInput("daytime_slider", "Time in a Day", 0, 23, 12),
              plotOutput("plot4",click="clickMap2"),
              fluidRow(box(textOutput('textTab3'))),
              fluidRow(box(textOutput('textTab4')))
            )
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = paste('My Dashboard')),
    sidebar,
    body
  ),
  server = function(input, output) {
    
    # read database from path
    newData <- read_excel('database/newData.xlsx')
    rawdata <- read_excel('database/rawdata.xlsx')
    originalData <- read_excel('database/originalData.xlsx')
    energySaverData <- read_excel('database/energySaverData.xlsx')
    plotFinal <- read_excel('database/plotFinal.xlsx')
    lmJuly <- readRDS('database/linear_model.rds')

    
    tab1Data <- reactive({
      if (input$radio == 1) {
        
        
        return(          newData %>% 
                           group_by(in.clothes_dryer) %>%   # select specific variable as grouping reference
                           summarise(Count=n()) %>% 
                           mutate(pct = prop.table(Count)))
      } 
      else if (input$radio == 2) {
        
        
        return(newData %>% 
                 group_by(in.cooling_setpoint) %>%   # select specific variable as grouping reference
                 summarise(Count=n()) %>% 
                 mutate(pct = prop.table(Count)))
      }
      else if (input$radio == 3) {
        
        
        return(newData %>% 
                 group_by(in.geometry_wall_type) %>%   # select specific variable as grouping reference
                 summarise(Count=n()) %>% 
                 mutate(pct = prop.table(Count)))
      }
      
      
    })
    
    tab1label <- reactive({
      if (input$radio == 1) {
        
        
        return(c("Clothes Dryer Appliance","Clothes Dryer Types","Percentage"))
      } 
      else if (input$radio == 2) {
        
        
        return(c("Clothes Dryer Appliance","Clothes Dryer Types","Percentage"))
      }
      else if (input$radio == 3) {
        
        
        
        return(c("Clothes Dryer Appliance","Clothes Dryer Types","Percentage"))
      }
      
      
    })     
    
    tab1variable <- reactive({
      if (input$radio == 1) {
        
        
        return(c("in.clothes_dryer"))
      } 
      else if (input$radio == 2) {
        
        
        return("in.cooling_setpoint")
      }
      else if (input$radio == 3) {
        
        
        
        return("in.cooling_setpoint")
      }
      
      
    })  
    
    #plotData
    
    
    output$table <- renderDT(
      tab1Data() |> 
        datatable()
    )
    output$plot1 <- renderPlot({
      if (input$radio == 1) {
        return(ggplot(tab1Data(), aes(x=in.clothes_dryer, y=pct,label = scales::percent(pct))) + geom_col() + scale_x_discrete(guide = guide_axis(n.dodge=3)) + labs(title="Clothes Dryer Appliance")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + scale_y_continuous(labels = scales::percent)+xlab("Clothes Dryer Types") + ylab("Percentage"))
      } 
      else if (input$radio == 2) {
        
        
        return(ggplot(tab1Data(), aes(x=in.cooling_setpoint, y=pct,label = scales::percent(pct))) + geom_col() + scale_x_discrete(guide = guide_axis(n.dodge=3)) + labs(title="Cooling Setpoints")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + scale_y_continuous(labels = scales::percent)+xlab("Cooling Setpoint") + ylab("Percentage"))
      }
      else if (input$radio == 3) {
        
        
        
        return(ggplot(tab1Data(), aes(x=in.geometry_wall_type, y=pct,label = scales::percent(pct))) + geom_col() + scale_x_discrete(guide = guide_axis(n.dodge=3)) + labs(title="House Geometry")+ geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3) + scale_y_continuous(labels = scales::percent)+xlab("Wall Types") + ylab("Percentage"))
      }
      
      
    })
    
    
    
    output$plot2 <- renderPlot({
      
      if (input$radio == 1) {
        return(ggplot(data=newData) + aes(x=energy_consumption, y=in.clothes_dryer) + geom_point() + geom_boxplot()+xlab("Energy Usage") + ylab("Clothes Dryer Types"))
      } 
      else if (input$radio == 2) {
        
        
        return(ggplot(data=newData) + aes(x=energy_consumption, y=in.cooling_setpoint) + geom_point() + geom_boxplot()+xlab("Energy Usage") + ylab("Cooling Setpoint"))
      }
      else if (input$radio == 3) {
        
        
        
        return(ggplot(data=newData) + aes(x=energy_consumption, y=in.geometry_wall_type) + geom_point() + geom_boxplot()+xlab("Energy Usage") + ylab("Wall Types"))
      }
      
      
      
    })
    
    
    
    
    
    
    
    output$plot3 <- renderPlot({
      register_stadiamaps("3361de07-653a-4211-ac7c-67613b31c4a7")
      
      RawData <- rawdata
      # position array of houses
      selectCol <- c("in.county","in.weather_file_latitude","in.weather_file_longitude","energy_consumption")
      countyPos <- RawData[,selectCol]
      
      
      countyPos <- countyPos %>% 
        group_by(in.county) %>% 
        summarise(aveEnergy=mean(energy_consumption),aveLon=mean(in.weather_file_longitude),aveLat=mean(in.weather_file_latitude))
      
      countyPos <- arrange(countyPos,countyPos$aveEnergy)
      
      us <- c(left = -83.2, bottom = 32.2, right = -78.2, top = 35.7)
      map <- get_stadiamap(us, zoom = 7, maptype = "stamen_toner_lite") %>% ggmap()  # US Map
      
      # Scatter plot on base map
      tab2plot <- map+geom_point(countyPos,mapping=aes(x=aveLon,y=aveLat,color=aveEnergy,size=aveEnergy))+scale_size(range = c(1,30))+scale_colour_gradient(low="green", high="red")
      
      tab2plot
      
      
    },height = 600, width = 1000)
    
    
    
    tab3Data <- reactive({
      if(input$house_setup == "Original Preference"){
        
        
        
        
        return(            tempdf <- originalData %>% 
                             mutate(mean_temperature=mean_temperature+input$temperature_slider) %>% 
                             filter(date_time==input$daytime_slider))
        
      }
      else if (input$house_setup == "Energy Saver"){
        
        return(tempdf <- energySaverData %>% 
                 mutate(mean_temperature=mean_temperature+input$temperature_slider) %>% 
                 filter(date_time==input$daytime_slider))
      }
    })
    
    
    
    
    
    output$plot4 <- renderPlot({
      
      
      sc_counties <- map_data("county","south carolina")
      countyMap <- ggplot(sc_counties) + aes(long,lat, group=group) + geom_polygon(fill = "white", color = "black") 
      
      prediction <- predict(lmJuly,tab3Data())
      result <- data.frame(county=tab3Data()$countyName,pred=prediction)
      
      
      plot4 <- merge(result,sc_counties,all.x=TRUE, by.x="county",by.y="subregion")
      
      b <- c(300, 500, 700, 1000, 1200, 1400, 2000)
      c <- c("skyblue", "green", "yellow", "orange", "purple", "red")
      
      
      tab3plot<-ggplot(plot4) + aes(long,lat, group=group) + geom_polygon(aes(fill= pred), color = "black",) + scale_fill_distiller(direction = 1)+scale_fill_gradientn(colors = c, values = scales::rescale(b),limits=c(300,2000))
      
      tab3plot
      
    })
    
    
    countyNumber <- reactive({
      longlatGroup <- plotFinal[,2:4]
      point <- data.frame(x=input$clickMap2$x,y=input$clickMap2$y)
      
      
      for (i in 1:46){
        temp <- longlatGroup[longlatGroup$group==i,]
        
        check <- as.numeric(point.in.polygon(point.x=point$x,point.y=point$y,pol.x=temp$long,pol.y=temp$lat))
        countyNumber <- 1
        if(check == 1){
          countyNumber <- (i)
          break
        }
        
        
      }
      return(countyNumber)
      
    })
    
    
    
    
    output$textTab3 <- renderText({
      prediction <- predict(lmJuly,tab3Data())
      result <- data.frame(county=tab3Data()$countyName,pred=prediction)
      
      input$clickMap2
      req(input$clickMap2)
      
      str1 <- paste("In ",as.character(result[countyNumber(),1]," ","county"))
      isolate(HTML(paste(str1)))
      
      
    })
    output$textTab4 <- renderText({
      prediction <- predict(lmJuly,tab3Data())
      result <- data.frame(county=tab3Data()$countyName,pred=prediction)
      
      input$clickMap2
      req(input$clickMap2)
      

      str2 <- paste("The predicted energy usage is ",as.character(result[countyNumber(),2]))
      isolate(HTML(paste(str2)))
      
      
    })
    
    
    
  }
)

