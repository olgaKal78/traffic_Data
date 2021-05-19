library(shiny)
library(leaflet)
#library(leaflet.extras2)
library(dplyr)
library(ggplot2)
library(webTRISr)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(formattable)
library(rCharts)
library(reshape2)
library(ggmosaic)
library(DT)
library(leaflet.minicharts)

#webTRISr: A Wrapper Around 'WebTRIS' Traffic Flow API from Highways England

sites<-webtris_sites()
sites<-filter(sites,Status=='Active')
load("data/JTData.RData")
data$id<-1:dim(data)[1]

ui <- dashboardPage(

    dashboardHeader(title = "Highways England Data"),
   
     dashboardSidebar(
          sidebarMenu(
             menuItem("Traffic Flow ",tabName = "tf" ,icon = icon("dashboard")),
             menuItem("Journey Time", tabName = "jt", icon = icon("car"))
          )
     ),
    dashboardBody(  tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
    ),   tabItems(
      tabItem(tabName = "tf",
      fluidRow(column(12,
                           box(
                               background = "olive", width = NULL,height = 500,
                               br(),
                               leafletOutput("mymap",height = 400),
                              
                               ))),
                  fluidRow( dataTableOutput('map_click')),
                  fluidRow( uiOutput('DynSelection'))
),
tabItem(tabName = "jt",
        fluidRow(
          column(12,
                 box(
                   background = "olive", width = NULL,
                   leafletOutput("mymap2",height = 400)))),
                  fluidRow( dataTableOutput('map_click2')),
                   fluidRow(column(4,uiOutput('dateSelection')),column(4,uiOutput('timeSelection'))),
        fluidRow( uiOutput('Flow'))
                  ))))


server <- function(input, output) {
    
    output$mymap = renderLeaflet({leaflet(sites) %>%
        addTiles() %>% 
        addMarkers(lng=~Longitude, lat=~Latitude,layerId = ~Id,clusterOptions = TRUE,
                   popup = ~paste0("<br/>Id: ",Id,"<br/>Description: ",Description, "<br/>Lat: ",
                                   Latitude,"<br/>Long: ",Longitude))})
    
    observeEvent(input$mymap_marker_click, { 
      
        p<-reactive({filter(sites,Id==input$mymap_marker_click[[1]])})
        
        output$map_click <-renderDataTable(p())
        
        
        d<-reactive({webtris_report(sites=p()$Id,start_date=format(input$daterange1,format="%d-%m-%Y"),end_date=format(input$daterange1,format="%d-%m-%Y"),report_type = "daily")})
        
        output$box1 <- renderValueBox({valueBox(value = weekdays(input$daterange1), subtitle="Day of the week",icon = icon("far fa-calendar-alt"))})
        output$box2 <- renderValueBox({valueBox(value = round(sum(na.omit(d()$`Avg mph`))/length(na.omit(d()$`Avg mph`)),2), subtitle="Average Daily Miles per Hour",icon = icon("fas fa-info-circle"))})
        
        output$daily<-renderPlotly({
          data<-d()
          fig<-plot_ly(data,x=~`Time Period Ending`,y=~`0 - 520 cm`,type="scatter",mode="lines",name= "< 5.2 m " )
          fig<-fig%>%add_trace(y=~`521 - 660 cm`,type="scatter",mode="lines",name= "5.2m - 6.6 m " )
          fig<-fig%>%add_trace(y=~`661 - 1160 cm`,type="scatter",mode="lines",name= "6.61 m - 11.6 m " )
          fig<-fig%>%add_trace(y=~`1160+ cm`,type="scatter",mode="lines",name= "> 11.6 m " )
          fig<-fig %>% layout(title="Traffic Flow within the 15 minute time slice",xaxis = list(title= ""), yaxis = list(title="Number of vehicles") ,
                              legend=list(title=list(text='<b> Vechicle: </b>')))
          fig
        })
        
        output$dailytab<-renderFormattable({
          data<-d()
          formattable(data[c(1:3,24)],list( `Total Volume` = color_bar("#FA614B")))
        })
        
        d1<-reactive({f<-webtris_report(sites=p()$Id,start_date=format(input$range[1],format="%d-%m-%Y"), end_date=format(input$range[2],format="%d-%m-%Y"), report_type ="monthly-hourly-aggregate")
        bb<-melt(f[-2],id.vars =c("Month","DayName"))
        bb
        })
        d2<-reactive({f<-webtris_report(sites=p()$Id,start_date=format(input$range2[1],format="%d-%m-%Y"), end_date=format(input$range2[2],format="%d-%m-%Y"), report_type ="monthly-daily-aggregate")
        dd<-melt(f[-c(2,12)],id.vars=c("Month", "DayName" ))
        #dd$Month<-as.yearmon(dd$Month)
        #dd$DayName<-factor(dd$DayName, levels=c("Mon", "Tue", "Wed", "Thur", 
                                              # "Fri", "Sat", "Sun"))
        })
        output$select<-renderUI({
          mydata = d1()
          if(input$radio=="Month"){
            selectInput("m_h","Select ",choices = c(levels(as.factor(mydata$Month))))}
          else{selectInput("m_h","Select ",choices = c(levels(mydata$variable)))}
        })
        
      
        
         output$select2<-renderUI({
          mydata2 = d2()
        list(
          "The Average Daily Traffic is calculated as a simple average for all
traffic flows for all days of the week within the month.",
          if(input$inhours=="24Hours"){
            "24h indicates calculations based
on time period 00:00-23:59"

          }
          else if (input$inhours=="18Hours"){
            "18h indicates calculations based
on time period 06:00-23:59"}
          else if (input$inhours=="16Hours"){
            "16h indicates calculations based
on time period 06:00-21:59
"}
          else if (input$inhours=="12Hours"){
            "12h indicates calculations based
on time period 07:00-18:59
"}      ,
          selectInput("m_h2","Select ",choices = c(levels(as.factor(mydata2$Month))),multiple = TRUE,selected=levels(as.factor(mydata2$Month))[1]))
         
         })
        
        md<-reactive({
          if(input$radio=="Month"){
            d<-filter(d1(),Month==input$m_h)}
          else{d<-filter(d1(),variable==input$m_h)}
        })
        output$myChart <- renderChart({
          if(input$radio=="Month"){
            p2 <- nPlot(value ~ DayName , group = 'variable', data = md(), type = 'multiBarChart')
            p2$addParams(height = 600, dom = 'myChart')
            return(p2)}
          else{
            p2 <- nPlot(value ~ DayName  , group = 'Month', data = md(), type = 'multiBarChart')
            p2$addParams(height = 600, dom = 'myChart')
            return(p2)} 
          
        })
        
        fd<-reactive({
          if(input$radio2=="Flow"){
            d<-filter(d2(),variable==paste0("AverageFlow",substring(input$inhours,0,2))& Month %in% input$m_h2)
           }
          else{d<-filter(d2(),variable==paste0("PercentageOfLargeVehiclesIn",input$inhours) & Month %in% input$m_h2)
         }
        })
         output$avflow<-renderPlotly({
           data=fd()
           data$DayName<-factor(data$DayName, levels=c("Mon", "Tue", "Wed", "Thur", 
                                                   "Fri", "Sat", "Sun"))
         
          ggplot(data =data) +
             geom_mosaic(aes(weight=value, x=product(DayName), fill=Month))
          
         })
         
         output$radar<-renderPlotly({
           data=fd()
           data$DayName<-factor(data$DayName, levels=c("Mon", "Tue", "Wed", "Thur", 
                                                       "Fri", "Sat", "Sun"))
           fig <- plot_ly(
             type = 'scatterpolar',
             fill = 'toself'
           ) 
           fig <- fig %>%
             add_trace(
               r = data$value,
               theta = data$DayName,
               name = data$Month,
               hovertemplate = 'Value: %{r:.2f} <br>Weekday: %{theta}</br> '
             )  
        fig
           
         })
        output$DynSelection<-renderUI({list(fluidRow(
          
          tabBox(
            id = "tabset1",width = 12, side = "left",
            tabPanel("Daily",
                     
                     fluidRow( column(4,dateInput("daterange1", "Date:", value=as.character(Sys.Date()-60),
                                             min  = "2015-05-01",
                                             max    = as.character(Sys.Date()-60))),
                              valueBoxOutput("box1",width = 4),
                             valueBoxOutput("box2",width = 4)
            ),
            br(),
            plotlyOutput("daily"),
            formattableOutput("dailytab")
            
            ),
            tabPanel("Monthly-hourly-aggregate",
                     column(4, airMonthpickerInput(
                       inputId = "range",
                       label = "Select range of months:",minDate=as.Date("2015-05-01"),maxDate=as.character(Sys.Date()-60),
                       range = TRUE, value = c(as.character(Sys.Date()-425),as.character(Sys.Date()-60))
                     ),
                     prettyRadioButtons(
                       inputId = "radio",
                       label = "",
                       icon = icon("check"),
                       choices = c("Month", "Hour"),
                       animation = "tada",
                       status = "default"
                     ),
                     uiOutput("select")
                     ),
                     column(8,showOutput("myChart", "nvd3"))
                     )
            
             ,
             tabPanel("Average Flow",
                       column(4,
                              airMonthpickerInput(
                        inputId = "range2",
                        label = "Select range of months:",minDate=as.Date("2015-05-01"),maxDate=as.character(Sys.Date()-60),
                        range = TRUE, value = c(as.character(Sys.Date()-425),as.character(Sys.Date()-60))
                       ),
                     
                      
                      selectInput("inhours","Average in: ",choices =c('24Hours',"18Hours","16Hours","12Hours")),
                      prettyRadioButtons(
                        inputId = "radio2",
                        label = "",
                        icon = icon("check"),
                        choices = c("Flow", "% of Large Vechicles"),
                        animation = "tada",
                        status = "default"
                      ), uiOutput("select2")
             
          ),
          column(8,plotlyOutput("avflow"),plotlyOutput("radar"))
        ))))})
        
        
    })
    
   
   a<-data%>%
        select(NTIS.Link.Number,Road,Carriageway,NTIS.Link.Description,Link.Length,Start_Lat, Start_Lon,  End_Lat,End_Lon)%>%
        group_by(NTIS.Link.Number)
    roads<-a[-which(duplicated(a)),]
  
    
    output$mymap2 = renderLeaflet({leaflet(roads) %>%
        addTiles() %>%  addCircles(lng=~c(Start_Lon,End_Lon), lat=~c(Start_Lat,End_Lat),layerId = ~NTIS.Link.Number,color = "red", 
                                   stroke=FALSE, fillOpacity = 1,radius = 30,
                                   popup = ~paste0("<br/>Description: ",
                                  NTIS.Link.Description,"<br/>Carriageway: ",Carriageway ))%>% 
        addPolylines(lng=~c(Start_Lon,End_Lon), lat=~c(Start_Lat,End_Lat), popup = ~paste0("<br/>Description: ",
                                                                                           NTIS.Link.Description,"<br/>Carriageway: ",Carriageway ),color = "red",weight = 2)
      })
    
    observeEvent(input$mymap2_shape_click, { 
      
      p<-reactive({filter(roads, NTIS.Link.Number==input$mymap2_shape_click[[1]])})
      
      output$map_click2 <-renderDataTable(p())
      output$dateSelection<-renderUI({dateInput("JT_daterange1", "Date:", value=min(data$Local.Date),
                min  = min(data$Local.Date),
                max    = max(data$Local.Date))})
      dat<-reactive({d<-filter(data,Local.Date==input$JT_daterange1 & NTIS.Link.Number==p()$NTIS.Link.Number )
      })
     
  
      
    output$timeSelection<-renderUI({list(sliderTextInput("inmin","Time Range: ",
                                                                  choices = dat()$Local.Time, selected = dat()$Local.Time))})
    selectID<-reactive({dat()$id[which(dat()$Local.Time==input$inmin[1])]:dat()$id[which(dat()$Local.Time==input$inmin[2])]})
    dat2<-reactive({filter(dat(),id %in% selectID())})
    
    output$addFlowmap<-DT::renderDT(server = FALSE,{
      DT::datatable(
        dat2(),
        extensions = c("Buttons"),
        options = list(scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "csv", text = "Download Current Page", filename = "page",
                 exportOptions = list(
                   modifier = list(page = "current")
                 )
            ),
            list(extend = "csv", text = "Download Full Results", filename = "data",
                 exportOptions = list(
                   modifier = list(page = "all")
                 )
            )
          )
        )
      )
    }
    )
    tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"   
    output$flowMap<-renderLeaflet({
      
      leaflet(width="100%",height = "400px")%>%
        addTiles(tilesURL)%>%addFlows(dat2()$Start_Lon,dat2()$Start_Lat,dat2()$End_Lon,dat2()$End_Lat,
                              flow=dat2()$Fused.Travel.Time,
                              time=dat2()$Local.Time,
                              popup=popupArgs(labels="Fussed Travel Time"),
                              popupOptions=list(closeOnClick=FALSE,autoClose=FALSE))
        
    })
      
                                     
    output$Flow<-renderUI({list(leafletOutput("flowMap"),dataTableOutput("addFlowmap"))})
    })
   

}

shinyApp(ui, server)