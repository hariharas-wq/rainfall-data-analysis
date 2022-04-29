library(shiny)   #Used for implementing Shiny framework for web app
library(leaflet) #Used for implementing interactive maps in the web app.
library(dplyr)   #Used for fast data manipulation form the memory.
library(magrittr) #Used for pipelining of instructions.
library(sf)      #Provides a standardized way to encode spatial vector data. Used for manipulation and projection of geographical data. 
library(raster)  #Used for geographic data analysis and modeling.
library(sp)      #Provides functions for for spatial data.
library(shinydashboard)  #Used for creating dashboard layout for the web app.
library(shinydashboardPlus)  #Used for more better customizations in the dashboard.
library(DT)      #Used for projecting table on the web app. 
library(openxlsx) #Used for reading xlsx file
library(pracma)  #Used for practical numerical math functions like time series.
library(rgdal)   #Used for binding several packages to allow publishing of the web app
library(shinythemes) # For adding custom themes
library(plotly) # For plotting interactive graphs
library(readxl) # For reading "xlsx" files


#reading of shapefiles and rainfall data need to be hard coded 

shapefile=shapefile("data/2011_Dist.shp") 
rainfall=read.csv("data/rainfallstates.csv")
rainfall=rainfall %>% rename(DISTRICT=district.y,ST_NM=state)
Area<-read.xlsx("District_Area.xlsx")

#-----------------------------------------------------------------------------------------
#UI component is used to design the App Layout (Front end of the App)  

ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis"),
  skin = "blue",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Maps", tabName = "Maps", icon = icon("map")),
      menuItem("Graphs", tabName = "Graphs", icon = icon("bar-chart-o")),
      menuItem("Hari",tabName="Hari",icon = icon("bar-chart-o")),
      
      menuItem("Controls", tabName = "Controls", icon = icon("th"),
               # Adding dropdown menu for selecting input year
               selectInput('yearselectd',label="Select Year",selected=1901,choices=as.vector(na.omit(unique(rainfall$Year)))),
               # Adding checkbox group for selecting input month
               radioButtons("checkGroupd",label="Select Period",choices=list("Jan"=1,"Feb"=2,"Mar"=3,
                                                                             "Apr"=4,"May"=5,"Jun"=6,"Jul"=7,"Aug"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dec"=12,"Annual"=13,"Kharif"=14),selected=1),
               # Adding dropdown menu for selecting input maptype
               selectInput('mapselectd',label="Map Type",selected="Esri.NatGeoWorldMap",choices=
                             c("Esri.NatGeoWorldMap","OpenMapSurfer.Roads","Esri.WorldPhysical","CartoDB.DarkMatter")),
               # Adding dropdown menu for selecting district
               selectInput('districtSelectd',label = "District",selected  = "Agra",choices = as.vector(na.omit(unique(rainfall$DIST)))),
               # Adding a submit button
               actionButton('god',"submit"),br())
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Maps",
              fluidPage(tags$h2("Rainfall Data Analysis")),
              leafletOutput("mymap"),
              hr(),
              fluidRow(  
                column(3,
                       selectInput('yearselect',label = "Select Year",selected  = 2011,choices = unique(rainfall$Year)),
                       selectInput('mapselect',label = "Map Type",selected  = "OpenMapSurfer.Roads",choices = c("Esri.NatGeoWorldMap","OpenMapSurfer.Roads","Esri.WorldPhysical","CartoDB.DarkMatter"))
                       
                ),
                column(4,offset = 1,
                       actionButton('go',"submit")
                ),
                column(4,
                       radioButtons("checkGroup", label = "Select Month", choices = list("Annual"=1,"June" =6,"July" = 7, "Aug" = 8, "Sept" = 9),selected =NULL)
                )  
                
                
              )                          
              
      ),
      # Second tab content
      tabItem(tabName = "Graphs", 
              mainPanel(
                # creats two tabs within Graphs tab
                tabsetPanel(type = "tabs",
                            tabPanel("Plot",
                                     selectInput('districtSelect',label = "District",selected  = "Agra",choices = unique(rainfall$DIST)),       
                                     sliderInput("slider", label = h3("Year"), min =1, max = 111 ,value=c(1,111)),
                                     box(
                                       title = "Annual Data Visualisation",
                                       width = 12,
                                       icon = "fa fa-calendar-alt",
                                       gradientColor = "blue", 
                                       boxToolSize = "xs", 
                                       closable = FALSE,
                                       footer = plotOutput("plotxy")
                                     ),
                                     hr(),
                                     box(
                                       title = "Moving Average",
                                       width = 12,
                                       icon = "fa fa-thermometer-half",
                                       gradientColor = "red", 
                                       boxToolSize = "xs", 
                                       closable = FALSE,
                                       footer = plotOutput("plotmov")
                                     ),
                                     hr(),
                                     box(
                                       title = "Deviation from Moving Average",
                                       width = 12,
                                       icon = "fa fa-deviantart",
                                       gradientColor = "maroon", 
                                       boxToolSize = "xs", 
                                       closable = FALSE,
                                       footer = plotOutput("plotdev")
                                     ),
                                     hr(),
                                     box(
                                       title = "Rainfall Prediction ",
                                       width = 12,
                                       icon = "fa fa-dice",
                                       gradientColor = "green", 
                                       boxToolSize = "xs", 
                                       closable = FALSE,
                                       footer = sliderInput("slider2", label = h3("Prediction Range"), min =1900, max =2030 ,value=c(2000,2020)),plotOutput("plotpre")
                                     )),
                            #second tab for the table 
                            tabPanel("Summary", 
                                     fluidPage(style = "font-size: 50%; width: 50%",
                                               column(12, wellPanel( DT::dataTableOutput("mytable" ))) 
                                     ) 
                            )
                            
                )
              )
              
      ),
      
      tabItem(tabName = "Hari",leafletOutput("mymapd"),hr(),
              # "box" for creating a box to show plot
              box(title = "Above and Below Normal Rainfall",width = 12,icon = "fa fa-thermometer-half",
                          gradientColor = "blue", boxToolSize = "xs", closable = FALSE, footer = plotOutput("distPlot")),hr(),
              # "box" for creating a box to show plot
              box(title = "Yearwise variations from Average Rainfall",width = 12,icon = "fa fa-calendar-alt",
                          gradientColor = "red", boxToolSize = "xs", closable = FALSE, footer = plotlyOutput("avgPlot"))
              # Adding theme to the application
              
    )
  )
)
)

#---------------------------------------------------------------------------------------
 

server <- function(input, output, session) {
  
  
  #prepares raw data for the map according to user's selection 
  
  dataplot= eventReactive(input$go,
                          {
                            
                            shaperainfall=merge(shapefile,rainfall %>% filter(Year==input$yearselect),
                                                by='DISTRICT',duplicateGeoms = TRUE)}
                          
  )
  
  
  
  
  
  output$mytable = DT::renderDataTable({
    datatable(rainfall, filter="bottom", options = list(
      columnDefs = list(list(targets = c(2,4,5,6,7,8),searchable = FALSE)), escape=FALSE))
    
  })
  

  
  output$plotxy <- renderPlot({
    
    datatoplot<-read.xlsx(paste("data/NewFolder/",input$districtSelect,".xlsx",sep = ""))
    
    Annual<-datatoplot
    
    Annual<-(as.vector(t(as.matrix(Annual[,14]))))
    Annual<-ts(Annual,frequency=1,start=datatoplot$Year[input$slider[1]],
               end=datatoplot$Year[input$slider[2]])
    
    plot(Annual,main="Annual data Visualization ",ylab="Rainfall in mm",xlab="Years")
    #polygon(Annual, col='red', border=NA)
  })
  
  
  
  output$plotmov<-renderPlot({
    
    datatoplot<-read.xlsx(paste("data/NewFolder/",input$districtSelect,".xlsx",sep = ""))
    Annual<-datatoplot
    
    
    Annual<-(as.vector(t(as.matrix(Annual[,14]))))
    Annual<-ts(Annual,frequency=1,start=datatoplot$Year[input$slider[1]],
               end=datatoplot$Year[input$slider[2]])
    
    plot(movavg(Annual,n=30),type="l",ylab="Rainfall in mm",main="Moving Average for 30 years",
         xlab="Period (Number of years past 1901)")
    
  })
  
   
  
  output$plotdev<-renderPlot({
    
    datatoplot<-read.xlsx(paste("data/NewFolder/",input$districtSelect,".xlsx",sep = ""))
    Annual<-datatoplot
    Annual<-(as.vector(t(as.matrix(Annual[,14]))))
    Annual<-ts(Annual,frequency=1,start=datatoplot$Year[input$slider[1]],
               end=datatoplot$Year[input$slider[2]])
    
    
    plot(Annual-movavg(Annual,n=30),type="l",ylab="Deviation in mm",
         main="Deviation of Actual Rainfall from 30 years Moving Average ",xlab="Years")
    # polygon(Annual-movavg(Annual,n=30), col='red', border=NA)
    
  })
  
  
  
  output$plotpre<-renderPlot({
    
    datatoplot<-read.xlsx(paste("data/NewFolder/",input$districtSelect,".xlsx",sep = ""))
    Annual<-datatoplot
    
    
    
    r=data.frame(Annual$Jan,Annual$Feb,Annual$Mar,Annual$Apr,Annual$May,Annual$Jun,Annual$Jul,Annual$Aug,Annual$Sep,Annual$Oct
                 ,Annual$Nov,Annual$Dec)
    r<-ts(as.vector(t(as.matrix(r))),frequency = 12,start=1901)
    
    decompose(r)
    #plot(decompose(r))
    #estimated holt winters
    hw2<-HoltWinters(r)
    #predict the future
    hw2.pred<-predict(hw2,30*12,prediction.interval = TRUE)
    
    
    plot.ts(hw2.pred[,1],col="blue", xlim=c(input$slider2[1],input$slider2[2]),
            ylim=c(-500,500),ylab="Rainfall in mm", xlab="Years",main="Fitting and Prediction")
    lines(hw2$fitted[,1],lty=2,col="red")
    lines(hw2.pred[,2],lty=2,col="seagreen")
    lines(hw2.pred[,3],lty=2,col="seagreen")
    legend('bottomright',c('fitting','prediction','limits'),
           fill = c("red","blue","seagreen"), bty = 'n',
           border = NA)
    
    
    
    
  })
  
  
  
  #------------------------Hari's server parts------------------------------
  
  output$mymapd <- renderLeaflet(if(input$checkGroupd=="1"){
    # Defining color levels
    collevels<-c(0,500,1000,1500,2000,Inf) 
    # Defining colors to be used
    mypal <- colorBin(c("midnightblue","blue4","deepskyblue","blue","cyan","cornsilk"), 
                      # "dataplot" function executes the results to be shown on map
                      domain = dataplot()$Jan, bins = collevels, reverse = TRUE) 
    leaflet() %>% addProviderTiles(input$mapselectd) %>% addPolygons(data = dataplot(),
                                                                     stroke = FALSE, smoothFactor = 0.9, fillOpacity = 0.9, fillColor = ~mypal(dataplot()$Jan),
                                                                     popup = paste("District: ", dataplot()$DISTRICT, "<br>", "Rainfall: ", dataplot()$Jan, "<br>")) %>%
      addLegend(position = "bottomright", pal = mypal, values = dataplot()$Jan,
                title = "Rainfall (in mm) ", opacity = 1)})
    
    # For Feburary
    # "renderLeaflet" for creating a map
    # Depends upon the value given in chechGroup variable
    
  
  
  # Above and Below Normal Rainfall
  output$distPlot <- renderPlot({
    datatoplot<-read.xlsx(paste0("Districts/",input$districtSelectd,".xlsx"))
    row.names(datatoplot)<-as.vector(na.omit(unique(rainfall$Year)))
    Normal<-sum(datatoplot$WC13)/length(datatoplot$WC13)
    ggplot(datatoplot, aes(x=rownames(datatoplot),y=WC13)) + geom_point(aes(col=ifelse(WC13 >= Normal*1.1,'#56B4E9',
                                                                                       ifelse(WC13 <= Normal*0.9,'#999999','#66FF00')))) + theme(axis.text.x = element_text(size = 10,angle = 45, hjust = 1)) + 
      ylab("Annual Rainfall (in mm)") + xlab("Year") + scale_x_discrete(breaks = rownames(datatoplot)[seq(1, 
                                                                                                          length(rownames(datatoplot)), by = 2)]) + labs(color = "Rainfall") + scale_color_manual(values=c("#56B4E9", "#999999", "#66FF00"),
                                                                                                                                                                                                  labels=c("Above Normal", "Normal", "Below Normal")) + geom_hline(yintercept=Normal*1.1, linetype="dashed", color = "skyblue", size=1) + 
      geom_hline(yintercept=Normal*0.9, linetype="dashed", color = "green", size=1)})
  
  # Average Annual Rainfall
  output$avgPlot <- renderPlotly({
    Year<-read_excel(paste0("Yearwise/",input$yearselectd,"_data.xlsx"))
    rownames(Year)<-Area$Name
    Average<-sum(Year$Value)/length(Year$Value)
    District<-rownames(Year)
    ggplotly(ggplot(Year, aes(x=District,y=Value,text = paste("District : ",District,"<br>Rainfall (in mm) : ",Value,"<br>Area (in sq. km.) : ",Area))) + 
               geom_point(aes(col=factor(ifelse(Value>Average,'#56B4E9',
                                                ifelse(Value<Average,'#66FF00','#999999')),labels = c("Above\nAverage\nRainfall", "Below\nAverage\nRainfall")),size=Area)) + 
               labs(x="District",y="Rainfall (in mm)") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
                                                               axis.ticks.y=element_blank()) + geom_hline(yintercept=Average, linetype="dashed", color = "skyblue", size=1) +
               scale_color_manual(values=c("#56B4E9","#66FF00"),labels=c("Above Average Rainfall","Below Average Rainfall")) +
               theme(axis.title=element_text(size=8)) + theme(legend.text=element_text(size=7),legend.title=element_blank()),tooltip = "text")})
  
  
}  


#------------------------------------------------------------------------------------


shinyApp(ui, server)   #This calls/runs the app