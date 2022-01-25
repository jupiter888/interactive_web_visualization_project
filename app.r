library(shiny)
library(dygraphs)
library(httr)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(data.table)
library(DT)
library(ggiraph)
library(xml2)
source("load_data.r")
locations = list(
  list(name = "Snezka", latitude = "50.7", longitude = "15.7"),
  list(name = "Palava", latitude = "48.8", longitude = "16.6"),
  list(name = "Komorni hurka", latitude = "50.1", longitude = "12.3")
)
latitudes = c(50.7, 48.8, 50.1)
longitudes = c(15.7, 16.6, 12.3)
location_labels = c("Snezka", "Palava", "Komorni Hurka")
location_ids = c("snezka", "palava", "komorni_hurka")
dt_all<-data.table(longitudes,latitudes,location_ids)

ui <- fluidPage(shinythemes::themeSelector(), 
                titlePanel("Interactive Web Visualization Project")
                #removed redundant code from this line(meteorological javascript)
                column(3, titlePanel("Project by: Daniel Tracy"),
                       selectInput("variable", "Variable:", levels(data$variable)),
                       checkboxGroupInput("locations", "Locations:", levels(data$location), selected = "snezka"),
                       girafeOutput("plot_girafe"),
                       ),
                column(9,
                       dygraphOutput("dygraphs_plot"),
                       verbatimTextOutput("current_temp"),
                       leafletOutput("map"),
                       dataTableOutput("data_table1")
                )
)

server <- function(input, output) {
  data_to_plot <- reactive({
    data[location %in% input$locations & variable == input$variable ]
    # Make sure requirements are met <- this fixed the top error when no location is selected , however with data selected errors display
    req( data[location %in% input$locations & variable == input$variable ] )
  data_selection<-data[location %in% input$locations & variable == input$variable ]    
    if( nrow(data_selection)==0 ) 
      req(input$locations)
    else
      req(data_selection)
    #req(input$locations)
  })
  output$current_temp <-renderText({
    text=""
    for (location in locations) {
      url=paste0("https://api.met.no/weatherapi/locationforecast/2.0/classic?lat=", location$latitude, "&lon=", location$longitude)
      response=GET(url)
      stop_for_status(response)
      temperature_nodes=xml_find_all(content(response), ".//temperature")
      temperatures=xml_attr(temperature_nodes, "value")
      text=paste0(text, location$name, ": ", temperatures[1], " °C\n")
    }
    return(text)
  })
  output$dygraphs_plot <- renderDygraph({
    dygraph(dcast(data_to_plot(), "time ~ location + variable")) %>% dyRangeSelector()
  })
  output$map <- renderLeaflet({
    selected_checkbox_data<-dt_all$location_ids %in% input$locations
    long_data<-dt_all$longitudes[selected_checkbox_data]
    lat_data<-dt_all$latitudes[selected_checkbox_data]
    label_data<-dt_all$location_ids[selected_checkbox_data]
    added<- sum(selected_checkbox_data)
    #conditional for the markers to be added only if location input data is selected
    if(added==0){ 
        leaflet() %>% setView(lng=15, lat=50, zoom=7) %>% addTiles()
      }
    else{
      leaflet() %>% setView(lng=15, lat=50, zoom=7) %>% addTiles() %>%
      addMarkers(lng=long_data, lat=lat_data, label=label_data,      
                 options=markerOptions(opacity=0.6)
                 )
    }
  })
  output$plot_girafe <- renderGirafe({
    p=ggplot(data_to_plot(), aes(x=time, y=value, colour=location, tooltip=paste(location, value)))
    p=p+geom_point_interactive()
    girafe(ggobj=p)
  })
  output$data_table1=renderDT({
    data[location %in% input$locations & variable == input$variable]
  })
    
}

#Create shiny app object
shinyApp(ui = ui, server = server)


