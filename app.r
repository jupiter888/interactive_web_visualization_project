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
ui <- fluidPage(shinythemes::themeSelector(),  #sets the page the way you wish to see the theme and coloring( effective for difference in graph display. This box is able to be dragged anywhere on the app page for being able to see content)
                titlePanel("Interactive Web Visualization Project"),
                tags$head(tags$script(src = "get_forecast.js")),
                column(3, titlePanel("Project by: Daniel Tracy"),
                       selectInput("variable", "Variable:", levels(data$variable)),
                       #checkboxGroupInput removed to eliminate error. 25.01
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
#perhaps the need for req() is here, for handling the null selection, for the reactive expression not to fail. 
#second solution being the primitive method of if (...) return(NULL), thus not rendering the errors to the screen  https://shiny.rstudio.com/articles/req.html

server <- function(input, output) {
  data_to_plot <- reactive({
    data[location %in% input$locations & variable == input$variable ]
    # Make sure requirements are met <- this fixed the top error when no locatoin is selected 
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
    leaflet() %>% setView(lng=15, lat=50, zoom=7) %>% addTiles() %>%
      addMarkers(lng=longitudes, lat=latitudes, label=location_labels,      
                 options=markerOptions(draggable=TRUE, opacity=0.6),
                 #take sel locations and selected lng's and lat's)
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


