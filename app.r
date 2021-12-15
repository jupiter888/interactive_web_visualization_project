library(shiny)
library(dygraphs)
library(httr)
library(xml2)
library(shinythemes)
source("load_data.r")
#loading locations 
locations = list(
    list(name = "Sněžka", latitude = "50.7", longitude = "15.7"),
    list(name = "Pálava", latitude = "48.8", longitude = "16.6"),
    list(name = "Komorní hůrka", latitude = "50.1", longitude = "12.3")
)
#Define user interface
ui <- fluidPage(shinythemes::themeSelector(),  #set the page the way you wish to see the theme and coloring( effective for difference in graph display)
                sidebarPanel(
                    textInput("txt", "Text input:", "enter your favorite color here"),
                    sliderInput("slider", "How much do you love this color:", 1, 100, 30),
                    tags$head(tags$script(src = "message-handler.js")),
                    actionButton("do", "Click Me"),#this is for the button to show popupbox
                ),
                mainPanel(
                ),
    tags$head(tags$script(src = "get_forecast.js")),
    column(3, titlePanel("Interactive Web Visualization Project"),
        selectInput("variable", "Variable:", levels(data$variable)),
        checkboxGroupInput("locations", "Locations:", levels(data$location), selected = "snezka")),
    column(9,
        dygraphOutput("dygraphs_plot"),
        verbatimTextOutput("current_temp"))
)

#Define Server Function
server <- function(input, output) {
    data_to_plot <- reactive({
        data[location %in% input$locations & variable == input$variable]
    })
    output$current_temp <- renderText({
        text = ""
        for (location in locations) {
            url = paste0("https://api.met.no/weatherapi/locationforecast/2.0/classic?lat=", location$latitude, "&lon=", location$longitude)
            response = GET(url)
            stop_for_status(response)
            temperature_nodes = xml_find_all(content(response), ".//temperature")
            temperatures = xml_attr(temperature_nodes, "value")
            text = paste0(text, location$name, ": ", temperatures[1], " °C\n")
        }
        return(text)
    })

    output$dygraphs_plot <- renderDygraph({
        dygraph(dcast(data_to_plot(), "time ~ location + variable")) %>% dyRangeSelector()
    })
    
   
}

#Create shiny app object
shinyApp(ui = ui, server = server)
