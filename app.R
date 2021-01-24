


library(shiny)
library(leaflet)
library(tidyverse)
library(shinycssloaders)
library(shiny.semantic)
library(semantic.dashboard)
library(shinyWidgets)
source("helper.R")
data <- read_csv('Data/ships.csv')


ui <- dashboardPage(
  dashboardHeader(color = "blue", title = "Vessel Traffic Observer", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = 'blue',dim_page = TRUE,closable = TRUE,inverted = TRUE,
    sidebarMenu(
      menuItem(tabName = "main", "Main")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('* {font-family: "Tahoma"};'))),
    tabItems(
      selected = 1,
      tabItem(
        tabName = "main",
        fluidRow(box(width = 4,selectInput("shiptype",choices =  unique(data$ship_type), selected = "Cargo", label = h3("Select type"), width = 200)),
                 box(width = 4,uiOutput("shipselection")),
                 box(width = 5,h3("Vessel Traffic Viewer"), p("Select a ship to see the largest distance navigated by that vessel"))),
      
        fluidRow(box(withSpinner(p(textOutput("message"),type = 5)),width = 4),
                 box(withSpinner( leafletOutput("mymap"),type = 5),width = 8))
      )
    )
  )
)






server <- function(input, output, session) {
  
  

  list_name <-  reactive(unique(data[data$ship_type == input$shiptype,]$SHIPNAME))
  
  output$shipselection <- renderUI({
    selectInput("shipname", choices = list_name() , selected=  head(list_name(),1), label = h3("Select vessel"), width = 200)
  })
  
  
  pointy <- reactive(data %>% 
                       filter(SHIPNAME == input$shipname)
                       %>% get_row_distance())
  
  output$message <- renderText(paste("The", input$shipname, "Longest Navigation Distance is:",pointy()$dis[1])) 
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=pointy()$lon, lat=pointy()$lat, popup= pointy()$point) %>% 
      addPolylines(data = pointy(), lng = ~lon, lat = ~lat,label = pointy()$dis[1])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)