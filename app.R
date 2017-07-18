library(shiny)
library(leaflet)
library(geojsonio)
library(maptools)


source("map_data.R")

ui <- navbarPage("Map", id="nav",
                 tabPanel("Interactive map",
   leafletOutput("mymap"),
   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                 draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                 width = 330, height = "auto",
                 selectInput("choose","Choose shape file",
                             c("TN","Country")),
                 selectInput("var", "Var",
                             c("None","rate","var2")
                 )
   )
))

server <- function(input, output, session) {
  dtplot <- reactive({
    if(input$choose=="TN") {
      return(
        list(ct=county_TN,
             rt=rating_TN)
        )
    } else if(input$choose=="Country") {
      return(
        list(ct=county_full2,
             rt=rating_area2)
      )
    } else stop("Error input")
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data=dtplot()$ct,options = leafletOptions(minZoom = 3, maxZoom = 8)) %>% 
      #addTiles() %>%
      #add county borders and have labels diplayed with mouse action
      addPolygons(stroke = TRUE, weight = 1, color = "#444444", fill = FALSE,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>%
      addLabelOnlyMarkers(lng=~lng, lat=~lat, label =  ~as.character(NAME), 
                          labelOptions = labelOptions(noHide = F, textOnly = F, clickable=T,
                                                      style=list(
                                                        "color" = "blue",
                                                        "font-family" = "serif",
                                                        "font-style" = "italic",
                                                        "font-size" = "8px"
                                                      ))) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  x <- reactive({
    if(input$var=="rate") {
      return(dtplot()$rt$rate)
    } else if(input$var=="var2") {
      return(dtplot()$rt$var2)
    } else if(input$var=="None") {
      NULL
    } else stop("Error input")
  })
  
  observe(({
    if(!is.null(x())) {
    leafletProxy("mymap",data=dtplot()$rt) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity =0.5, fillColor = ~pall(x()),
                  group="upper") %>% 
      addLabelOnlyMarkers(lng=~lng, lat=~lat, label =  ~as.character(name), 
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, 
                                                      style=list("font-size" = "12px")),
                          group="upper") %>%
        showGroup("upper")
    } else {
      proxy <- leafletProxy("mymap")
      proxy %>% hideGroup("upper")
    }
    
  }))
  
  observe({
    if(!is.null(x())) {
    proxy <- leafletProxy("mymap")
    proxy %>% clearControls() %>% addLegend("bottomleft",pal = pall, values = x(), title="Var by rating area")
    } else {
      proxy <- leafletProxy("mymap")
      proxy %>% clearControls() 
      }
  })
  
}

shinyApp(ui, server)

#modify graph
#explore layer