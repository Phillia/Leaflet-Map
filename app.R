library(shiny)
library(leaflet)
library(geojsonio)
library(maptools)


source("map_data.R")

ui <- navbarPage("Map", id="nav",
                 tabPanel("Interactive map",
   leafletOutput("mymap"),
   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                 draggable = TRUE, top = 500, left = "auto", right = 20, bottom = "auto",
                 width = 300, height = "auto",
                 checkboxInput("check","Hide Rating Area Label"),
                 selectInput("choose","Choose shape file",
                             c("TN","USA")),
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
    } else if(input$choose=="USA") {
      return(
        list(ct=county_full2,
             rt=rating_area2)
      )
    } else stop("Error input")
  })
  
  output$mymap <- renderLeaflet({
    leaflet(data=dtplot()$ct,options = leafletOptions(minZoom = 3.5, maxZoom = 8)) %>% 
      #addTiles() %>%
      #add county borders and have labels diplayed with mouse action
      addPolylines(stroke = TRUE, weight = 1, color = "#444444", fill = FALSE) 
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
  
  observe({
    if(!is.null(x())) {
    leafletProxy("mymap",data=dtplot()$rt) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity =0.5, fillColor = ~pall(x()),
                  group="rating_area") %>% 
      addLabelOnlyMarkers(lng=~lng, lat=~lat, label =  ~as.character(name), 
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, 
                                                      style=list("font-size" = "12px")),
                          group="rating_label") %>%
        showGroup("rating_area") 
    } else {
      leafletProxy("mymap") %>% hideGroup(c("rating_area","rating_label"))
    }
    
  })
  
  observe({
    if(!is.null(x())) {
      leafletProxy("mymap") %>% clearControls() %>% addLegend("bottomleft",pal = pall, values = x(), title="Var by rating area")
    } else {
      leafletProxy("mymap") %>% clearControls() 
      }
  })
  
  observe({
    if(input$check==TRUE) {
      leafletProxy("mymap") %>% hideGroup("rating_label") 
    } else {
      leafletProxy("mymap") %>% showGroup("rating_label") 
    }
  })
  


}
  
shinyApp(ui, server)

#modify graph
#explore layer