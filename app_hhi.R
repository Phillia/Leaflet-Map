###TO DO:
#auto select dataset
#reactive ui for year
#data panel tab (cbind a master dataset)


library(shiny)
library(leaflet)
library(geojsonio)
library(maptools)


source("data_hhi.R")

ui <- navbarPage("Map", id="nav",
                 
                 tabPanel("Interactive map",
                          
                          div(class="outer",
                              
                              tags$head(
                                      # Include our custom CSS
                                      includeCSS("styles.css"),
                                      includeScript("gomap.js")
                              ),    
                              
                              #map
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              #parameter panel
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                                            width = 300, height = "auto",
                                            checkboxInput("check","Hide Rating Area Label"),
                                            selectInput("choose","Choose shape file",
                                                        c("USA",as.vector(states$STUSAB)),
                                                        selected="TN"),
                                            selectInput("var", "Select Variable to Plot",
                                                        c("None",varlist))
                              ),
                              
                              conditionalPanel(
                                      condition = "input.var == 'hhi.hix'",
                                      selectInput("year","Select Year",c(1,2))
                              )
                          )
                 ),
                 
                 tabPanel("Data Explorer",
                          dataTableOutput("mytable")
                 )
)

server <- function(input, output, session) {
        
        #data to plot
        dtplot <- reactive({
                if(input$choose=="USA") {
                        return(
                                list(ct=county_full2,
                                     rt=rating_area2)
                        )
                } else if(input$choose %in% states$STUSAB) {
                        fips <- states$STATE[states$STUSAB==input$choose]
                        dt1 <- county_full2[county_full2$STATEFP==fips,]
                        dt2 <- rating_area2[grepl(paste0(input$choose,"_*"),rating_area2$name),]
                        return(list(ct=dt1,
                                    rt=dt2))
                } else stop("sth wrong")
        })
        
        #base map with county borders
        output$mymap <- renderLeaflet({
                leaflet(data=dtplot()$ct,options = leafletOptions(minZoom = 3.5, maxZoom = 8)) %>% 
                        #addTiles() %>%
                        #add county borders and have labels diplayed with mouse action
                        addPolylines(stroke = TRUE, weight = 1, color = "#444444", fill = FALSE) 
        })
        
        #variable to plot
        x <- reactive({
                if(input$var=="None") {
                        NULL
                } else {
                        v <- dtplot()$rt
                        names(v)[names(v)==input$var] <- "var"
                        v$var
                } 
        })
        
        #upper layer of rating areas 
        observe({
                if(!is.null(x())) {
                        leafletProxy("mymap",data=dtplot()$rt) %>%
                                clearGroup("rating_area") %>%
                                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity =0.5, fillColor = ~pall(x()),
                                            label=~as.character(name),labelOptions=labelOptions(clickable=TRUE, direction = 'top', textOnly = TRUE),
                                            group="rating_area") %>% 
                                addLabelOnlyMarkers(lng=~lng, lat=~lat, label =  ~as.character(name), 
                                                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, 
                                                                                style=list("font-size" = "10px")),
                                                    group="rating_label") %>%
                                showGroup("rating_area") 
                } else {
                        leafletProxy("mymap") %>% hideGroup(c("rating_area","rating_label"))
                }
                
        })
        
        #reactive legend
        observe({
                t <- input$choose  
                if(!is.null(x())) {
                        leafletProxy("mymap") %>% clearControls() %>% addLegend("bottomleft",pal = pall, values = x(), title="Var by rating area")
                } else {
                        leafletProxy("mymap") %>% clearControls() 
                }
        })
        
        #reactive rating area label
        observe({
                if(input$check==TRUE) {
                        leafletProxy("mymap") %>% hideGroup("rating_label") 
                } else {
                        leafletProxy("mymap") %>% showGroup("rating_label") 
                }
        })
        
        output$mytable <- renderDataTable({
                data.frame(dtplot()$rt) %>% select(-id,-lng,-lat) %>% arrange(name)
        })
        
}

shinyApp(ui, server)
