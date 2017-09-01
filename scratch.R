##3.states file
state_full <- readOGR(dsn="./cb_2016_us_state_500k/cb_2016_us_state_500k.shp",
                       layer = "cb_2016_us_state_500k",verbose = FALSE)
#from: https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
state_full <- trans(state_full,type="county")
state_full$lng <- unlist(lapply(state_full@polygons, function(dt) dt@labpt[1]))
state_full$lat <- unlist(lapply(state_full@polygons, function(dt) dt@labpt[2]))
