#library(geojsonio)
library(maptools)
library(rgdal)
#library(rgeos)
#library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
#library(maps)
#library(ggrepel)


library(devtools)
library(viridis)
library(hexmapr)
library(gridExtra)



setwd("~/Box Sync/Active/Leaflet")
rm(list=ls())

clean <- function(shape){
        shape@data$id = rownames(shape@data)
        shape.points = fortify(shape, region="id")
        shape.df = join(shape.points, shape@data, by="id")
}


input_file <- system.file('extdata', 'states.json', package = 'hexmapr')
original_shapes <- read_polygons(input_file)
original_details <- get_shape_details(original_shapes)
raw <- read_polygons(input_file)
raw@data$xcentroid <- coordinates(raw)[,1]
raw@data$ycentroid <- coordinates(raw)[,2]

result_df_raw <- clean(raw)
rawplot <- ggplot(result_df_raw) +
        geom_polygon( aes(x=long, y=lat, fill = CENSUSAREA, group = group)) +
        geom_text(aes(xcentroid, ycentroid, label = substr(NAME,1,4)), size=2,color = "white") +
        coord_equal() +
        scale_fill_viridis() +
        guides(fill=FALSE) +
        theme_void()


new_cells_hex <-  calculate_cell_size(original_shapes, original_details,0.03, 'hexagonal',2)
resulthex <- assign_polygons(original_shapes,new_cells_hex)
new_cells_reg <-  calculate_cell_size(original_shapes, original_details,0.03, 'regular', 1)
resultreg <- assign_polygons(original_shapes,new_cells_reg)
result_df_hex <- clean(resulthex)
result_df_reg <- clean(resultreg)


#https://www.r-bloggers.com/moving-the-earth-well-alaska-hawaii-with-r/

#function extracted from other project 
trans <- function(dt,type) {
        # convert it to Albers equal area
        us_aea <- spTransform(dt, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
        
        if(type=="rating_area") {
                # extract, then rotate, shrink & move alaska (and reset projection)
                alaska <- us_aea[grepl("AK_*",us_aea$name),]
                alaska <- elide(alaska, rotate=-50)
                alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
                alaska <- elide(alaska, shift=c(-1870000, -2500000))
                proj4string(alaska) <- proj4string(us_aea)
                
                # extract, then rotate & shift hawaii
                hawaii <- us_aea[grepl("HI_*",us_aea$name),]
                hawaii <- elide(hawaii, rotate=-35)
                hawaii <- elide(hawaii, shift=c(5400000, -1400000))
                proj4string(hawaii) <- proj4string(us_aea)
                
                us_aea <- us_aea[!grepl("AK_*",us_aea$name) & !grepl("HI_*",us_aea$name),]
                
        } else if(type=="state") {
                # extract, then rotate, shrink & move alaska (and reset projection)
                alaska <- us_aea[us_aea$name=="02",]
                alaska <- elide(alaska, rotate=-50)
                alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
                alaska <- elide(alaska, shift=c(-1870000, -2500000))
                proj4string(alaska) <- proj4string(us_aea)
                
                # extract, then rotate & shift hawaii
                hawaii <- us_aea[us_aea$name=="15",]
                hawaii <- elide(hawaii, rotate=-35)
                hawaii <- elide(hawaii, shift=c(5400000, -1400000))
                proj4string(hawaii) <- proj4string(us_aea)
                
                us_aea <- us_aea[!(us_aea$name %in% c("02","15")),]                
                
        }
        
        us_aea <- rbind(us_aea, alaska, hawaii)
        # remove old states and put new ones back in; note the different order
        # we're also removing puerto rico in this example but you can move it
        # between texas and florida via similar methods to the ones we just used
        
        us_aea2 <- spTransform(us_aea, proj4string(dt))
        
        return(us_aea2)
}

#read in shapefile
rating_area <- readOGR(dsn="./output/ratingAreas.shp", 
                       layer = "ratingAreas",verbose = FALSE)
#reposition
rating_area2 <- trans(rating_area,"rating_area")

#save plot order
track.order <- data.frame(id=row.names(rating_area2@data),plotorder=rating_area2@plotOrder,
                          name=rating_area2$name)

#merge in variable to plot
dt <- read_excel("providerplanmapdata.xls") %>% 
        mutate(rating_area=paste0(st,"_",substring(market,3))) %>%
        select(rating_area,var='Number of provider plans (top coded at 3)')
call <- data.frame(
        rating_area=as.character(rating_area2$name),
        stringsAsFactors = FALSE
) %>% 
        left_join(dt,by="rating_area") %>%
        left_join(track.order,by=c("rating_area"="name"))

rating_area2@data <- rating_area2@data %>% left_join(call,by=c("name"="rating_area"))

original_shapes <- rating_area2
original_details <- get_shape_details(original_shapes)



###exam func
shape <- original_shapes
shape_details <- original_details
learning_rate <- 0.03

max_allowed_area <- shape_details$total_area/shape_details$nhex
hexagon_diam <- sqrt(max_allowed_area/2.598076) * 2
cellsize <- shape_details$start_size

repeat {
        HexPts <- spsample(shape, type = grid_type, cellsize = cellsize, iter = 10000)
        npolygons <- length(HexPts)
        print(npolygons)
        print(cellsize)
        
        if (npolygons == shape_details$nhex)
                break else if (npolygons > shape_details$nhex)
                {
                        print("too many polygons")
                        cellsize_new <- cellsize * (1 + learning_rate)
                        cellsize <- cellsize_new
                } else
                {
                        # else (npolygons < shape_details$nhex)
                        print("too few polygons")
                        cellsize_new <- cellsize * (1 - learning_rate)
                        cellsize <- cellsize_new
                }
}

print(paste0("The cellsize is ", cellsize))

if (grid_type == "hexagonal")
{
        Pols <- HexPoints2SpatialPolygons(HexPts)
} else
{
        Pols <- SpatialPixels(HexPts)
        Pols <- as(Pols, "SpatialPolygons")
}
# or spatial polygons? need to turn this into same object as hexagons
# above try making dataframe and going that route. need correct ids for
# match between then and now note <- cellsize could be unsolveable. Add
# rotation of grid if needed.

return(list(HexPts, Pols))



###




new_cells_hex <-  calculate_cell_size(original_shapes, original_details,0.03, 'hexagonal',1)
resulthex <- assign_polygons(original_shapes,new_cells_hex)


#process shapefile 
reorder <- function(data,group.var,unit.var) {
        #current order
        order1 <- data %>% arrange_(group.var,"plotorder") %>% 
                select(group.var,unit.var,plotorder) %>% unique() 
        order1$index <- seq(nrow(order1))
        
        #extract polygons with holes and reorder
        holes <- data %>% filter(hole==TRUE) %>% 
                select(group.var,unit.var) %>% unique() %>%
                mutate(index=0)
        
        if(length(holes)!=0) {
                order.new <- order1 %>% left_join(holes,by=c(group.var,unit.var)) %>%
                        mutate(index=ifelse(index.y==0,0,index.x)) %>% 
                        arrange_(group.var,"index")
                
                order1[c(group.var,unit.var)] <- order.new[c(group.var,unit.var)] 
                data <- data %>% 
                        left_join(order1 %>% select(group.var,unit.var,neworder=plotorder),
                                  by=c(group.var,unit.var)) %>%
                        arrange(neworder)  
        }
        return(data)
}

map <- fortify(rating_area2) %>% left_join(rating_area2@data,by="id") %>%
        mutate(state=substr(name,1,2))
map <- reorder(map,group.var = "state",unit.var = "name")