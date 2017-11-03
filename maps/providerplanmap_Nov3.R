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


setwd("~/Desktop/leaflet-map/")
rm(list=ls())

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

#merge in variable to plotå
dt <- read_excel("./maps/providerplanmapdata2.xls") %>% 
        mutate(rating_area=paste0(st,"_",substring(market,3))) %>%
        select(rating_area,var='Number of provider plans (top coded at 3)')
dt$var <- factor(dt$var,levels = c(0,1,2,3),labels =c("0","1","2","3+"),ordered = TRUE)
call <- data.frame(
        rating_area=as.character(rating_area2$name),
        stringsAsFactors = FALSE
        ) %>% 
        left_join(dt,by="rating_area") %>%
        left_join(track.order,by=c("rating_area"="name"))

rating_area2@data <- rating_area2@data %>% left_join(call,by=c("name"="rating_area"))

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


# #put polygon with holes as first in plot order
# map <- fortify(rating_area2) %>% left_join(rating_area2@data,by="id") %>%
#         mutate(state=substr(name,1,2)) %>% group_by(state) %>% mutate(min=min(plotorder))
# ll <- map %>% filter(hole==TRUE) %>% select(name,state,exchange=plotorder) %>% unique()
# map <- map %>%
#         left_join(ll %>% select(state,exchange),by="state") %>%
#         mutate(plotorder=ifelse(plotorder==min,exchange,plotorder),
#                plotorder=ifelse(name %in% ll$name,min,plotorder)) %>%
#         arrange(plotorder)
               
        

#rating area centroids
# ra.label <- aggregate(cbind(long,lat)~name,data=map,FUN=mean)
# state.label <- aggregate(cbind(long,lat)~substr(name,1,2),data=map,FUN=mean)
# names(state.label)[1] <- "name"
ggplot() + 
        geom_map(data=map,map=map, aes(map_id=id,fill = var,group=group)) +
        scale_fill_brewer(palette = "YlGnBu") +
        # scale_fill_manual(values = c("lightblue1", "steelblue1","dodgerblue2","navyblue")) +
        # scale_fill_gradient(low = "lightblue1", high = "navyblue", guide="legend") +
        guides(fill = guide_legend(title = ""),color = FALSE) +
        geom_path(data=map,aes(group=group,x=long,y=lat),color="black",size=0.1) +
        #geom_text_repel(data=state.label,aes(label = name, x = long, y = lat), segment.size = 0.008) +
        #geom_text(data=ra.label,aes(label=name,x=long,y=lat),size=2) +
        theme_bw() +
        ggtitle("Number of provider-owned plans by rating area")+
        theme(plot.title = element_text(hjust = 0.5),
              axis.line = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks = element_blank(),
              legend.position = "top",
              legend.direction = "horizontal"
        ) + 
        coord_fixed(1.5)




# library(leaflet)
# library(RColorBrewer)
# pall <- colorNumeric("viridis", NULL)
# leaflet(data=rating_area2) %>%
#         addPolylines(stroke = TRUE, weight = 1, color = "#444444", fill = FALSE) %>%
#         addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity =0.5, fillColor = ~pall(var),
#                     label=~as.character(name))
# 
# spplot(rating_area2,"var")