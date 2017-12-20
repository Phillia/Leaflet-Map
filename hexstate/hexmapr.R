setwd("~/Desktop/leaflet-map")
#https://github.com/sassalley/hexmapr
library(devtools)
library(ggplot2)
library(viridis)
library(plyr)
library(dplyr)
# install_github("sassalley/hexmapr")
library(hexmapr)
library(gridExtra)


#read in rating area shapefile
library(rgdal)
rating_area <- readOGR(dsn="./output/ratingAreas.shp", 
                       layer = "ratingAreas",verbose = FALSE)

library(maptools)
trans <- function(dt,type) {
        
        # convert it to Albers equal area
        us_aea <- spTransform(dt, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
        us_aea@data$id <- rownames(us_aea@data)
        
        ###rules to filter states
        if(type=="rating") {
                alaska <- us_aea[grepl("AK_*",us_aea$name),]
                hawaii <- us_aea[grepl("HI_*",us_aea$name),]
                alaska <- elide(alaska, rotate=-50)
                alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
                alaska <- elide(alaska, shift=c(-1870000, -2500000))
        } else if (type=="county") {
                alaska <- us_aea[us_aea$STATEFP=="02",]
                hawaii <- us_aea[us_aea$STATEFP=="15",]
                alaska <- elide(alaska, rotate=-50)
                alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
                alaska <- elide(alaska, shift=c(-2100000, -2500000))
        } else stop("type error")
        
        # extract, then rotate, shrink & move alaska (and reset projection)
        proj4string(alaska) <- proj4string(us_aea)
        
        # extract, then rotate & shift hawaii
        hawaii <- elide(hawaii, rotate=-35)
        hawaii <- elide(hawaii, shift=c(5400000, -1400000))
        proj4string(hawaii) <- proj4string(us_aea)
        
        if(type=="rating") {
                us_aea <- us_aea[!grepl("AK_*",us_aea$name) & !grepl("HI_*",us_aea$name),]
        } else if (type=="county") {
                us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
        } else stop("type error")
        
        us_aea <- rbind(us_aea, alaska, hawaii)
        # remove old states and put new ones back in; note the different order
        # we're also removing puerto rico in this example but you can move it
        # between texas and florida via similar methods to the ones we just used
        
        us_aea2 <- spTransform(us_aea, proj4string(dt))
        
        return(us_aea2)
}

rating_area <- trans(rating_area,type="rating")

# the package does not work for graph with fewer than 5 polygons
states <- unique(substr(rating_area$name,1,2))
crit <- data.frame(table(substr(rating_area$name,1,2))) %>% filter(Freq>=5)
# 1. states that can be plotted by themselves
states2 <- as.character(crit$Var1)
# 2. states that need to be pooled together 
st2fix <- states[!(states %in% states2)]

# 1. function to plot and save
honeycomb <- function(shape,st,seed=1) {
        original_shapes <- shape[substr(shape$name,1,2)==st,]
        original_details <- get_shape_details(original_shapes)
        new_cells_hex <-  calculate_cell_size(original_shapes, original_details,0.03, 'hexagonal', seed)
        resulthex <- assign_polygons(original_shapes,new_cells_hex)
        out <- paste0("hex_",st,seed)
        assign(out,resulthex)
        
        outname <- paste0("./hexstate/",out,".jpeg")
        jpeg(file=outname)
        plot(new_cells_hex[[2]], main = paste0(st,"_","Seed ",seed))
        dev.off()
        
        save(list=out,file=paste0("./hexstate/",out,".rda"))
        
}

for(i in seq(length(states2))) {
        cat("running",states2[i],"/",i,"of 51","\n")
        honeycomb(shape=rating_area,st=states2[i],seed=1)
}

# 2. function to pool and separate after plotting

# for(i in st2fix) {
#         print(paste(i,length(rating_area$name[grepl(i,rating_area$name)])))
# }
# length(rating_area$name[grepl("AZ",rating_area$name)])

duet <- function(st1,st2,save1,save2,seed=1) {
        test <- rating_area[grepl(st1,rating_area$name) | grepl(st2,rating_area$name),]
        original_shapes <- test
        original_details <- get_shape_details(original_shapes)
        new_cells_hex <-  calculate_cell_size(original_shapes, original_details,0.03, 'hexagonal', seed)
        resulthex <- assign_polygons(original_shapes,new_cells_hex)
        
        new <- new_cells_hex[[2]]
        new$order <- new@plotOrder 
        t <- data.frame(order=test@plotOrder,name=as.character(test$name))
        new@data <- new@data %>% left_join(t,by="order")
        
        if(save1==TRUE) {
                s <- new[grepl(st1,new$name),]
                out <- paste0("hex_",st1,seed)
                assign(out,s)
                outname <- paste0("./hexstate/",out,".jpeg")
                jpeg(file=outname)
                plot(s, main = paste0(st1,"_","Seed ",seed))
                dev.off()
                save(list=out,file=paste0("./hexstate/",out,".rda"))
        }

        if(save2==TRUE) {
                s <- new[grepl(st2,new$name),]
                out <- paste0("hex_",st2,seed)
                assign(out,s)
                outname <- paste0("./hexstate/",out,".jpeg")
                jpeg(file=outname)
                plot(s, main = paste0(st2,"_","Seed ",seed))
                dev.off()
                save(list=out,file=paste0("./hexstate/",out,".rda"))
        }
        
        
}

# one way to pool
duet("NV","DC",T,T)
duet("WY","VT",T,T)
duet("MT","AK",T,F)
duet("ND","RI",T,T)
duet("SD","HI",T,T)
duet("NE","DE",T,T)
duet("AK","AZ",T,F)
duet("ME","NH",T,T)
duet("MD","DE",T,F)



# library(leaflet)
# library(RColorBrewer)
# pall <- colorNumeric("viridis", NULL)
# leaflet(data=resulthex) %>%
#         addPolylines(stroke = TRUE, weight = 1, color = "#444444", fill = FALSE) %>%
#         addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity =0.5, fillColor = ~pall(var),
#                     label=~as.character(paste0(NAME,"-",var)))
# 
# spplot(resulthex,"var")
