library(leaflet)
library(geojsonio)
library(maptools)
library(rgdal)
library(rgeos)
library(reshape2)
library(dplyr)
rm(list=ls())

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


##1.rating area map from NYT
rating_area <- readOGR(dsn="./output/ratingAreas.shp", 
                       layer = "ratingAreas",verbose = FALSE)
#made up data 
#rating_area$rate <- sample(1:10000,499)
#rating_area$var2 <- sample(1:5,499,replace = TRUE)
#static label needs location point
#rating_area$lng <- unlist(lapply(rating_area@polygons, function(dt) dt@labpt[1]))
#rating_area$lat <- unlist(lapply(rating_area@polygons, function(dt) dt@labpt[2]))

rating_area2 <- trans(rating_area,type="rating")
#made up data 
rating_area2$rate <- sample(1:10000,499)
rating_area2$var2 <- sample(1:5,499,replace = TRUE)
#static label needs location point
rating_area2$lng <- unlist(lapply(rating_area2@polygons, function(dt) dt@labpt[1]))
rating_area2$lat <- unlist(lapply(rating_area2@polygons, function(dt) dt@labpt[2]))


##2.all county file from http://eric.clst.org/Stuff/USGeoJSON 
#county_full <- geojsonio::geojson_read("gz_2010_us_050_00_500k.json", what = "sp")
county_full <- readOGR(dsn="./cb_2016_us_county_20m/cb_2016_us_county_20m.shp", 
                       layer = "cb_2016_us_county_20m",verbose = FALSE)
#county_full <- county_full[!county_full$STATEFP !="72",]
#county_full$lng <- unlist(lapply(county_full@polygons, function(dt) dt@labpt[1]))
#county_full$lat <- unlist(lapply(county_full@polygons, function(dt) dt@labpt[2]))

county_full2 <- trans(county_full,type="county")
county_full2$lng <- unlist(lapply(county_full2@polygons, function(dt) dt@labpt[1]))
county_full2$lat <- unlist(lapply(county_full2@polygons, function(dt) dt@labpt[2]))

###

pall <- colorNumeric("viridis", NULL)

library(dplyr)
states <- read.delim("state.txt",header=TRUE,sep="|",colClasses="character") %>% filter(as.numeric(STATE) < 60)

##add hhi test
#issues: map does not have PR, GU
#Idaho only has 6 rating areas now, go back to old data (7 RA)

#load("hhi1.rda")
#test <- hhi1 %>% filter(!(state %in% c("PR","GU")))
#call <- as.character(rating_area2$name)
#rating_area2$hhi <- test$hhi[order(match(test$ratingarea,call))]

call <- as.character(rating_area2$name)

load("hhi.drg.full2.rda")
drg <- drg %>% filter(!(state %in% c("PR","GU"))) %>% mutate(market=gsub("_0","",market))
#fill in 0 cell
library(data.table)
cons <- data.table(drg)
cons <- cons[ ,list(rating_area=call),by=c("market","year")] %>% data.frame()
drg <- drg %>% full_join(cons,by=c("market","year","rating_area")) %>% mutate(newvar=paste0(market,"_",year))

add_metrics <- function(target,source,newname) {
  source <- source %>% filter(newvar==newname)
  target$new <-  source$hhi[order(match(source$rating_area,call))]
  names(target)[names(target)=="new"] <- newname
  return(target)
}

for(i in unique(drg$newvar)) {
  rating_area2 <- add_metrics(rating_area2,drg,i)
}

