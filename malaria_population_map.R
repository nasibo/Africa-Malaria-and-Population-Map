library(raster)
library(sp)
library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(dplyr)

setwd("/your/working/directory")

## Load in the population density raster, malaria raster,
## world map shapefile, and a list of countries in Africa
world_tif <- raster("gpw-v4-population-density_2015.tif")
malaria <- raster("MODEL43.2015.inc.rate.PR.rmean.stable.tif")
countries <- readOGR(dsn = ".","gpw-v4-national-identifier-grid_polygons")
african_countries <- read.csv("countries.csv", stringsAsFactors = FALSE)

## Crop the population density map to just Africa and then 
## aggregate the cells by a factor of 5 to match the resoluation 
## of the malaria raster
af_pop <- crop(world_tif, extent(-18.00006, 52.04157, -35.00001, 37.54163))
af_pop <- aggregate(af_pop,5)


## Use the list of countries in Africa to limit the shapefile to 
## only those countries. 
african_countries <- as.data.frame(african_countries) 
colnames(african_countries) <- "Country"
african_countries[african_countries$Country == "Libyan Arab Jamahiriya","Country"] <- "Libya" 

all_countries <- countries$NAME0
all_countries <- as.data.frame(all_countries)
colnames(all_countries) <- "Country"

keep_countries <- semi_join(african_countries,all_countries,by="Country")

## 2 African countries are missing, figure out which ones they are 
## and put them into the list of countries to keep in the raster
missing <- anti_join(african_countries, keep_countries,by="Country")
miss1 <- missing[1,]
miss2 <- missing[2,]

## now change the names of miss1 and miss2 to what they are called in
## the shapefile. 
miss1 <- "Côte d'Ivoire"
miss2 <- "Réunion"

keep_countries <- keep_countries[,1]
## Add in South Sudan because the csv list of countries used was 
## dated 
keep_countries <- c(keep_countries,miss1,miss2,"South Sudan")

countries_africa <- countries[countries$NAME0 %in% keep_countries,]


## Use the shapefile to make sure the raster only shows African 
## countries and not other countries within the croped area 
af_pop <- mask(af_pop,countries_africa)

## combine the Africa population raster and the malaria raster
## then transform them into a dataframe
af_pop_mal <- brick(af_pop,malaria)
af_pop_mal_df <- rasterToPoints(af_pop_mal)
colnames(af_pop_mal_df) <- c("long","lat","population","malaria")
af_pop_mal_df <- as.data.frame(af_pop_mal_df)
af_pop_mal_df$malaria <- af_pop_mal_df$malaria * 1000 

## Create a for loop to agregate population density data. 
## 21 lines of latitude are taken, then the population density 
## data for the first 10 lines and last 10 lines are summed and
## added to the 11th line. Now each point of longitude for any 
## remaining latitude line will have the summed population value 
## of the 10 points above and 10 points below.
lats <- unique(af_pop_mal_df$lat)
steps <- seq(1,length(lats),21)

ag_lines <- NULL
for(i in 1:length(steps)){
  start <- steps[i]
  mid <- start + 10
  end <- steps[i+1] - 1
  lat_start <- lats[start]
  lat_mid <- lats[mid]
  lat_end <- lats[end]
  data <- af_pop_mal_df[af_pop_mal_df$lat <= lat_start & af_pop_mal_df$lat >= lat_end,]
  mid_data <- af_pop_mal_df[af_pop_mal_df$lat==lat_mid,]
  mid_data$group  <- i
  longs <- unique(mid_data$long)
  for(j in 1:length(longs)){
    long <- longs[j]
    data2 <- data[data$long==long,]
    sum_pop <- sum(data2$pop)
    mean_mal <- mean(data2$malaria, na.rm=TRUE)
    mid_data[mid_data$long==long,"population"] <- sum_pop
    mid_data[mid_data$long==long,"malaria"] <- mean_mal
  }
  ag_lines <- rbind(ag_lines,mid_data)
}

## The next two for loops check to see where there are big jumps in 
## longitude that cause straight lines where there should not be.
## The first finds large differences in longitude, the second 
## puts lines of latitude with large jumps in longitude into 
## seperate groups. For example, a latitude line that goes from 
## the continent of Africa to Madagascar would be drawn in the 
## ocean. To prevent this from happening, this one line needs to 
## become two lines - one for the continent of Africa and one 
## for Madagascar - and we do this by breaking the one line into 
## two groups. 

jump_data <- ag_lines
groups <- unique(jump_data$group)
jump_dif <- NULL
for(i in 1:length(groups)){
  group <- groups[i]
  data <- jump_data[jump_data$group == group,]
  long_dif_col <- NULL
  for(j in 1:nrow(data)){
    long1 <- data[j,"long"]
    long2 <- data[j+1,"long"]
    long_dif <- long1 - long2
    long_dif_col <- c(long_dif_col,long_dif)
  }
  data$long_dif_col <- long_dif_col
  jump_dif <- rbind(jump_dif,data)
}

## This for loop checks to see where there are big jumps in 
## longitude that cause straight lines where there should not be

jump_dif$row_num <- 1:nrow(jump_dif)
new_ag_lines <- NULL
for(i in 1:length(groups)){
  group <- groups[i]
  data <- jump_dif[jump_dif$group == group,]
  max_dif <- min(data$long_dif_col, na.rm=TRUE)
  if(max_dif > -1){
    new_ag_lines <- rbind(new_ag_lines,data)
  } else {
  break_row <- data[data$long_dif_col==max_dif,"row_num"]
  break_row <- break_row[1]
  same <- data[data$row_num <= break_row,]
  different <- data[data$row_num > break_row,]
  different$group <- different$group + .2
  new_data <- rbind(same,different)
  new_ag_lines <- rbind(new_ag_lines,new_data)
  }
}

## create latitude lines as a function of the population density data
new_ag_lines$lat_curved <- new_ag_lines$lat + new_ag_lines$population/7000

## Make a pretty ggplot plot

low <- min(new_ag_lines$malaria, na.rm = TRUE)
high <- max(new_ag_lines$malaria, na.rm = TRUE)
mid <- 100

breaks <- c(low,mid,high)
breaks <- round(breaks, 0)
values <- breaks

plot <- ggplot(data=new_ag_lines) + 
  geom_line(aes(x=long,y=lat_curved, group=group, color = malaria), size=1.1)+
  scale_color_gradientn(breaks = breaks, values = values, 
    colors = c("green","yellow","red"),
    rescaler = function(x, ...) x, oob = identity,
    name ="Malaria (Pf) incidence \nper 1000 people") +
  annotate("text", x = 5, y = 55, 
    label = "Malaria and Population Density in Africa", 
    color = "#999999", size = 10) + 
  theme(panel.background = element_rect(fill = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(.2, .15),
    legend.background = element_rect(fill ="black"),
    legend.text = element_text(color="#999999", size = 15),
    legend.title = element_text(color="#999999", size = 20),
    legend.key.size = unit(1, "cm")) 

plot
ggsave(plot, filename = "Africa_Plot.png",
  width=30, height=40, units = "cm")

