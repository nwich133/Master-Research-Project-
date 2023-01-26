###################################################################################
### Script description: This script creates the figures 2, 3 and 5 from my term paper
### "Do Restaurants Match Neighbourhoods? Evidence from Yelp".
###################################################################################

library(tidyverse)
library(xtable)
library(geosphere)
library(sf)
library(cowplot)

#####################################################
### Figure 2: Spatial distribution of restaurants ### 
#####################################################

#Read and transform Yelp data.
berlin<-read.csv2("C:/Rdata/Winter20/yelp/berlin_data_final.csv",stringsAsFactors = F)
berlin$lat<-as.numeric(berlin$lat)
berlin$long<-as.numeric(berlin$long)
berlin$Preiskat<-berlin$Preiskat %>% str_replace("€€€€","4") %>% 
  str_replace("€€€","3") %>% str_replace("€€","2") %>% str_replace("€","1")
berlin$Preiskat<-as.numeric(berlin$Preiskat)

#Calculate distance to main station for each restaurant.
berlin_hbf <- c(13.369543840855453,52.52491374248917) #long, lat
berlin_hbf <- as.data.frame(t(berlin_hbf))
berlin$Distanzhbf <- distGeo(berlin_hbf,cbind(berlin$long,berlin$lat))/1000

#Plot: Kernel density estimate of distance to main station for the restaurants.
kde <- berlin %>% ggplot(aes(Distanzhbf)) + 
  geom_density(bw = bw.nrd(berlin$Distanzhbf), kernel = "gaussian",color = "lightcyan4", lwd = 0.9) +
  xlab("Shortest distance to Berlin main station in km")+ylab("Density estimate")+
  scale_x_continuous(breaks = seq(0,25,5), labels = as.character(seq(0,25,5)))+
  coord_fixed(ratio = 120)+
  theme_minimal() 

#Plot: Map of spatial distribution of the restaurants.
bluescale <- scale_fill_gradient(low="lightcyan1",high="cadetblue4",trans="log10") #colour scale
shape_dis <- read_sf(dsn = "C:/Rdata/Winter20/GEO_GRID_v9/vg250", layer = "VG250_F") #read shape file
berlin_shape <- shape_dis[shape_dis$ARS == "110000000000",]
berlin_shape_LAEA <- st_transform(berlin_shape, crs = "WGS84")

heat_map <- ggplot() + 
  geom_sf(data = berlin_shape_LAEA, colour = "black", fill = "gray90")+
  geom_bin2d(data=berlin,aes(x=long,y=lat),bins=c(150))+
  geom_point(data=berlin_hbf,aes(x=as.numeric(berlin_hbf[1]),y=as.numeric(berlin_hbf[2])),color = "chocolate3", size = 1.8)+
  labs(fill = "Number of restaurants")+
  theme_void()+
  theme(legend.direction = "vertical", 
        legend.position = c(0.87,0.8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))+
  bluescale

rest_spatial_dis <- plot_grid(heat_map, kde, scale = 0.975)

#ggsave("rest_spatial_dis.pdf", rest_spatial_dis, path = "C:/Rdata/Winter20/yelp",width = 10, height = 4.5)



###########################################
### Figure 3: Google Trends Time Series ###
###########################################

#Read Google Trends Data.
Date_seq <- seq.Date(as.Date("2010-01-01"),as.Date("2020-09-01"),"months")
google_US <- read.csv("C:/Rdata/Winter20/yelp/USA.csv")
google_Ger <- read.csv("C:/Rdata/Winter20/yelp/germany.csv")
google_trend <- data.frame(Date = Date_seq,US = google_US$counts,Germany = google_Ger$counts)

#Plot: Google Trends Scores
google_trend %>% pivot_longer(cols = - Date, names_to = "Country", values_to = "Score") %>% 
    ggplot(aes(x = Date, y = Score, col = Country)) + geom_line(lwd = 0.7) + ylab("Google Trends score")+
  theme_minimal() + scale_x_continuous(breaks = seq.Date(as.Date("2010-01-01"), as.Date("2020-01-01"),by = "year"), labels = as.character(2010:2020)) + 
  scale_color_manual(values = c("lightcyan4", "chocolate3"))

#ggsave("trend_plot.pdf", path = "C:/Rdata/Winter20/yelp",width = 6, height = 3)



###########################################################################################
### Figure 5: Spatial distribution of inhabitants and restaurants across the grid cells. ###
###########################################################################################

#Load geo grid data and grid shape file.
geo_grid <- read.csv("C:/Rdata/Winter20/GEO_GRID_v9/GEO_GRID_v9labels.csv")
grid_shape <- read_sf(dsn = "C:/Rdata/Winter20/GEO_GRID_v9/shape", layer = "ger_1km_rectangle")

#Load shape file of germany, extract Berlin and change projection to LAEA.
shape_dis <- read_sf(dsn = "C:/Rdata/Winter20/GEO_GRID_v9/vg250", layer = "VG250_F")
berlin_shape <- shape_dis[shape_dis$ARS == "110000000000",]
berlin_shape_LAEA <- st_transform(berlin_shape, crs = st_crs(grid_shape))

#Join the berlin shape file with the grid shape file (returning the grid cells).
berlin_LAEA_join <- st_join(grid_shape, berlin_shape_LAEA, join = st_intersects, left = F)

#Filter grids from geo-grid for Berlin and only take data for 2017.
geo_grid_berlin <- geo_grid[geo_grid$r1_id %in% berlin_LAEA_join$idm,]
geo_grid_berlin_2017 <- geo_grid_berlin[geo_grid_berlin$year == 2017,]

#Join the grid cells with the geo grid data.
geo_grid_join <- left_join(berlin_LAEA_join, geo_grid_berlin_2017, by = c("idm" = "r1_id"))
geo_grid_join$r1_ewa_a_gesamt[geo_grid_join$r1_ewa_a_gesamt == -1] <- NA

#Load webscraped berlin data and transform WGS coordinates to LAEA.
berlin_data <-read.csv2("C:/Rdata/Winter20/yelp/berlin_data_final.csv",stringsAsFactors = F)
berlin_data_sf <- berlin_data %>% st_as_sf(coords = c(11,10))
st_crs(berlin_data_sf) <- "WGS84"
berlin_data_LAEA <- st_transform(berlin_data_sf, crs = st_crs(grid_shape))

#Join webscraped data with grid data to find grid location of restaurants.
berlin_data_LAEA1 <- st_join(berlin_data_LAEA, berlin_LAEA_join, left = T, join = st_intersects)
class(berlin_data_LAEA1)
st_geometry(berlin_data_LAEA1) <- NULL

#Number of restaurants vs. number of inhabitants.
inhab_vs_rest <- berlin_data_LAEA1 %>% group_by(idm) %>% 
  summarise(anzahl = n()) %>% arrange(desc(anzahl)) %>% 
  right_join(x = ., y = geo_grid_join, by = "idm") %>% 
  select(idm, anzahl, r1_ewa_a_gesamt) 

inhab_vs_rest$r1_ewa_a_gesamt <- inhab_vs_rest$r1_ewa_a_gesamt/1000
inhab_vs_rest$r1_ewa_a_gesamt <- inhab_vs_rest$r1_ewa_a_gesamt+1

#Plot: spatial distribution of inhabitants.
bluescale <- scale_fill_gradient(low="lightcyan1",high="cadetblue4", na.value="gray90") 

inhab_plot <- ggplot() + geom_sf(data = geo_grid_join, aes(fill = r1_ewa_a_gesamt), color = NA) +
  geom_sf(data = berlin_shape_LAEA, colour = "black", fill = NA) +
  labs(fill = "Number of inhabitants")+
  theme_void()+
  theme(legend.direction = "vertical", 
        legend.position = c(0.87,0.8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))+
  bluescale

inhab_vs_rest <- left_join(inhab_vs_rest, berlin_LAEA_join, by = "idm") %>% st_as_sf()

#Plot: spatial distribution of restaurants.
rest_plot <- ggplot() + geom_sf(data = inhab_vs_rest, aes(fill = anzahl), color = NA) +
  geom_sf(data = berlin_shape_LAEA, colour = "black", fill = NA) +
  labs(fill = "Number of restaurants")+
  theme_void()+
  theme(legend.direction = "vertical", 
        legend.position = c(0.87,0.8),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))+
  bluescale

inhab_vs_rest_plot <- plot_grid(inhab_plot, rest_plot)

#ggsave("inhab_vs_rest_plot.pdf",inhab_vs_rest_plot, path = "C:/Rdata/Winter20/GEO_GRID_v9", width = 10, height = 4)

