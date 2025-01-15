# Sara Quaresima,
# Jun-Nov2024
# 
#  Map of hydro-graphic and topographic Po river basin 
#  Centrality measures
#  Network community
#  Distribution of centrality measures 
#  Directional network analysis
# ##################


## LOADING LIBRARY 
##################
library(ggplot2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(paletteer)
library(Hmisc)
library(ggarchery)
library(terra)
library(giscoR)
library(raster)
library(metR)
library(elevatr)
library(tidyverse)
library(terra)
library(whitebox)
library(ggnewscale)
library(tidyterra)
library(giscoR)
library(units)
library(ggblend)  
library(ggspatial)
##################


######################
wdir <- getwd()
dir.create(paste0(wdir, "/output_plot_V3/paper/"), recursive = T, showWarnings = F)
pathout<-paste0(wdir, "/output_plot_V3/paper/")
######################


# SHP PO BASIN (Autorità Bacino del Po)
# https://www.adbpo.it/
# https://www.adbpo.it/download/webgis/cartografia_di_base/
{
############################ 
shp <- read_sf('shp_po/Adb_PoBasinWGS84union.shp')
shp_wgs84<-sf::st_transform(shp, 4326) # trasformo lo shp in WGS84
#shp_po<-st_union(shp_wgs84$geometry) # faccio union dei vari sottobacini
shp_po<-shp_wgs84
# PO-MASK
POBBOX<-st_bbox(c(xmin = 5, xmax = 14.5, ymax = 43, ymin = 47.5), crs = st_crs(4326))%>%
     st_as_sfc() %>% 
     st_as_sf()
PO_mask <- st_difference(POBBOX, shp_po) # da usare per "grigettare"l'intorno del bacino del po
###########################################
###########################################
}

# SHP PO LAKES
# https://www.hydrosheds.org/products/hydrobasins
{
# HYDRO LAKES
po_lakes <- st_read("lake_po_shp/HydroLAKES_polys_v10.shp") 
sf_use_s2(FALSE)
PO_LAKE <- sf::st_intersection(po_lakes, shp_po)  
#filter the largest ones
PO_LAKE <- dplyr::filter(PO_LAKE, Lake_area > 30)
PO_LAKE <- PO_LAKE %>% filter(Lake_name != "Valli di Comacchio") # tolgo Valli di Comacchio perchè una "zona umida", non un vero lago
}

# SHP PO RIVERS
# https://www.hydrosheds.org/products/hydrobasins
{

# HYDRO RIVERS
po_rivers <- st_read("river_po_shp/HydroRIVERS_v10.shp")  
PO_RIVER <- sf::st_intersection(po_rivers, shp_po)
PO_RIVER2 <- st_cast(PO_RIVER, "MULTILINESTRING")

# https://milospopovic.net/map-rivers-with-sf-and-ggplot2-in-r/
river_width <- PO_RIVER2 %>%
  mutate(
    width = as.numeric(ORD_FLOW),
    width = case_when(
      width == 3 ~ 1,
      width == 4 ~ 0.8,
      width == 5 ~ 0.7,
      width == 6 ~ 0.0,
      width == 7 ~ 0.0,
      width == 8 ~ 0.0,
      TRUE ~ 0
      )) %>%
      sf::st_as_sf()
    


}



# mask SeaLand 
{  
shp_oce <- read_sf('/Users/saraquaresima/SHAPE_FILE/Ocean_shp/ne_10m_ocean/ne_10m_ocean.shp') 
shp_reg <- read_sf('/Users/saraquaresima/SHAPE_FILE/ITALY_ISTAT/Limiti01012022_g/Reg01012022_g/Reg01012022_g_WGS84.shp') 
}  

# dataset Antonio:
{
data <- read.csv(paste0(wdir, "/data/sara.csv"), sep='\t', header=T)
new_data<-data

# OutStrength:
new_data$outward_ori1_rad<-(new_data$dominant_outward_orientation_1*pi)/180
new_data$outward_ori2_rad<-(new_data$dominant_outward_orientation_2*pi)/180
new_data$outward_ori3_rad<-(new_data$dominant_outward_orientation_3*pi)/180
# InStrength:
new_data$inward_ori1_rad<-(new_data$dominant_inward_orientation_1*pi)/180
new_data$inward_ori2_rad<-(new_data$dominant_inward_orientation_2*pi)/180
}
   
# resize data bbox Po basin:
{
long <- new_data$lon
lat <- new_data$lat
points <- tibble::tibble("longitude" = long, "latitude" = lat)
sf_p <- sf::st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)
st_geometry(new_data)<-st_geometry(sf_p)

XLIM=c(6,14)
YLIM=c(43.5,47)

box_map <- st_bbox(c(xmin = XLIM[1], xmax = XLIM[2], ymin = YLIM[1], ymax = YLIM[2]), crs = st_crs(4326))%>%
     st_as_sfc() %>% 
     st_as_sf()

sf_use_s2(FALSE)
data_subset <- sf::st_intersection(new_data, box_map)

##############################################################################
DAT <- dplyr::select(data_subset, Nodo, lon, lat, 
                     InStrength, OutStrength,
                     DegreeCentrality, Clustering ,ClosenessCentrality, BetweennessCentrality,
                     outward_ori1_rad , outward_ori2_rad , outward_ori3_rad,
                     inward_ori1_rad, inward_ori2_rad ) %>%
                     
               mutate(norm_InStrength = (InStrength - min(InStrength))/(max(InStrength)- min(InStrength)),
                      norm_OutStrength = (OutStrength - min(OutStrength))/(max(OutStrength)- min(OutStrength)),
                      norm_DegreeCentrality = (DegreeCentrality - min(DegreeCentrality))/(max(DegreeCentrality)- min(DegreeCentrality)),
                      norm_Clustering = (Clustering - min(Clustering))/(max(Clustering)- min(Clustering)),
                      norm_ClosenessCentrality = (ClosenessCentrality - min(ClosenessCentrality))/(max(ClosenessCentrality)- min(ClosenessCentrality)),
                      norm_BetweennessCentrality  = (BetweennessCentrality  - min(BetweennessCentrality ))/(max(BetweennessCentrality )- min(BetweennessCentrality))
                      ) %>%
               mutate(#cuttedIS_quart =  ntile(norm_InStrength, 4),
                      cuttedIS_quart = cut2(norm_InStrength, cuts = as.numeric(quantile(norm_InStrength, prob = seq(0, 1, length = 5), type = 7))),
                      cuttedOS_quart = cut2(norm_OutStrength, cuts = as.numeric(quantile(norm_OutStrength, prob = seq(0, 1, length = 5), type = 7))),
                      cuttedDC_quart = cut2(norm_DegreeCentrality, cuts = as.numeric(quantile(norm_DegreeCentrality, prob = seq(0, 1, length = 5), type = 7))),
                      cuttedDC_equi = cut2(norm_DegreeCentrality, cuts = c(0, 0.25, 0.5, 0.75, 1)),
                      cuttedCl_quart = cut2(Clustering, cuts = as.numeric(quantile(norm_Clustering, prob = seq(0, 1, length = 5), type = 7))),
                      cuttedCC_quart = cut2(norm_ClosenessCentrality, cuts = as.numeric(quantile(norm_ClosenessCentrality, prob = seq(0, 1, length = 5), type = 7))),
                      cuttedBC_quart = cut2(norm_BetweennessCentrality, cuts = as.numeric(quantile(norm_BetweennessCentrality, prob = seq(0, 1, length = 5), type = 7)))
                      ) %>%
                mutate(
                      orient1_rad_OS = ifelse(as.numeric(cuttedOS_quart) < 3, NA, outward_ori1_rad),
                      orient2_rad_OS = ifelse(as.numeric(cuttedOS_quart) < 3, NA, outward_ori2_rad),
                      orient3_rad_OS = ifelse(as.numeric(cuttedOS_quart) < 3, NA, outward_ori3_rad),
                      point_IS = ifelse(as.numeric(cuttedIS_quart) < 3, 'NO', 'SI'),
                      orient1_rad_IS = ifelse(as.numeric(cuttedIS_quart) < 3, NA, inward_ori1_rad),
                      orient2_rad_IS = ifelse(as.numeric(cuttedIS_quart) < 3, NA, inward_ori2_rad)
                      )
}


# DEM
{
# setting up boundig box
locations <- data.frame(x = c(5, 14.5),
                        y = c(43, 47.5))
# get gem
dem <- get_elev_raster(locations = locations, prj = sf::st_crs(PO_mask), z = 10, clip = "bbox")

# convert to terra and mask area of interest
dem <- rast(dem) %>% 
       mask(vect(shp_po)) 

# reproject
dem <- project(dem, crs(shp_po))
# Convert elevation data to dataframe
demdf <- as.data.frame(dem, xy = TRUE)
colnames(demdf)[3] <- 'elevation'
# estimate the slope
sl <- terrain(dem, "slope", unit = "radians")
# estimate the aspect or orientation
asp <- terrain(dem, "aspect", unit = "radians")
# calculate the hillshade effect with 45º of elevation
hill_single <- shade(sl, asp, 
      angle = 45, 
      direction = 300,
      normalize= TRUE)

# final hillshade 
# convert the hillshade to xyz
hilldf_single <- as.data.frame(hill_single, xy = TRUE)
# pass multiple directions to shade()
hillmulti <- map(c(270, 15, 60, 330), function(dir){ 
                    shade(sl, asp, 
                          angle = 45, 
                          direction = dir,
                          normalize= TRUE)}
                 )
# create a multidimensional raster and reduce it by summing up
hillmulti <- rast(hillmulti) %>% sum()
# convert the hillshade to xyz
hillmultidf <- as.data.frame(hillmulti, xy = TRUE)
  
}

#####                 
# Centrality measures
# (ggarrange)
{
lab_clas <- c("LOW","MEDIUM-LOW","MEDIUM-HIGH","HIGH")                 
                   
# MAP fill "DegreeCentrality"
# quartili
###########################################
{
colo <- c('white', rev(hcl.colors(5, "Oranges"))[-c(1,5)])

pp1<-ggplot() +
   geom_tile(data= DAT,aes(x = lon, y = lat, 
                  fill = factor(cuttedDC_quart)), alpha = 1.0, color=NA) +
  
   scale_fill_manual(values = colo,
                     labels = lab_clas,
                     name = 'Degree Centrality') +
   
  guides(fill = guide_legend(title.position = "top")) +
  
   theme(legend.position = 'bottom',
         #legend.direction = 'vertical',
         legend.direction = 'horizontal',
         legend.title = element_text(size=8, face = "bold"),
         legend.text=element_text(size=6),
         legend.justification = c('left', 'top'),
         #legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "gray92", linewidth = 0.5, colour ="gray92"),
         axis.text = element_text(size=8),
         axis.title=element_text(size=8,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) 
  #labs(title = "(A)")
         #plot.margin = margin(0.3, 0.3, 0, 0, "cm"))

DC <- pp1 +
  geom_sf(data = PO_LAKE, fill = "dodgerblue3", colour = "dodgerblue3") +
  geom_sf(data = river_width, color = "dodgerblue3", aes(alpha = factor(ORD_FLOW))) +
  scale_alpha_manual(guide = 'none',
                     values = c(
                     "3" = 1,
                     "4" = 1,
                     "5" = 1,
                     "6" = 0,
                     "7" = 0,
                     "8" = 0)) +
  geom_sf(data = shp_po, fill = NA , colour = 'black', linewidth = 1) +
  geom_sf(data = PO_mask, fill = "grey" , colour = NA, linewidth = 1, alpha=0.7) +
  geom_sf(data = shp_oce, fill = 'whitesmoke', colour = 'black', linewidth = 0.4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T) +
  scale_y_continuous(breaks = seq(44,47,1)) 
###########################################
}
# MAP fill "BetweennessCentrality"
# quartili
###########################################
{
colo <- c('white', rev(hcl.colors(5, "Reds2"))[-c(1,5)])
 
pp1<-ggplot() +
   geom_tile(data= DAT,aes(x = lon, y = lat, 
                  fill = factor(cuttedBC_quart)), alpha = 1, color=NA) +
   
   scale_fill_manual(values = colo,
                     labels = lab_clas, 
                     name = 'Betweenness Centrality') +
   
   guides(fill = guide_legend(title.position = "top")) +

   theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=8, face = "bold"),
         legend.text=element_text(size=6),
         legend.justification = c('left', 'top'),
         #legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "gray92", linewidth = 0.5, colour ="gray92"),
         axis.text = element_text(size=8),
         axis.title=element_text(size=8,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5))

BC <- pp1 +
  geom_sf(data = PO_LAKE, fill = "dodgerblue3", colour = "dodgerblue3") +
  geom_sf(data = river_width, color = "dodgerblue3", aes(alpha = factor(ORD_FLOW))) +
  scale_alpha_manual(guide = 'none',
  values = c(
  "3" = 1,
  "4" = 1,
  "5" = 1,
  "6" = 0,
  "7" = 0,
  "8" = 0)) +
  geom_sf(data = shp_po, fill = NA , colour = 'black', linewidth = 1) +
  geom_sf(data = PO_mask, fill = "grey" , colour = NA, linewidth = 1, alpha=0.7) +
  geom_sf(data = shp_oce, fill = 'whitesmoke', colour = 'black', linewidth = 0.4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T) +
  scale_y_continuous(breaks = seq(44,47,1)) 
###########################################
}
# MAP fill "ClosenessCentrality"
# quartili
###########################################
{
colo <- c('white', rev(hcl.colors(5, "Peach"))[-c(1,5)])
pp1<-ggplot() +
   geom_tile(data= DAT,aes(x = lon, y = lat, 
                  fill = factor(cuttedCC_quart)), alpha = 1, color=NA) +
   
   scale_fill_manual(values = colo, 
                     labels = lab_clas,
                     name = 'Closeness Centrality') +
    
  guides(fill = guide_legend(title.position = "top")) +

    theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=8, face = "bold"),
         legend.text=element_text(size=6),
         legend.justification = c('left', 'top'),
         #legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "gray92", linewidth = 0.5, colour ="gray92"),
         axis.text = element_text(size=8),
         axis.title=element_text(size=8,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5))

 CC <- pp1 +
  geom_sf(data = PO_LAKE, fill = "dodgerblue3", colour = "dodgerblue3") +
  geom_sf(data = river_width, color = "dodgerblue3", aes(alpha = factor(ORD_FLOW))) +
  scale_alpha_manual(guide = 'none',
                     values = c(
                     "3" = 1,
                     "4" = 1,
                     "5" = 1,
                     "6" = 0,
                     "7" = 0,
                     "8" = 0)) +
  geom_sf(data = shp_po, fill = NA , colour = 'black', linewidth = 1) +
  geom_sf(data = PO_mask, fill = "grey" , colour = NA, linewidth = 1, alpha=0.7) +
  geom_sf(data = shp_oce, fill = 'whitesmoke', colour = 'black', linewidth = 0.4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T) +
  scale_y_continuous(breaks = seq(44,47,1)) 
###########################################
}

mix_CENTR<-ggpubr::ggarrange(DC,
                             CC,
                             BC, 
                       ncol=3, nrow=1,
                       labels = c("(A)","(B)","(C)"),
                       common.legend = F,
                       heights = c(1),
                       align = "v",
                       font.label = list(size = 12, face = "bold", color ="black"))

mix_CENTR<-ggpubr::ggarrange(DC,
                             CC + rremove('ylab') + rremove('y.text'),
                             BC + rremove('ylab') + rremove('y.text'), 
                       ncol=3, nrow=1,
                       labels = c("(A)","(B)","(C)"),
                       common.legend = F,
                       align = "v",
                       font.label = list(size = 12, face = "bold", color ="black"))

ggsave(paste0(pathout, "Centrality-measures_v1.png"), 
       plot = mix_CENTR, 
       width = 30, height = 10, units = "cm",
       bg = "white")
}

#####
#####

#####
# Directional network analysis
# (ggarrange)
{
lab_clas <- c("LOW","MEDIUM-LOW","MEDIUM-HIGH","HIGH")                 
                   
# MAP fill "OutStrength"
# quartili
###########################################
{
colo <- c('white', '#FFC08E','#EC5D2F','#88002D')
pp1<-ggplot() +
   geom_tile(data= DAT,aes(x = lon, y = lat, 
                  fill = factor(cuttedOS_quart)), alpha = 0.7, color=NA) +
  
   scale_fill_manual(values = colo,
                     labels = lab_clas,
                     name = 'Out Strength') +
  
  guides(fill = guide_legend(title.position = "top")) +

   theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=8, face = "bold"),
         legend.text=element_text(size=6),
         legend.justification = c('left', 'top'),
         #legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "gray92", linewidth = 0.5, colour ="gray92"),
         axis.text = element_text(size=6),
         axis.title=element_text(size=8,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +

   geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient1_rad_OS,
                  radius = 0.14), arrow = arrow(length = unit(.1, 'cm')), linewidth = 0.2) +
   geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient2_rad_OS,
                  radius = .14), arrow = arrow(length = unit(.1, 'cm')), linewidth = 0.2) +
   geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient3_rad_OS,
                  radius = .14), arrow = arrow(length = unit(.1, 'cm')), linewidth = 0.2)

OS <- pp1 +
  geom_sf(data = PO_LAKE, fill = "dodgerblue3", colour = "dodgerblue3") +
  geom_sf(data = river_width, color = "dodgerblue3", aes(alpha = factor(ORD_FLOW))) +
  scale_alpha_manual(guide = 'none',
                     values = c(
                     "3" = 1,
                     "4" = 1,
                     "5" = 1,
                     "6" = 0,
                     "7" = 0,
                     "8" = 0)) +
  geom_sf(data = shp_po, fill = NA , colour = 'black', linewidth = 1) +
  geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient1_rad_OS,
                  radius = .14), arrow = arrow(length = unit(.1, 'cm')), linewidth = 0.2) +
   geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient2_rad_OS,
                  radius = .14), arrow = arrow(length = unit(.1, 'cm')), linewidth = 0.2) +
   geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient3_rad_OS,
                  radius = .14), arrow = arrow(length = unit(.1, 'cm')), linewidth = 0.2) +
  geom_sf(data = PO_mask, fill = "grey" , colour = NA, linewidth = 1, alpha=0.7) +
  geom_sf(data = shp_oce, fill = 'whitesmoke', colour = 'black', linewidth = 0.4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T) + 

  scale_y_continuous(breaks = seq(44,47,1)) 
}

# MAP fill "InStrength" pallino
# quartili
###########################################
{
colo <- c('white', '#FFC08E','#EC5D2F','#88002D')
pp1<-ggplot() +
   geom_tile(data= DAT,aes(x = lon, y = lat, 
                  fill = factor(cuttedIS_quart)), alpha = 0.7, color=NA) +
   
   scale_fill_manual(values = colo,
                     labels = lab_clas,
                     name = 'In Strength') +
  
    guides(fill = guide_legend(title.position = "top")) +

   theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=8, face = "bold"),
         legend.text=element_text(size=6),
         legend.justification = c('left', 'top'),
        #legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "gray92", linewidth = 0.5, colour ="gray92"),
         axis.text = element_text(size=6),
         axis.title=element_text(size=8,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +


  geom_point(data = DAT, aes(x = lon, y = lat, color=point_IS), size=0.4, show.legend = F) +
  scale_color_manual(values=c( "transparent" ,"black")) +
  
  geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient1_rad_IS,
                  radius = .14), linewidth = 0.2) +
  geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient2_rad_IS,
                  radius = .14), linewidth = 0.2)

IS <- pp1 +
  geom_sf(data = PO_LAKE, fill = "dodgerblue3", colour = "dodgerblue3") +
  geom_sf(data = river_width, color = "dodgerblue3", aes(alpha = factor(ORD_FLOW))) +
  scale_alpha_manual(guide = 'none',
                     values = c(
                     "3" = 1,
                     "4" = 1,
                     "5" = 1,
                     "6" = 0,
                     "7" = 0,
                     "8" = 0)) +
  geom_sf(data = shp_po, fill = NA , colour = 'black', linewidth = 1) +
  
  geom_point(data = DAT, aes(x = lon, y = lat, color=point_IS), size=0.4, show.legend = F) +

  geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient1_rad_IS,
                  radius = .14), linewidth = 0.2) +
  geom_spoke(data = DAT, aes(x = lon, y = lat, 
                  angle = orient2_rad_IS,
                  radius = .14), linewidth = 0.2) +
  geom_sf(data = PO_mask, fill = "grey" , colour = NA, linewidth = 1, alpha=0.7) +
  geom_sf(data = shp_oce, fill = 'whitesmoke', colour = 'black', linewidth = 0.4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T) +
  scale_y_continuous(breaks = seq(44,47,1)) 

###########################################
}


mix_STRE<-ggpubr::ggarrange(IS,
                            OS, 
                       ncol=2, nrow=1,
                       labels = c("(A)","(B)"),
                       common.legend = F,
                       align = "v",
                       font.label = list(size = 10, face = "bold", color ="black")) 

mix_STRE<-ggpubr::ggarrange(IS,
                            OS + rremove('ylab') + rremove('y.text'), 
                       ncol=2, nrow=1,
                       labels = c("(A)","(B)"),
                       common.legend = F,
                       align = "v",
                       font.label = list(size = 10, face = "bold", color ="black")) 

ggsave(paste0(pathout, "Strength-measures_v1.png"), 
       plot = mix_STRE, 
       width = 20, height = 10, units = "cm",
       bg = "white")
}

#####
#####



# DEM + CLUSTER
############################################
{

data_cd <- read.csv(paste0(wdir, '/data/community_detection_po_basin.csv'), sep=';', header=T, dec=",") # 08/10/2024

lon <- data_cd$lon
lat <- data_cd$lat
points <- tibble::tibble("longitude" = lon, "latitude" = lat)
sf_p <- sf::st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)
st_geometry(data_cd)<-st_geometry(sf_p)
sf_use_s2(FALSE)
data_com <- sf::st_intersection(data_cd, shp_po)
  
# V2: dem colorato2 , simboli colorati
{
# map
m<-ggplot() +
  geom_raster(data = hillmultidf,
              aes(x, y, fill = sum),
              show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys", 
                       breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  new_scale_fill() +
  geom_raster(data = demdf,
              aes(x, y, fill = elevation),
              alpha = .7) +

  scale_fill_wiki_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  
  geom_point(data= data_com,
             aes(x = lon, y = lat, color = factor(community), shape = factor(community)), 
             size = 3) +
  
  scale_color_manual(name = "",
                     breaks = c("0","1", "2"),
                     values = c("blue","red2", "gold"),
                     labels = c("Community 1", "Community 2", "Community 3") ) +
  scale_shape_manual(name = "",
                     breaks = c("0","1", "2"),
                     values = c(15, 16, 17),
                     labels = c("Community 1", "Community 2", "Community 3") ) +
  
  guides(fill = guide_colorsteps(barwidth = 40,
                                 barheight = .5,
                                 title.position = "right")) +
  
  labs(fill = "m") +

  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.direction = 'horizontal',
        legend.box="vertical",
        legend.title = element_text(size=20, face = "bold"),
        legend.text=element_text(size=20),
        legend.justification = c('center', 'top'),
        legend.margin = margin(6,6,6,1),
        legend.background = element_rect(fill = "gray92", linewidth = 0.5, colour ="gray92"),
        axis.text = element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.8))

m2 <- m +
  geom_sf(data = PO_LAKE, fill = "dodgerblue3", colour = "dodgerblue3") +
  geom_sf(data = river_width, color = "dodgerblue3", aes(alpha = factor(ORD_FLOW))) +
  scale_alpha_manual(guide = 'none',
                     values = c(
                     "3" = 1,
                     "4" = .8,
                     "5" = .7,
                     "6" = 0,
                     "7" = 0,
                     "8" = 0)) +
  geom_sf(data = shp_po, fill = NA , colour = 'black', linewidth = 1) +
  geom_sf(data = PO_mask, fill = "grey" , colour = NA, linewidth = 1, alpha=0.7) +
  geom_sf(data = shp_oce, fill = 'whitesmoke', colour = 'black', linewidth = 0.4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T) +
  geom_point(data= data_com,
             aes(x = lon, y = lat, colour = factor(community), shape = factor(community)),
             size = 3) 

ggsave(paste0(pathout, "dem_community_v2.png"), plot = m2,
       width = 30, 
       height = 20, 
       units = "cm")

}



}


# Map of hydro-graphic and topographic Po river basin along with the ERA5 grid points
############################################
{
# italy 
{
italy <- ggplot() +
  geom_sf(data = st_union(shp_reg$geometry), fill = NA, colour = 'black', linewidth = 0.4) +
  #geom_sf(data = PO_for_mask, fill = NA , colour = 'black', linewidth = 0.5)
  geom_sf(data = shp_po, fill = "darkgreen" , colour = 'black', linewidth = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

italygrob<-ggplotGrob(italy)
}

# map
m<-ggplot() +
  geom_raster(data = hillmultidf,
              aes(x, y, fill = sum),
              show.legend = FALSE) +
  scale_fill_distiller(palette = "Greys", 
                       breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  new_scale_fill() +
  geom_raster(data = demdf,
              aes(x, y, fill = elevation),
              alpha = .7) +

  scale_fill_wiki_c(breaks = c(180, 250, 500, 1000,
                                     1500,  2000, 2500,
                                     3000, 3500, 4000)) +
  
  guides(fill = guide_colorsteps(barwidth = 40,
                                 barheight = .5,
                                 title.position = "top")) +
  
  labs(fill = "Elevation (m)") +

  theme(legend.position = "bottom",
        #plot.title = element_text(hjust = 0.5),
        legend.direction = 'horizontal',
        legend.box="vertical",
        legend.title = element_text(size=20, face = "bold"),
        legend.text=element_text(size=20),
        legend.justification = c('center', 'top'),
        legend.margin = margin(6,6,6,1),
        legend.background = element_rect(fill = "gray92", linewidth = 0.5, colour ="gray92"),
        axis.text = element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.8))

m2 <- m +
  geom_sf(data = PO_LAKE, fill = "dodgerblue3", colour = "dodgerblue3") +
  geom_sf(data = river_width, color = "dodgerblue3", aes(alpha = factor(ORD_FLOW))) +
  scale_alpha_manual(guide = 'none',
                     values = c(
                     "3" = 1,
                     "4" = .8,
                     "5" = .7,
                     "6" = 0,
                     "7" = 0,
                     "8" = 0)) +
    geom_point(data= DAT,
             aes(x = lon, y = lat, color = 'black'),
             size = 0.7) +
  scale_color_manual(name = "Grid points",
                     values = c("black"),
                     labels = NULL) +
  geom_sf(data = shp_po, fill = NA , colour = 'black', linewidth = 1) +
  geom_sf(data = PO_mask, fill = "grey" , colour = NA, linewidth = 1, alpha=0.7) +
  geom_sf(data = shp_oce, fill = 'whitesmoke', colour = 'black', linewidth = 0.4) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T) +
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(which_north = "true",
                         height = unit(1.2, "cm"),
                         width = unit(1.2, "cm"),
                         pad_x = unit(0.5, "cm"),
                         pad_y = unit(0.5, "cm"),
                         location = "bl",
                         style = north_arrow_fancy_orienteering)
  m3 <- m2 + annotation_custom(grob = italygrob, 
                       xmin = 13, xmax = 14.2,
                       ymin = 46, ymax = 47.1)

ggsave(paste0(pathout, "dem_introduction_gridpoints_hd_var1.png"), plot = m4,
       width = 30, 
       height = 20, 
       units = "cm")

}




# boxplot 
# Distribution of Centrality measures within the communities. 
#############################################
{
data_cd <- read.csv(paste0(wdir, '/data/community_detection_po_basin.csv'), sep=';', header=T, dec=",") # 08/10/2024

Centrality_meas <- data_cd %>%
  select(DegreeCentrality, ClosenessCentrality, BetweennessCentrality, community)

Centrality_meas <- Centrality_meas %>%
  mutate(community=recode(community, 
                          "0" = 1,
                          "1" = 2,
                          "2" = 3 )) 

CM <- Centrality_meas %>% 
      reshape::melt(measure.vars = c("DegreeCentrality", "ClosenessCentrality", "BetweennessCentrality"),
                    variable_name = "CentralityMeasure")


# Degree Centrality
{
CM_DC <- rbind(filter(CM, CentralityMeasure == "DegreeCentrality") %>%
  mutate(community=replace(community, community > 0, 0)),
  filter(CM, CentralityMeasure == "DegreeCentrality"))

pp_DC <- ggplot(CM_DC, aes(x = community, y = value, fill = factor(community) )) +
         geom_boxplot() +
         theme_bw() +
         scale_fill_manual(name = "",
                breaks = c("0","1", "2", "3"),
                values = c("grey", "blue","red2", "gold"),
                labels = c("All", "Community 1", "Community 2", "Community 3") ) +
         theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5), 
            axis.text = element_text(size=14),
            legend.position = "bottom",
            legend.direction = 'horizontal',
            legend.text=element_text(size=14)) +
         ggtitle("Degree Centrality")
  }     

# Closness Centrality
{
CM_CC <- rbind(filter(CM, CentralityMeasure == "ClosenessCentrality") %>%
  mutate(community=replace(community, community > 0, 0)),
  filter(CM, CentralityMeasure == "ClosenessCentrality"))

pp_CC <- ggplot(CM_CC, aes(x = community, y = value, fill = factor(community))) +
         geom_boxplot() +
         theme_bw() +
         scale_fill_manual(name = "",
                         breaks = c("0","1", "2", "3"),
                         values = c("grey", "blue","red2", "gold"),
                         labels = c("All", "Community 1", "Community 2", "Community 3") ) +
         theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size=14)) +
         ggtitle("Closeness Centrality")
}

# Betweenness Centrality
{
CM_BC <- rbind(filter(CM, CentralityMeasure == "BetweennessCentrality") %>%
  mutate(community=replace(community, community > 0, 0)),
  filter(CM, CentralityMeasure == "BetweennessCentrality"))

pp_BC <- ggplot(CM_BC, aes(x = community, y = value, fill = factor(community))) +
         geom_boxplot() +
         theme_bw() +
         scale_fill_manual(name = "",
                         breaks = c("0","1", "2", "3"),
                         values = c("grey", "blue","red2", "gold"),
                         labels = c("All", "Community 1", "Community 2", "Community 3") ) +
         theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(size=14)) +
         ggtitle("Betweenness Centrality")
  }   

figure = ggpubr::ggarrange(pp_DC, pp_CC, pp_BC,
                           ncol = 3, nrow = 1,
                           legend = "none") %>%
         gridExtra::grid.arrange(ggpubr::get_legend(pp_DC), heights = unit(c(80, 5), "mm"))

ggsave(paste0(pathout, "Distr_CentralityMeasures_community.png"), plot = figure,
       width = 30, 
       height = 10, 
       units = "cm")

}
###########################



############################
############################
############################
############################
############################
############################





