# Sara Quaresima,
# Nov2024
# 
# computing SPI from average tp on PO basin
##################


## LOADING LIBRARY 
##################
library(terra)
library(SPEI)
library(ggplot2)
library(sf)
library(dplyr)
library(ggpubr)
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
}  

######################
# Data:
# 
# tp: cumulati mensili prec from 01/1950 to 12/2023
{
tp <- rast(paste0(wdir, "/data/Rid_tp_Monthly_ERA5_1940-2023.nc"))
shp <- vect(paste0(wdir,'/shp_po/Adb_PoBasinWGS84union.shp'))
shp <- project(shp, "+proj=longlat")
crs(shp) <- crs(tp) # trasformo lo shp in WGS84


# subset from JAN 1980 to DEC 2023:
tp_79_23 <- subset(tp, time(tp) >= "1979-01-01")

# crop su shp PO:
tp_PO <- crop(tp_79_23, shp, mask=T)
tp_PO_mean <- global(tp_PO, 'mean', na.rm=T) # faccio una media areale dell tp from1980 to 2023
}
######################

# SPI:
{
spi12<- spi(tp_PO_mean$mean, 12)

plot(spi12, type='l')
spi12_num <- spi12$fitted[13:540]
df_spi12<-data.frame(1:length(spi12_num), spi12_num)
names(df_spi12)<-c("time", "spi12")

}
######################

######################
# ts SPI plot with labels (Duration, Peak, Severity)
{
pp <- ggplot(data = df_spi12, aes(x = time)) + 
      geom_ribbon(aes(ymax = -1, ymin = ifelse(spi12 > -1, -1, spi12), fill = "")) +
      geom_line(aes(y = spi12),lwd=1.5, col="grey28") +
      geom_hline(yintercept = -1, col="red", lty=2) +
      scale_fill_manual(values = c("red"), name = NULL) +
      theme_bw() +
      scale_x_continuous(breaks = seq(1,528,24), labels =  seq(1980,2023,2)) +
      theme(axis.text.y = element_text(size=20, face ="bold"),
            axis.text.x = element_text(size=20, face ="bold", angle = 90, vjust = 0.5, hjust=1),
            axis.title=element_text(size=20,face="bold"),
            legend.position="none") +
      xlab("Time") +
      ylab("SPI")
  
# Add labels
# severity
pp1<- pp + annotate(geom = "segment", x = 495, y = -1.7, xend = 515, yend = -1.2,
              arrow = arrow(length = unit(3, "mm")),lwd=1.2) +
     annotate(geom = "text", x = 495, y =  -1.7, label = "Severity",
               hjust = "right", fontface="bold", size=8) +
# peak
     annotate(geom = "segment", x = 136, y = -1, xend = 136, yend = min(df_spi12$spi12),
              arrow = arrow(end ="both", length = unit(3, "mm")),lwd=1.2) +
     annotate(geom = "segment", x = 130, y = -1, xend = 142, yend = -1, lwd=1.2) +
     annotate(geom = "segment", x = 130, y = min(df_spi12$spi12), xend = 142, yend = min(df_spi12$spi12), lwd=1.2) +
     annotate(geom = "text", x = 144, y =  -1.9, label = "Peak",
              hjust = "left", fontface="bold", size=8) +

# duration
     annotate(geom = "segment", x = 318, y = -0.5, xend = 329, yend = -0.5, lwd=1.2) +
     annotate(geom = "segment", x = 318, y = -0.45, xend = 318, yend = -0.55, lwd=1.2) +
     annotate(geom = "segment", x = 329, y = -0.45, xend = 329, yend = -0.55, lwd=1.2) +
     annotate(geom = "segment", x = 323, y = 0, xend = 323, yend = -0.5,
              arrow = arrow(length = unit(3, "mm")),lwd=1.2) +
     annotate(geom = "text", x = 323, y =  0.1, label = "Duration",
               hjust = "center", fontface="bold", size=8) 
   
ggsave(paste0(pathout, "SPI_Po-basin_1980-2023_labels_var3.png"), plot = pp1,
       width = 50, height = 20, units = "cm")

}

######################
######################

######################
# Data per mappe introduttive: Frequency, Duration, Severity, Peak

data <- read.csv(paste0(wdir, "/data/Spell_features.csv"), sep=',', header=T)# secondo dataset 06/06/2024
######################

XLIM=c(6,14)
YLIM=c(43.5,47)

long <- data$x
lat <- data$y
points <- tibble::tibble("longitude" = long, "latitude" = lat)
sf_p <- sf::st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)
st_geometry(data)<-st_geometry(sf_p)

box_map <- st_bbox(c(xmin = XLIM[1], xmax = XLIM[2], ymin = YLIM[1], ymax = YLIM[2]), crs = st_crs(4326))%>%
     st_as_sfc() %>% 
     st_as_sf()

sf_use_s2(FALSE)
data_subset <- sf::st_intersection(data, box_map)
##################
##################

# map Frequency
{
##########################
colo <- rev(hcl.colors(10, "RdYlGn"))

Freq_pp1<-ggplot() +
   geom_tile(data= data_subset, aes(x = x, y = y, 
                  fill = Frequency), alpha = 1, color = NA) +

   scale_fill_gradientn(colors = colo, # scala verde/rosso
                        breaks=c(min(data$Frequency), round(max(data$Frequency),digits=3)),
                        labels=c(paste0('Low: ',min(data$Frequency)) , paste0('High: ', round(max(data$Frequency),digits=3))),
                        limits=c(min(data$Frequency), round(max(data$Frequency),digits=3)) ) +

   theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=14, face = "bold", hjust= 1, vjust=1),
         legend.text=element_text(size=14),
         legend.justification = c('left', 'top'),
         legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "white", linewidth = 0.5, colour =NA),
         axis.text = element_text(size=10),
         axis.title=element_text(size=10,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5),
         plot.margin = margin(0.1,0,0.1,0.1, "cm"))

Freq_pp2 <- Freq_pp1 +
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
  coord_sf(xlim=XLIM, ylim=YLIM,expand = T)+
  scale_y_continuous(breaks = seq(44,47,1)) 
  
#######################################
}

# map Duration
{
##########################
colo <- rev(hcl.colors(10, "RdYlGn"))

Dura_pp1<-ggplot() +
   geom_tile(data= data_subset, aes(x = x, y = y, 
                  fill = Duration), alpha = 1, color = NA) +

   scale_fill_gradientn(colors = colo, # scala verde/rosso
                        breaks=c(min(data$Duration), round(max(data$Duration),digits=3)),
                        labels=c(paste0('Low: ',min(data$Duration)) , paste0('   High: ', round(max(data$Duration),digits=3))),
                        limits=c(min(data$Duration), round(max(data$Duration),digits=3)) ) +

   theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=14, face = "bold", hjust= 1, vjust=1),
         legend.text=element_text(size=14),
         legend.justification = c('left', 'top'),
         legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "white", linewidth = 0.5, colour =NA),
         axis.text = element_text(size=10),
         axis.title=element_text(size=10,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5),
         plot.margin = margin(0.1,0,0.1,0.1, "cm"))

Dura_pp2 <- Dura_pp1 +
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

#######################################
}

# map Peak
{
##########################
colo <- (hcl.colors(10, "RdYlGn")) 

Peak_pp1<-ggplot() +
   geom_tile(data= data_subset, aes(x = x, y = y, 
                  fill = Peak), alpha = 1, color = NA) +

   scale_fill_gradientn(colors = colo, # scala verde/rosso
                        breaks=c(min(data$Peak), round(max(data$Peak), digits=3)),
                        labels=c(paste0('High: ', min(data$Peak)) , paste0('    Low: ', round(max(data$Peak),digits=3))),
                        limits=c(min(data$Peak), max(data$Peak))) +

   theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=14, face = "bold", hjust= 1, vjust=1),
         legend.text=element_text(size=14),
         legend.justification = c('left', 'top'),
         legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "white", linewidth = 0.5, colour =NA),
         axis.text = element_text(size=10),
         axis.title=element_text(size=10,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5),
         plot.margin = margin(0.1,0,0.1,0.1, "cm"))

Peak_pp2 <- Peak_pp1 +
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


#######################################
}

# map Severity
{
##########################
colo <- (hcl.colors(10, "RdYlGn"))
#"#A51122" "#DC4A00" "#E98E00" "#F1C363" "#FAEDA9" "#EAF3A3" "#B4D66C" "#76B345" "#3A8B3A" "#006228"
Seve_pp1<-ggplot() +
   geom_tile(data= data_subset, aes(x = x, y = y, 
                  fill = Severity), alpha = 1, color = NA) +

   scale_fill_gradientn(colors = colo, # scala verde/rosso
                        breaks=c(min(data$Severity), round(max(data$Severity), digits=3)),
                        labels=c(paste0('High: ', min(data$Severity)) , paste0('         Low: ', round(max(data$Severity),digits=3))),
                        limits=c(min(data$Severity), max(data$Severity))) +

   theme(legend.position = 'bottom',
         legend.direction = 'horizontal',
         legend.title = element_text(size=14, face = "bold", hjust= 1, vjust=1),
         legend.text=element_text(size=14),
         legend.justification = c('left', 'top'),
         legend.margin = margin(6,6,6,1),
         legend.background = element_rect(fill = "white", linewidth = 0.5, colour =NA),
         axis.text = element_text(size=10),
         axis.title=element_text(size=10,face="bold"),
         plot.background = element_rect(fill = "white"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_blank(),
         panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5),
         plot.margin = margin(0.1,0,0.1,0.1, "cm"))

Seve_pp2 <- Seve_pp1 +
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

#######################################
}

mix<-ggpubr::ggarrange(Freq_pp2,
                       Dura_pp2,
                       Seve_pp2,
                       Peak_pp2, 
                       ncol=2, nrow=2,
                       labels = c("(A)", "(B)","(C)", "(D)"),
                       font.label = list(size = 12, face = "bold", color ="black")) 

ggsave(paste0(pathout, "Freq-Dura-Seve_Peak_v1.png"), 
       plot = mix, 
       width = 30, height = 20, units = "cm",
       bg = "white")

############################
############################
############################
############################
############################
############################
