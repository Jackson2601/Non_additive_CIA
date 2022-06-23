
###########################################################################################
##################################### 03_Figures ##########################################

library(tidyverse)
library(raster)
library(RColorBrewer)
library(rasterVis)
library(gridExtra)
library(viridis)
library(sf)
library(ggspatial)


### Calculate overall median and SD, and the range 

CI_add <- stack("r_CI_add.gri")
CI_non.add <- stack("r_CI.gri")

add_vect <- as.vector(CI_add) # These vectors are far too large to plot
add_median <- median(add_vect, na.rm=T)
add_mad <- mad(add_vect, na.rm=T)

non.add_vect <- as.vector(CI_non.add) # These vectors are far too large to plot
non.add_median <- median(non.add_vect, na.rm=T)
non.add_mad <- mad(non.add_vect, na.rm=T)

CI_add_min <- cellStats(CI_add, "min")
CI_add_max <- cellStats(CI_add, "max")
CI_non.add_min <- cellStats(CI_non.add, "min")
CI_non.add_max <- cellStats(CI_non.add, "max")

#### NOTE: None of these values have been inverted such that high scores mean high impact ####

add_median <- add_median*-1 
CI_add_max <- CI_add_max*-1 
CI_add_min <- CI_add_min*-1 
CI_non.add_max <- CI_non.add_max*-1 
CI_non.add_min <- CI_non.add_min*-1
non.add_median <- non.add_median*-1


### Create dataframe of raster values

add_df <- as.data.frame(rasterToPoints(CI_additive))
non.add_df <- as.data.frame(rasterToPoints(CI_non_additive))

add_df$CI_model <- "Additive" 
non.add_df$CI_model <- "Interaction" 

add_df <- rename(add_df, "value" = "layer.2")
non.add_df <- rename(non.add_df, "value" = "layer.2") 

CI_df <- rbind(add_df, non.add_df)

### Plot frequency of values
### Shows that higher values are more prevalent in the non-additive map

CI_hist <- ggplot(CI_df, aes(x=value, fill=CI_model)) +
  geom_histogram(position="dodge", bins = 35, alpha = 0.6, colour = "black") +
  geom_vline(aes(xintercept=add_median),
             linetype="solid", size = 1, colour="black", show.legend = F) +
  geom_vline(aes(xintercept=non.add_median),
             linetype="dashed", size = 1, colour="black", show.legend = F) + 
  scale_x_continuous(name="Cumulative impact score", limits=c(0, 3), breaks = seq(0, 3, 1)) +
  scale_fill_manual(values = c("lightblue3","yellow2")) + 
  theme_bw() + ylab(NULL) + 
  theme(axis.text.x = element_text(size = "12")) +
  theme(axis.title.x = element_text(size = "15")) +
  theme(axis.text.y = element_text(size = "12")) +
  theme(legend.title = element_blank()) +
  theme(legend.key.size = unit(1.2, 'cm')) + 
  theme(legend.text = element_text(size=12)) +
  theme(legend.position = c(.9, .80)) 


# Second histogram to show higher values

CI_hist_2 <- ggplot(CI_df, aes(x=value, fill=CI_model)) +
  geom_histogram(bins = 35, alpha = 0.6, colour = "black") + 
  scale_x_continuous(limits=c(2, 7.5), breaks = seq(2, 7.5, 1)) + 
  scale_fill_manual(values = c("yellow2")) +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(size = "20")) +
  theme(axis.text.y = element_text(size = "20")) +
  theme(legend.position="none")


CI_add <- stack("r_CI_main_ALL")*-1 # inverted so high values mean high impact
CI_non.add <- stack("r_CI_stack")*-1

## We are using the inverted median values from the quantiles for maps
CI_additive <- stack("quantiles_ALL")
CI_non_additive <- stack("quantiles_stack")
CI_additive <- CI_additive$layer.2*-1
CI_non_additive <- CI_non_additive$layer.2*-1


## Set the scale so they have the same min and max range
## The non-additive range is the widest, so we will use these values as the min and max

CI_min <- cellStats(CI_non_additive, "min")
CI_max <- cellStats(CI_non_additive, "max")


## Map of the South Australia state lines

# Read in a shapefile of Australia:
SA <- read_sf("SAcoast.shp")

# Convert to correct crs
proj4string(CI_additive)
SA_trns <- as_Spatial(st_transform(SA, "+proj=lcc +lat_0=-32 +lon_0=135 +lat_1=-28 +lat_2=-36 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"))
plot(SA_trns)

## Colour scheme
mycols <- (c("lightskyblue1", "lightskyblue3", "lightskyblue3", "blue4", "royalblue2", 
             "royalblue4", "navy", "navyblue"))
cols <- colorRampPalette(mycols)
myTheme <- rasterTheme(region = cols(4))
my.at <- seq(CI_min, CI_max, length=100)
my.labels <- list(at=c(CI_min, CI_max), labels=c("Low", "High"), cex = 1.2)

## Additive vs non-additive with additive SMD for a3S1S2
Add <- levelplot(CI_additive, margin=FALSE, maxpixels=1e6,  
                 par.settings=myTheme, at = my.at, 
                 colorkey=F, scales=list(draw=FALSE)) + 
  latticeExtra::layer(sp.polygons(SA_trns, fill = "gray85", col = "gray91"), under=T) +
  latticeExtra::layer(panel.points(1235000,1890000, pch=21, cex=0.5, fill="black", col = "black"))+
  latticeExtra::layer(panel.points(1275000,1865000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.points(1075000,1700000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.text(1203000,1900000, "Whyalla", cex = 1.2))+
  latticeExtra::layer(panel.text(1298000,1863000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1298000,1843000,  "Pirie", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1700000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1680000, "Lincoln", cex=1.2))
  

Non.add <- levelplot(CI_non_additive, margin=FALSE, maxpixels=1e6, 
                     par.settings=myTheme, at = my.at,
                     colorkey=list(at=seq(CI_min, CI_max, length=100), 
                                   height = 0.3, labels = my.labels),
                         scales=list(draw=FALSE)) + 
  latticeExtra::layer(sp.polygons(SA_trns, fill = "gray85", col = "gray91"), under=T) +
  latticeExtra::layer(panel.points(1235000,1890000, pch=21, cex=0.5, fill="black", col = "black"))+
  latticeExtra::layer(panel.points(1275000,1865000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.points(1075000,1700000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.text(1203000,1900000, "Whyalla", cex = 1.2))+
  latticeExtra::layer(panel.text(1298000,1863000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1298000,1843000,  "Pirie", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1700000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1680000, "Lincoln", cex=1.2))



## Plotting the differences between the additive and non-additive maps

Dif <- overlay(CI_non_additive, CI_additive, fun=function(x,y) x-y)

# Define the colour scale and breaks
max_abs <- cellStats(Dif, "max")
my.at.best <- do.breaks(c(-max_abs, max_abs), 8)
myColorkey <- list(at=my.at.best, height = 0.5, labels=list(c("4", "2", "0", "-2", "-4"), cex = 2))
myPal <- rev(brewer.pal(10, "RdBu"))
myTheme_dif <- rasterTheme(region=myPal) 

Dif_plot <- levelplot(Dif, margin=FALSE, maxpixels=1e6, 
                      par.settings=myTheme_dif, at = my.at.best,
                      colorkey = myColorkey,
                      scales=list(draw=FALSE)) +
  latticeExtra::layer(sp.polygons(SA_trns, fill = "gray85", col = "gray91"), under=T) +
  latticeExtra::layer(panel.points(1235000,1890000, pch=21, cex=0.5, fill="black", col = "black"))+
  latticeExtra::layer(panel.points(1275000,1865000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.points(1075000,1700000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.text(1203000,1900000, "Whyalla", cex = 1.2))+
  latticeExtra::layer(panel.text(1298000,1863000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1298000,1843000,  "Pirie", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1700000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1680000, "Lincoln", cex=1.2))


# Plot mitigating in one colour and exacerbating in another
Dif_bin <- Dif
Dif_bin[Dif_bin > 0] <- 1
Dif_bin[Dif_bin == 0] <- NA
Dif_bin[Dif_bin < 0] <- 0

# Proportion of synergies vs antagonisms
# 112329 total number of seagrass cells
(freq(Dif_bin, value = 1))/112329 # 0.7711366
(freq(Dif_bin, value = 0))/112329 # 0.2288634

(sum(Dif_bin > 0))/2277432
(Dif_bin[Dif_bin < 0])/2277432
(Dif_bin[Dif_bin == 0])/2277432

Dif_r <- ratify(Dif_bin)
Dif_lev <- levels(Dif_r)[[1]]
Dif_lev$code <- c(0, 1)
levels(Dif_r) <- Dif_lev

cols_bin <- colorRampPalette(rev(c("yellow2", "turquoise3")))
myTheme_bin <- rasterTheme(region = cols_bin(2))

Dif_bin_plot <- levelplot(Dif_r, 
                          par.settings=myTheme_bin, maxpixels=1e6,
                          colorkey=list(height = 0.35, 
                          labels = list(labels=c("Antagonistic", "Synergistic"), 
                                        cex = 1.8)),
                          scales=list(draw=FALSE)) + 
  latticeExtra::layer(sp.polygons(SA_trns, fill = "gray85", col = "gray91"), under=T) +
  latticeExtra::layer(panel.points(1235000,1890000, pch=21, cex=0.5, fill="black", col = "black"))+
  latticeExtra::layer(panel.points(1275000,1865000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.points(1075000,1700000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.text(1203000,1900000, "Whyalla", cex = 1.2))+
  latticeExtra::layer(panel.text(1298000,1863000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1298000,1843000,  "Pirie", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1700000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1680000, "Lincoln", cex=1.2))

### Calculate variance 

var_add <- raster("variance_main")
var_non.add <- raster("variance_raster")

variance_add <- var_add/var_non.add
variance_non.add <- 1-(var_add/var_non.add)

myTheme_var <- viridisTheme(region = rev(magma(5)))

variance_add <- levelplot(variance_add, margin=FALSE, maxpixels=1e6, 
                          par.settings=myTheme_var, 
                          scales=list(draw=FALSE)) +
  latticeExtra::layer(sp.polygons(SA_trns, fill = "gray85", col = "gray91"), under=T) +
  latticeExtra::layer(panel.points(1235000,1890000, pch=21, cex=0.5, fill="black", col = "black"))+
  latticeExtra::layer(panel.points(1275000,1865000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.points(1075000,1700000, pch=21, cex=0.5, fill="black", col="black"))+
  latticeExtra::layer(panel.text(1203000,1900000, "Whyalla", cex = 1.2))+
  latticeExtra::layer(panel.text(1298000,1863000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1298000,1843000,  "Pirie", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1700000, "Port", cex=1.2))+
  latticeExtra::layer(panel.text(1050000,1680000, "Lincoln", cex=1.2))


variance_non.add_plot <- levelplot(variance_non.add, margin=FALSE, maxpixels=1e6, 
                              par.settings=myTheme_var, 
                              scales=list(draw=FALSE)) +
  latticeExtra::layer(sp.polygons(SA_trns, fill = "gray85", col = "gray91"), under=T) +
  latticeExtra::layer(panel.points(1235000,1890000, pch=21, cex=1.2, fill="black", col = "black"))+
  latticeExtra::layer(panel.points(1275000,1865000, pch=21, cex=1.2, fill="black", col="black"))+
  latticeExtra::layer(panel.points(1075000,1700000, pch=21, cex=1.2, fill="black", col="black"))+
  latticeExtra::layer(panel.text(1203000,1900000, "Whyalla", cex = 1.5))+
  latticeExtra::layer(panel.text(1298000,1863000, "Port", cex=1.5))+
  latticeExtra::layer(panel.text(1298000,1843000,  "Pirie", cex=1.5))+
  latticeExtra::layer(panel.text(1050000,1700000, "Port", cex=1.5))+
  latticeExtra::layer(panel.text(1050000,1683000, "Lincoln", cex=1.5))



