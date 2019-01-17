################################################################################
##                                                                            ##
##    Hesselbarth et al. (2018):  Density-dependent spatial patterning of     ## 
##                                woody plants differs  between a semi-arid   ## 
##                                and a mesic savanna in South Africa.        ## 
##    Journal of Arid Environments (157), 103–112.                            ##
##                                                                            ##
##                                                                            ##
##    Maximilian Hesselbarth (Author R-Script)                                ##
##    Department of Ecosystem Modelling                                       ##
##    University of Goettingen                                                ##
##    maximilian.hesselbarth@uni-goettingen.de                                ##
##    https://mhesselbarth.rbind.io/                                          ##
##                                                                            ##
################################################################################

## Figure spatial point patterns of plots ##
##----------------------------------------##

#### Import libraries and set working directory #### 
library(ggplot2)
library(maptools)
library(patchwork)
library(sp)
library(spatstat)
library(tidyverse)

# Results are saved using an own written functions one can install with the 
# following comand [requires library(devtools)]. The function do not overwrite 
# already existing files. Of course also the built-in saving functions can be used

# devtools::install_github(repo="mhesselbarth/UtilityFunctions")

# Directory for raw data and results
raw_data <- paste0(getwd(), "/Raw_data")
results <- paste0(getwd(), "/Results")
figures <- paste0(getwd(), "/Figures")


#### Import data ####

# Semi-arid savanna #
# Open plot  
semi_arid_open <- raw_data %>%
  paste0('/Semi_arid_open.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species),
                Plot="Open plot")

semi_arid_open_plot <- raw_data %>% 
  paste0("/Semi_arid_open.shp") %>%
  rgdal::readOGR() %>%
  fortify() %>%
  dplyr::select(long, lat) %>% 
  tibble::as.tibble() %>%
  setNames(c("x", "y")) 

# Dense plot
semi_arid_dense <- raw_data %>%
  paste0('/Semi_arid_dense.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species), 
                Plot="Dense plot")

semi_arid_dense_plot <- raw_data %>% 
  paste0("/Semi_arid_dense.shp") %>%
  rgdal::readOGR() %>%
  fortify() %>%
  dplyr::select(long, lat) %>% 
  tibble::as.tibble() %>%
  setNames(c("x", "y")) 

# Combine data
semi_arid <- dplyr::bind_rows(semi_arid_open, semi_arid_dense) %>%
  dplyr::mutate(Species=factor(Species),
                Plot=factor(Plot),
                HeightGroup=factor(dplyr::if_else(Height<=quantile(Height, probs=1/3), 
                                                  'small', dplyr::if_else(Height<=quantile(Height, probs=2/3),
                                                                          'medium', 'large')),
                                   levels=c("small", "medium", "large")))


# Mesic savanna #
# Open plot  
mesic_open <- raw_data %>%
  paste0('/Mesic_open.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species),
                Plot="Open plot")

mesic_open_plot <- raw_data %>% 
  paste0("/Mesic_open.shp") %>%
  rgdal::readOGR() %>%
  fortify() %>%
  dplyr::select(long, lat) %>% 
  tibble::as.tibble() %>%
  setNames(c("x", "y")) 


# Dense plot
mesic_dense <- raw_data %>%
  paste0('/Mesic_dense.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species), 
                Plot="Dense plot")

mesic_dense_plot <- raw_data %>% 
  paste0("/Mesic_dense.shp") %>%
  rgdal::readOGR() %>%
  fortify() %>%
  dplyr::select(long, lat) %>% 
  tibble::as.tibble() %>%
  setNames(c("x", "y")) 

# Combine data
mesic <- dplyr::bind_rows(mesic_open, mesic_dense) %>%
  dplyr::mutate(Species=factor(Species),
                Plot=factor(Plot),
                HeightGroup=factor(dplyr::if_else(Height<=quantile(Height, probs=1/3), 
                                                  'small', dplyr::if_else(Height<=quantile(Height, probs=2/3),
                                                                          'medium', 'large')),
                                   levels=c("small", "medium", "large")))


#### Plot ####

margins <- c(0,0,0,0)

# Semi-arid savanna #
semi_arid_open_ggplot <- ggplot(data=dplyr::filter(semi_arid, Plot=="Open plot")) + 
  geom_point(aes(x=x, y=y, shape=HeightGroup)) + 
  geom_polygon(data=semi_arid_open_plot, aes(x=x, y=y), fill=NA, col="black") +
  geom_segment(aes(x=max(semi_arid_open_plot$x)-20, xend=max(semi_arid_open_plot$x),
               y=min(semi_arid_open_plot$y), yend=min(semi_arid_open_plot$y)), 
               size=2.5) +
  geom_text(aes(x=max(semi_arid_open_plot$x)-10, y=min(semi_arid_open_plot$y)+6.5),
            label="20 m") +  
  geom_text(aes(x=min(semi_arid_open_plot$x)+5, y=max(semi_arid_open_plot$y)),
            label="(A)") +
  scale_shape_manual(name="Height group", values=c(1,0,2)) +
  coord_equal() +
  labs(x="", y="y-coordinate") +
  theme_classic(base_size = 12) +
  theme(axis.text=element_blank(), plot.margin=unit(margins, "cm")) +
  guides(shape=F)

semi_arid_dense_ggplot <- ggplot(data=dplyr::filter(semi_arid, Plot=="Dense plot")) + 
  geom_point(aes(x=x, y=y, shape=HeightGroup)) + 
  geom_polygon(data=semi_arid_dense_plot, aes(x=x, y=y), fill=NA, col="black") +
  geom_segment(aes(x=max(semi_arid_dense_plot$x)-20, xend=max(semi_arid_dense_plot$x),
                   y=min(semi_arid_dense_plot$y), yend=min(semi_arid_dense_plot$y)), 
               size=2.5) +
  geom_text(aes(x=max(semi_arid_dense_plot$x)-10, y=min(semi_arid_dense_plot$y)+6.5),
            label="20 m") + 
  geom_text(aes(x=min(semi_arid_dense_plot$x)+5, y=max(semi_arid_dense_plot$y)),
            label="(C)") +
  scale_shape_manual(name="Height group", values=c(1,0,2)) +
  coord_equal() +
  labs(x="x-coordinate", y="y-coordinate") +
  theme_classic(base_size = 12) +
  theme(axis.text=element_blank(), plot.margin=unit(margins, "cm")) +
  guides(shape=F)

# Mesic savanna 
mesic_open_ggplot <- ggplot(data=dplyr::filter(mesic, Plot=="Open plot")) + 
  geom_point(aes(x=x, y=y, shape=HeightGroup)) + 
  geom_polygon(data=mesic_open_plot, aes(x=x, y=y), fill=NA, col="black") +
  geom_segment(aes(x=max(mesic_open_plot$x)-10, xend=max(mesic_open_plot$x),
                   y=min(mesic_open_plot$y), yend=min(mesic_open_plot$y)), 
               size=2.5) +
  geom_text(aes(x=max(mesic_open_plot$x)-5, y=min(mesic_open_plot$y)+3),
            label="10 m") + 
  geom_text(aes(x=min(mesic_open_plot$x)+2.5, y=max(mesic_open_plot$y)),
            label="(B)") +
  scale_shape_manual(name="Height group", values=c(1,0,2)) +
  coord_equal() +
  labs(x="", y="") +
  theme_classic(base_size = 12) +
  theme(axis.text=element_blank(), plot.margin=unit(margins, "cm")) +
  guides(shape=F)

mesic_dense_ggplot <- ggplot(data=dplyr::filter(mesic, Plot=="Dense plot")) + 
  geom_point(aes(x=x, y=y, shape=HeightGroup)) + 
  geom_polygon(data=mesic_dense_plot, aes(x=x, y=y), fill=NA, col="black") +
  geom_segment(aes(x=max(mesic_dense_plot$x)-10, xend=max(mesic_dense_plot$x),
                   y=min(mesic_dense_plot$y), yend=min(mesic_dense_plot$y)), 
               size=2.5) +
  geom_text(aes(x=max(mesic_dense_plot$x)-5, y=min(mesic_dense_plot$y)+2),
            label="10 m") + 
  geom_text(aes(x=min(mesic_dense_plot$x)+2.5, y=max(mesic_dense_plot$y)),
            label="(D)") +
  scale_shape_manual(name="Height group", values=c(1,0,2)) +
  coord_equal() +
  labs(x="x-coordinate", y="") +
  theme_classic(base_size = 12) +
  theme(axis.text=element_blank(), plot.margin=unit(margins, "cm")) + 
  guides(shape=F)

plot_overall <- semi_arid_open_ggplot + mesic_open_ggplot + 
                semi_arid_dense_ggplot + mesic_dense_ggplot
  
# UtilityFunctions::Save.Function.ggplot(plot=plot_overall, 
#                                        path=figures,
#                                        filename="FIG2.tiff",
#                                        dpi=500, width=190, height=145, unit="mm")


