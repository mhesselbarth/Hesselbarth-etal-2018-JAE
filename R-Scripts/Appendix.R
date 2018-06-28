################################################################################
##                                                                            ##
##    Hesselbarth et al. (2018):  Density-dependent spatial patterning of     ## 
##                                woody plants differs  between a semi-arid   ## 
##                                and a mesic savanna in South Africa.        ## 
##    Journal of Arid Environments. X, xxx-xxx                                ##
##                                                                            ##
##                                                                            ##
##    Maximilian Hesselbarth (Author R-Script)                                ##
##    Department of Ecosystem Modelling                                       ##
##    University of Goettingen                                                ##
##    maximilian.hesselbarth@uni-goettingen.de                                ##
##                                                                            ##
################################################################################

## Figures appendix plot characteristics ##
##---------------------------------------##

#### Import libraries and set working directory #### 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(viridis)

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
                Plot=factor("Open plot", levels=c("Open plot", "Dense plot")))

# Dense plot
semi_arid_dense <- raw_data %>%
  paste0('/Semi_arid_dense.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species), 
                Plot=factor("Dense plot", levels=c("Open plot", "Dense plot")))

# Combine data
semi_arid <- dplyr::bind_rows(semi_arid_open, semi_arid_dense) %>%
  dplyr::mutate(Species=factor(Species),
                Savanna=factor("Semi-arid savanna",
                               levels=c("Semi-arid savanna", "Mesic savanna")),
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
                Plot=factor("Open plot", levels=c("Open plot", "Dense plot")))

# Dense plot
mesic_dense <- raw_data %>%
  paste0('/Mesic_dense.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species), 
                Plot=factor("Dense plot", levels=c("Open plot", "Dense plot")))

mesic <- dplyr::bind_rows(mesic_open, mesic_dense) %>%
  dplyr::mutate(Species=factor(Species),
                Savanna=factor("Mesic savanna", 
                               levels=c("Semi-arid savanna", "Mesic savanna")), 
                HeightGroup=factor(dplyr::if_else(Height<=quantile(Height, probs=1/3), 
                                                  'small', dplyr::if_else(Height<=quantile(Height, probs=2/3),
                                                                          'medium', 'large')),
                                   levels=c("small", "medium", "large")))


#### Height classes #### 
height_class <- rbind(semi_arid, mesic) %>%
  tibble::as.tibble() %>%
  dplyr::mutate(Height_class=cut(Height, breaks=seq(0, max(Height)+1, 1))) %>%
  dplyr::select(Savanna, Height_class, Plot) %>%
  dplyr::group_by(Savanna, Plot, Height_class) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(rel=n/sum(n)*100)

height_class_ggplot <- ggplot(data=height_class, aes(x=Height_class, y=rel, fill=Plot)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~Savanna, ncol=2, nrow=1) + 
  labs(y="Relative count [%]", x="Height class", fill="") +
  scale_x_discrete(labels=c("<1 m", "<2 m", "<3 m", "<4 m", 
                            "<5 m", "<6 m", "<7 m", "<8 m", "<9 m")) +
  scale_y_continuous(breaks=seq(0, 100, 10), limits=c(0,100)) +
  scale_fill_viridis(discrete=T) +
  theme_bw(base_size = 12) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.25), legend.position="bottom")

UtilityFunctions::Save.Function.ggplot(plot=height_class_ggplot,
                                       filename="FIGA5.tiff",
                                       path=paste0(figures, "/Appendix"),
                                       dpi=500, width=140, height=90, unit="mm")

#### Species distribution ####
semi_arid_species <- semi_arid %>%
  dplyr::select(Species, Plot, CanopyArea) %>%
  dplyr::group_by(Plot, Species) %>%
  dplyr::summarise(n=n(),
                   Canopy=sum(CanopyArea)) %>%
  dplyr::mutate(Abundance_rel=n/sum(n)*100,
                Canopy_rel= Canopy/sum(Canopy)*100) %>%
  dplyr::select(Species, Plot, Abundance_rel, Canopy_rel) %>%
  tidyr::gather(Measure, Percentage, -Plot, -Species, factor_key=T)

mesic_species <- mesic %>%
  dplyr::select(Species, Plot, CanopyArea) %>%
  dplyr::group_by(Plot, Species) %>%
  dplyr::summarise(n=n(),
                   Canopy=sum(CanopyArea)) %>%
  dplyr::mutate(Abundance_rel=n/sum(n)*100,
                Canopy_rel= Canopy/sum(Canopy)*100) %>%
  dplyr::select(Species, Plot, Abundance_rel, Canopy_rel) %>%
  tidyr::gather(Measure, Percentage, -Plot, -Species, factor_key=T)

semi_arid_species_ggplot <- ggplot(data=semi_arid_species, 
                                 aes(x=factor(Species, levels=sort(levels(Species))),
                                     y=Percentage, 
                                     fill=Measure,
                                     group=Measure)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~Plot, scales="free_x", ncol=1, nrow=2) +
  labs(y="Relative value [%]", x="Species", fill="") +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  scale_fill_viridis(discrete=T, labels=c("Abundance", "Canopy Area")) +
  theme_bw(base_size = 12) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.25, hjust=1), 
        legend.position="bottom")

mesic_species_ggplot <- ggplot(data=mesic_species, 
                             aes(x=factor(Species, levels=sort(levels(Species))),
                                 y=Percentage, 
                                 fill=Measure,
                                 group=Measure)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~Plot, scales="free_x", ncol=1, nrow=2) +
  labs(y="Relative value [%]", x="Species", fill="") +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  scale_fill_viridis(discrete=T, labels=c("Abundance", "Canopy Area")) +
  theme_bw(base_size = 12) + 
  theme(axis.text.x=element_text(angle=90, vjust=0.25, hjust=1), legend.position="bottom")

# UtilityFunctions::Save.Function.ggplot(plot=semi_arid_species_ggplot,
#                                        filename="A2.tiff", 
#                                        path=paste0(figures, "/Appendix"),
#                                        dpi=500, width=190, height=240, unit="mm")
# 
# UtilityFunctions::Save.Function.ggplot(plot=mesic_species_ggplot,
#                                        filename="A3.tiff", 
#                                        path=paste0(figures, "/Appendix"),
#                                        dpi=500, width=190, height=240, unit="mm")
