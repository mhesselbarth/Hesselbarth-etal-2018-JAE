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

## Analyses of possible competition and facilitation effects ##
##-----------------------------------------------------------##

#### Import libraries and set working directory #### 
library(tidyverse)
library(maptools)
library(spatstat)

# Results are saved using an own written functions one can install with the 
# following comand [requires library(devtools)]. The function do not overwrite 
# already existing files. Of course also the built-in saving functions can be used

# devtools::install_github(repo="mhesselbarth/UtilityFunctions")

# Directory for raw data and results
raw_data <- paste0(getwd(), "/Raw_data")
results <- paste0(getwd(), "/Results")

#### Import data ####
# Semi-arid savanna #
# Open plot  
semi_arid_open <- raw_data %>%
  paste0('/Semi_arid_open.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species),
                Plot="Open plot")

# Dense plot
semi_arid_dense <- raw_data %>%
  paste0('/Semi_arid_dense.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species), 
                Plot="Dense plot")

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

# Dense plot
mesic_dense <- raw_data %>%
  paste0('/Mesic_dense.txt') %>%
  readr::read_delim(delim=";", col_names=T) %>%
  dplyr::mutate(Species=factor(Species), 
                Plot="Dense plot")

mesic <- dplyr::bind_rows(mesic_open, mesic_dense) %>%
  dplyr::mutate(Species=factor(Species),
                Plot=factor(Plot),
                HeightGroup=factor(dplyr::if_else(Height<=quantile(Height, probs=1/3), 
                                                  'small', dplyr::if_else(Height<=quantile(Height, probs=2/3),
                                                                          'medium', 'large')),
                                   levels=c("small", "medium", "large")))


#### Convert to spatstat::ppp ####
# Semi-arid savanna #
# Open plot
# Import shape file containing plot area
semi_arid_open_window <- raw_data %>% 
  paste0("/Semi_arid_open.shp") %>%
  rgdal::readOGR() %>%
  as("owin")

# Set units
spatstat::unitname(semi_arid_open_window) <- c("metre", "metres")

# Create spatstat::ppp
semi_arid_open_ppp <- semi_arid %>%
  dplyr::filter(Plot=="Open plot") %>%
  spatstat::ppp(x=.$x, y=.$y, 
                marks=.[,-c(1:3)],
                window=semi_arid_open_window)

# Dense plot
semi_arid_dense_window <- raw_data %>%
  paste0('/Semi_arid_dense.shp') %>%
  rgdal::readOGR() %>%
  as('owin')

unitname(semi_arid_dense_window) <- c("metre", "metres")

semi_arid_dense_ppp <- semi_arid %>%
  dplyr::filter(Plot=="Dense plot") %>%
  spatstat::ppp(x=.$x, y=.$y, 
                marks=.[,-c(1:3)],
                window=semi_arid_dense_window)

# Mesic savanna #
# Open plot
mesic_open_window <- raw_data %>% 
  paste0("/Mesic_open.shp") %>%
  rgdal::readOGR() %>%
  as("owin")

spatstat::unitname(mesic_open_window) <- c("metre", "metres")

mesic_open_ppp <- mesic %>%
  dplyr::filter(Plot=="Open plot") %>%
  spatstat::ppp(x=.$x, y=.$y, 
                marks=.[,-c(1:3)],
                window=mesic_open_window)

# Dense plot
mesic_dense_window <- raw_data %>%
  paste0('/Mesic_dense.shp') %>%
  rgdal::readOGR() %>%
  as('owin')

unitname(mesic_dense_window) <- c("metre", "metres")

mesic_dense_ppp <- mesic %>%
  dplyr::filter(Plot=="Dense plot") %>%
  spatstat::ppp(x=.$x, y=.$y, 
                marks=.[,-c(1:3)],
                window=mesic_dense_window)


#### Anlysis using mark correlation function ####

# Set parameters #
# Seed for reproducibility
set.seed(42) 
# p-value
p <- 0.05
# Number of  simulation
n_sim <- 1999 
# Stoyan's smoothing factor
styn <- 0.3 
# Divisor of pcf=function
dvsr <- "d" 
# Ripley's edge correction 
crrctn <- "Ripley" 
# rmax in semi-arid savanna
r_semi_arid <- seq(f=0,t=25,l=515) 
# rmax in mesic savanna
r_mesic <- seq(f=0,t=10,l=515) 

# Semi-arid savanna #
semi_arid_open_mark_correlation <- envelope(subset.ppp(semi_arid_open_ppp, select=Height, drop=T), 
                                fun=markcorr, nsim=n_sim, nrank=(p*(n_sim+1)/2), r=r_semi_arid,
                                correction=crrctn, normalise=T, savefuns=T, 
                                simulate=expression(rlabel(subset.ppp(semi_arid_open_ppp, select=Height), 
                                                           labels=marks(subset.ppp(semi_arid_open_ppp, select=Height)), 
                                                           permute=T)))
# saveRDS(object=semi_arid_open_mark_correlation,
#                   file=paste0(results, "/semi_arid_open_mark_correlation.rds"))

semi_arid_dense_mark_correlation <- envelope(subset.ppp(semi_arid_dense_ppp, select=Height, drop=T), 
                                fun=markcorr, nsim=n_sim, nrank=(p*(n_sim+1)/2), r=r_semi_arid,
                                correction=crrctn, normalise=T, savefuns=T,
                                simulate=expression(rlabel(subset.ppp(semi_arid_dense_ppp, select=Height), 
                                                           labels=marks(subset.ppp(semi_arid_dense_ppp, select=Height)), 
                                                           permute=T)))
# saveRDS(object=semi_arid_dense_mark_correlation,
#                   file=paste0(results, "/semi_arid_dense_mark_correlation.rds"))

# Mesic savanna #
mesic_open_mark_correlation <- envelope(subset.ppp(mesic_open_ppp, select=Height, drop=T), 
                                fun=markcorr, nsim=n_sim, nrank=(p*(n_sim+1)/2), r=r_mesic,
                                correction=crrctn, normalise=T, savefuns=T,
                                simulate=expression(rlabel(subset.ppp(mesic_open_ppp, select=Height), 
                                                           labels=marks(subset.ppp(mesic_open_ppp, select=Height)), 
                                                           permute=T)))
# saveRDS(object=mesic_open_mark_correlation,
#                   file=paste0(results, "/mesic_open_mark_correlation.rds"))

mesic_dense_mark_correlation <- envelope(subset.ppp(mesic_dense_ppp, select=Height, drop=T), 
                                 fun=markcorr, nsim=n_sim, nrank=(p*(n_sim+1)/2), r=r_mesic,
                                 correction=crrctn, normalise=T, savefuns=T,
                                 simulate=expression(rlabel(subset.ppp(mesic_dense_ppp, select=Height), 
                                                            labels=marks(subset.ppp(mesic_dense_ppp, select=Height)), 
                                                            permute=T)))
# saveRDS(object=mesic_dense_mark_correlation,
#                   file=paste0(results, "/mesic_dense_mark_correlation.rds"))


#### Nearest neighbour competition ####
nn_correlation <- function(pattern, which_mark, neighbours=4){
  # Vectors to save sum of distances and sum of heigths
  distance_sum <- 0 
  mark_sum <- 0
  # Subset of pattern with mark to analyse
  pattern <- subset.ppp(pattern, select=which_mark)
  
  # Loop over n-neareast neighbours
  for(i in 1:neighbours){
    # Sum of distance to n-nearest neighbours
    distance_sum <- distance_sum + spatstat::nndist(pattern, k=i)
    # Sum of marks of n-nearest neighbours
    mark_sum <- mark_sum + pattern$marks[nnwhich(pattern,k=i)]
  }

  # Kendall's tau correlation between sum of distances and sum of marks
  correlation <- cor.test(x=distance_sum, y=mark_sum, method="kendall")
  # Return result
  return(correlation)
}

# Semi-arid savanna # 
semi_arid_open_nn_correlation <- nn_correlation(pattern=semi_arid_open_ppp, which_mark="Height")
# saveRDS(object=semi_arid_open_nn_correlation,
#                   file=paste0(results, "/semi_arid_open_nn_correlation.rds"))

semi_arid_dense_nn_correlation <- nn_correlation(pattern=pattern_semi_arid_dense, which_mark="Height")
# saveRDS(object=semi_arid_open_nn_correlation,
#                   file=paste0(results, "/semi_arid_open_nn_correlation.rds"))

# Mesic savanna # 
mesic_open_nn_correlation <- nn_correlation(pattern=pattern_mesic_open, which_mark="Height")
# saveRDS(object=mesic_open_nn_correlation,
#                   file=paste0(results, "/mesic_open_nn_correlation.rds"))

mesic_dense_nn_correlation <- nn_correlation(pattern=pattern_mesic_dense, which_mark="Height")
# saveRDS(object=mesic_open_nn_correlation,
#                   file=paste0(results, "/mesic_open_nn_correlation.rds"))


