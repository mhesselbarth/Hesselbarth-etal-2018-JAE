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

## Analyses of shift from clustered to regular with increasing tree height ##
##-------------------------------------------------------------------------##

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

#### Analysing data using pcf-function and complete spatial randomness as null model ####

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
# open plot #
semi_arid_open_csr_small <- envelope(spatstat::subset.ppp(semi_arid_open_ppp, HeightGroup=="small", drop=T), 
                                     pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                     global=F, correction=crrctn, r=r_semi_arid, savefuns=T)
# saveRDS(object=semi_arid_open_csr_small,
#                   file=paste0(results, "/semi_arid_open_csr_small.rds"))

semi_arid_open_csr_medium <- envelope(spatstat::subset.ppp(semi_arid_open_ppp, HeightGroup=="medium", drop=T), 
                                     pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                     global=F, correction=crrctn, r=r_semi_arid, savefuns=T)
# saveRDS(object=semi_arid_open_csr_medium,
#                   file=paste0(results, "/semi_arid_open_csr_medium.rds"))

semi_arid_open_csr_large <- envelope(spatstat::subset.ppp(semi_arid_open_ppp, HeightGroup=="large", drop=T), 
                                     pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                     global=F, correction=crrctn, r=r_semi_arid, savefuns=T)
# saveRDS(object=semi_arid_open_csr_large,
#                   file=paste0(results, "/semi_arid_open_csr_large.rds"))

# Dense plot #
semi_arid_dense_csr_small <- envelope(spatstat::subset.ppp(semi_arid_dense_ppp, HeightGroup=="small", drop=T), 
                                     pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                     global=F, correction=crrctn, r=r_semi_arid, savefuns=T)
# saveRDS(object=semi_arid_dense_csr_small,
#                   file=paste0(results, "/semi_arid_dense_csr_small.rds"))

semi_arid_dense_csr_medium <- envelope(spatstat::subset.ppp(semi_arid_dense_ppp, HeightGroup=="medium", drop=T), 
                                      pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                      global=F, correction=crrctn, r=r_semi_arid, savefuns=T)
# saveRDS(object=semi_arid_dense_csr_medium,
#                   file=paste0(results, "/semi_arid_dense_csr_medium.rds"))

semi_arid_dense_csr_large <- envelope(spatstat::subset.ppp(semi_arid_dense_ppp, HeightGroup=="large", drop=T), 
                                     pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                     global=F, correction=crrctn, r=r_semi_arid, savefuns=T)
# saveRDS(object=semi_arid_dense_csr_large,
#                   file=paste0(results, "/semi_arid_dense_csr_large.rds"))


# Mesic savanna #
# open plot #
mesic_open_csr_small <- envelope(spatstat::subset.ppp(mesic_open_ppp, HeightGroup=="small", drop=T), 
                                     pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                     global=F, correction=crrctn, r=r_mesic, savefuns=T)
# saveRDS(object=mesic_open_csr_small,
#                   file=paste0(results, "/mesic_open_csr_small.rds"))

mesic_open_csr_medium <- envelope(spatstat::subset.ppp(mesic_open_ppp, HeightGroup=="medium", drop=T), 
                                      pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                      global=F, correction=crrctn, r=r_mesic, savefuns=T)
# saveRDS(object=mesic_open_csr_medium,
#                   file=paste0(results, "/mesic_open_csr_medium.rds"))

mesic_open_csr_large <- envelope(spatstat::subset.ppp(mesic_open_ppp, HeightGroup=="large", drop=T), 
                                     pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                     global=F, correction=crrctn, r=r_mesic, savefuns=T)
# saveRDS(object=mesic_open_csr_large,
#                   file=paste0(results, "/mesic_open_csr_large.rds"))

# Dense plot #
mesic_dense_csr_small <- envelope(spatstat::subset.ppp(mesic_dense_ppp, HeightGroup=="small", drop=T), 
                                      pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                      global=F, correction=crrctn, r=r_mesic, savefuns=T)
# saveRDS(object=mesic_dense_csr_small,
#                   file=paste0(results, "/mesic_dense_csr_small.rds"))

mesic_dense_csr_medium <- envelope(spatstat::subset.ppp(mesic_dense_ppp, HeightGroup=="medium", drop=T), 
                                       pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                       global=F, correction=crrctn, r=r_mesic, savefuns=T)
# saveRDS(object=mesic_dense_csr_medium,
#                   file=paste0(results, "/mesic_dense_csr_medium.rds"))

mesic_dense_csr_large <- envelope(spatstat::subset.ppp(mesic_dense_ppp, HeightGroup=="large", drop=T), 
                                      pcf, stoyan=styn, nsim=n_sim, divisor=dvsr, nrank=(p*(n_sim+1)/2),
                                      global=F, correction=crrctn, r=r_mesic, savefuns=T)
# saveRDS(object=mesic_dense_csr_large,
#                   file=paste0(results, "/mesic_dense_csr_large.rds"))
