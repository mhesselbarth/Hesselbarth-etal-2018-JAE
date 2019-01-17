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

## Analyses of associations between small and large trees ##
##--------------------------------------------------------##

#### Import libraries and set working directory #### 
library(maptools)
library(tidyverse)
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

#### Function for null model of antecedent conditions ####
antecedent_conditions <- function(pattern, which_mark, p, n_sim, r){
  # Name of heigh group mark
  pattern <- spatstat::subset.ppp(pattern, select=which_mark)
  # Empty list for simulated patterns
  list_pattern <- vector("list", n_sim)
  
  # Loop of number of simulations
  for(i in 1:n_sim){
    # Distribute "small" trees randomly within plot
    random <- spatstat::runifpoint(n=pattern[pattern$marks=="small"]$n, win=pattern$window)
    # Superimpose unchanged large trees
    superimposed_pattern <- spatstat::superimpose(small=random, 
                                                  large=unmark(pattern[pattern$marks=="large"]))
    # Save overall pattern in list
    list_pattern[i] <-list(superimposed_pattern)
  }
  
  # Using list with simulated patterns as null model for pcf-function
  result_pcf <- envelope(pattern, fun=pcfcross, i="large", j="small",
                          simulate=list_pattern, nsim=n_sim,  nrank=(p*(n_sim+1)/2),
                          correction="Ripley", divisor="d", stoyan=0.3, r=r, savefuns=T)
  # Return result of function
  return(result_pcf)
}


#### Analysing data using pcf-function and antecendet conditions as null model ####

# Set parameters #
# Seed for reproducibility
set.seed(42)
# p-value
p <- 0.05 
# Number of simulation
n_sim <- 1999 
# rmax in the semi-arid savanna
r_semi_arid <- seq(f=0,t=25,l=515) 
# rmax in the mesic savanna
r_mesic <- seq(f=0,t=10,l=515) 

# Semi-arid savanna #
semi_arid_open_antecedent_conditions <- antecedent_conditions(pattern=semi_arid_open_ppp, which_mark="HeightGroup",
                                            p=p, n_sim=n_sim, r=r_semi_arid)
# saveRDS(object=semi_arid_open_antecedent_conditions, 
#                   file=paste0(results, "/semi_arid_open_antecedent_conditions.rds"))

semi_arid_dense_antecedent_conditions <- antecedent_conditions(pattern=semi_arid_dense_ppp, which_mark="HeightGroup",
                                            p=p, n_sim=n_sim, r=r_semi_arid)
# saveRDS(object=semi_arid_dense_antecedent_conditions, 
#                   file=paste0(results, "/semi_arid_dense_antecedent_conditions.rds"))

# Mesic savanna # 
mesic_open_antecedent_conditions <- antecedent_conditions(pattern=mesic_open_ppp, which_mark="HeightGroup",
                                            p=p, n_sim=n_sim, r=r_mesic)
# saveRDS(object=mesic_open_antecedent_conditions, 
#                   file=paste0(results, "/mesic_open_antecedent_conditions.rds"))

mesic_dense_antecedent_conditions <- antecedent_conditions(pattern=mesic_dense_ppp, which_mark="HeightGroup",
                                             p=p, n_sim=n_sim, r=r_mesic)
# saveRDS(object=mesic_dense_antecedent_conditions, 
#                   file=paste0(results, "/mesic_dense_antecedent_conditions.rds"))
