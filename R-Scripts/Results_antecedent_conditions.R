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

## Results of associations between small and large trees ##
##-------------------------------------------------------##

# Import libraries
library(ggplot2)
library(spatstat)
library(tidyverse)
library(viridis)

# Results are saved using an own written functions one can install with the 
# following comand [requires library(devtools)]. The function do not overwrite 
# already existing files. Of course also the built-in saving functions can be used

# devtools::install_github(repo="mhesselbarth/UtilityFunctions")

# Set working directory
# Directory for raw data and results
results <- paste0(getwd(), "/Results")
figures <- paste0(getwd(), "/Figures")

#### Import data ####
# Semi-arid savanna #

# Import data
semi_arid_open_antecedent_conditions <- readRDS(paste0(results, "/semi_arid_open_antecedent_conditions.rds"))
# Diggle-Cressie-Loosmore-Ford and Maximum Absolute Deviation Tests
spatstat::dclf.test(semi_arid_open_antecedent_conditions)

semi_arid_dense_antecedent_conditions <- readRDS(paste0(results, "/semi_arid_dense_antecedent_conditions.rds"))
spatstat::dclf.test(semi_arid_dense_antecedent_conditions)

# Mesic savanna #
mesic_open_antecedent_conditions <- readRDS(paste0(results, "/mesic_open_antecedent_conditions.rds"))
spatstat::dclf.test(mesic_open_antecedent_conditions)

mesic_dense_antecedent_conditions <- readRDS(paste0(results, "/mesic_dense_antecedent_conditions.rds"))
spatstat::dclf.test(mesic_dense_antecedent_conditions)

#### Combine dataframes ####
antecedent_conditions <- dplyr::bind_rows( 
  tibble::as.tibble(c(semi_arid_open_antecedent_conditions, Savanna="Semi-arid savanna",  Plot="Open plot")),
  tibble::as.tibble(c(semi_arid_dense_antecedent_conditions, Savanna="Semi-arid savanna", Plot="Dense plot")), 
  tibble::as.tibble(c(mesic_open_antecedent_conditions, Savanna="Mesic savanna", Plot="Open plot")),
  tibble::as.tibble(c(mesic_dense_antecedent_conditions, Savanna="Mesic savanna", Plot="Dense plot"))) %>%
  dplyr::mutate(Type=factor(dplyr::if_else(obs<lo, 
                                           "Segregation", dplyr::if_else(obs>hi, 
                                                                         "Association","Randomness")), 
                            levels=c("Association", "Randomness", "Segregation")),
                Savanna=factor(Savanna, levels=c("Semi-arid savanna", "Mesic savanna")), 
                Plot=factor(Plot, levels=c("Open plot", "Dense plot")))


#### Plot ####
antecedent_conditions_ggplot <- ggplot(data=antecedent_conditions) +
  geom_line(aes(x=r, y=0, color=Type, group=2), size=10) + 
  facet_wrap(Plot ~ Savanna, scales="free") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylim(0, 0.1) +
  scale_color_viridis(name="", discrete=T) +
  labs(x="r [m]", y="")

# UtilityFunctions::Save.Function.ggplot(plot=antecedent_conditions_ggplot,
#                                        path=figures,
#                                        filename="FIG7.tiff",
#                                        dpi=500, width=140, height=65, unit="mm")

antecedent_conditions_ggplot_appendix <- ggplot(data=antecedent_conditions) +
  geom_ribbon(aes(x=r, ymin=lo, ymax=hi), alpha=0.3, col="grey") + 
  geom_line(aes(x=r, y=obs), size=0.75) + 
  geom_line(aes(x=r, y=0, color=Type, group=2), size=2.5) + 
  facet_wrap(Plot ~ Savanna, scales="free") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank()) +
  scale_color_viridis(name="", discrete=T) +
  labs(x="r [m]", y=expression(g[12](r)))

# UtilityFunctions::Save.Function.ggplot(plot=antecedent_conditions_ggplot_appendix, 
#                                        path=paste0(figures, "/Appendix"),
#                                        filename="FIGA4.tiff",
#                                        dpi=500, width=190, height=145, unit="mm")
