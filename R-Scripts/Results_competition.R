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

## Results of possible competition and facilitation effects ##
##----------------------------------------------------------##

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
semi_arid_open_mark_correlation <- readRDS(paste0(results, "/semi_arid_open_mark_correlation.rds"))
# Diggle-Cressie-Loosmore-Ford and Maximum Absolute Deviation Tests
dclf.test(semi_arid_open_mark_correlation)

semi_arid_dense_mark_correlation <- readRDS(paste0(results, "/semi_arid_dense_mark_correlation.rds"))
dclf.test(semi_arid_dense_mark_correlation)

# Mesic savanna #
mesic_open_mark_correlation <- readRDS(paste0(results, "/mesic_open_mark_correlation.rds"))
dclf.test(mesic_open_mark_correlation)

mesic_dense_mark_correlation <- readRDS(paste0(results, "/mesic_dense_mark_correlation.rds"))
dclf.test(mesic_dense_mark_correlation)

#### Combine dataframes ####
mark_correlation <- dplyr::bind_rows( 
  tibble::as.tibble(c(semi_arid_open_mark_correlation, Savanna="Semi-arid savanna",  Plot="Open plot")),
  tibble::as.tibble(c(semi_arid_dense_mark_correlation, Savanna="Semi-arid savanna", Plot="Dense plot")), 
  tibble::as.tibble(c(mesic_open_mark_correlation, Savanna="Mesic savanna", Plot="Open plot")),
  tibble::as.tibble(c(mesic_dense_mark_correlation, Savanna="Mesic savanna", Plot="Dense plot"))) %>%
  dplyr::mutate(Type=factor(dplyr::if_else(obs<lo, 
                                           "Negative", dplyr::if_else(obs>hi, 
                                                                         "Positive","Independence")), 
                            levels=c("Positive", "Independence", "Negative")),
                Savanna=factor(Savanna, levels=c("Semi-arid savanna", "Mesic savanna")), 
                Plot=factor(Plot, levels=c("Open plot", "Dense plot")))


#### Plot ####
# Semi-arid savanna #
semi_arid_mark_correlation_ggplot <- ggplot(data=dplyr::filter(mark_correlation, Savanna=="Semi-arid savanna")) +
  geom_line(aes(x=r, y=0, color=Type, group=2), size=10) + 
  facet_wrap(~ Plot, scales="free", nrow=2, ncol=1) + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text=element_text(size=rel(0.55))) +
  ylim(0, 0.1) +
  scale_color_viridis(name="", discrete = T) +
  labs(x="r [m]", y="")

# UtilityFunctions::Save.Function.ggplot(plot=semi_arid_mark_correlation_ggplot,
#                                        path=figures,
#                                        filename="FIG4.tiff",
#                                        dpi=500, width=90, height=75, unit="mm")

# Mesic savanna #
mesic_mark_correlation_ggplot <- ggplot(data=dplyr::filter(mark_correlation, Savanna=="Mesic savanna")) +
  geom_line(aes(x=r, y=0, color=Type, group=2), size=10) + 
  facet_wrap(~ Plot, scales="free", nrow=2, ncol=1) + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text=element_text(size=rel(0.55))) +
  ylim(0, 0.1) +
  scale_color_viridis(name="", discrete = T) +
  labs(x="r [m]", y="")

# UtilityFunctions::Save.Function.ggplot(plot=mesic_mark_correlation_ggplot, 
#                                        path=figures,
#                                        filename="FIG6.tiff",
#                                        dpi=500, width=90, height=75, unit="mm")

# Appendix - Both savannas # 
mark_correlation_ggplot_appendix <- ggplot(data=mark_correlation) +
  geom_ribbon(aes(x=r, ymin=lo, ymax=hi), alpha=0.3, col="grey") +
  geom_line(aes(x=r, y=obs)) +
  geom_line(aes(x=r, y=0.75, color=Type, group=2), size=2.5) +
  facet_wrap(Plot ~ Savanna, scales="free", nrow=2, ncol=2) +
  theme_classic(base_size = 15) +
  theme(legend.position="bottom",
        strip.background = element_blank(),
        strip.text = element_blank()) +
  scale_color_viridis(name="", discrete = T) +
  labs(x="r [m]", y=expression(paste(k[mm], "(r)")))

# UtilityFunctions::Save.Function.ggplot(plot=mark_correlation_ggplot_appendix, 
#                                        path=paste0(figures, "/Appendix"),
#                                        filename="FIGA3.tiff",
#                                        dpi=500, width=190, height=145, unit="mm")

