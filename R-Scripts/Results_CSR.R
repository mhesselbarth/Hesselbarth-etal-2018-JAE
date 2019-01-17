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

## Results of shift from clustered to regular with increasing tree height ##
##------------------------------------------------------------------------##

# Import libraries
library(ggplot2)
library(spatstat)
library(tidyverse)
library(viridis)

# Results are saved using an own written functions one can install with the 
# following comand [requires library(devtools)]. The function do not overwrite 
# already existing files. Of course also the built-in saving functions can be used

# devtools::install_github(repo="mhesselbarth/UtilityFunctions")

# Set seed for reproducibility
set.seed(42)

# Set working directory
# Directory for raw data and results
results <- paste0(getwd(), "/Results")
figures <- paste0(getwd(), "/Figures")


#### Import data ####
# Semi-arid savanna #
# Open plot #

# Import data 
semi_arid_open_csr_small <- readRDS(paste0(results, "/semi_arid_open_csr_small.rds"))
# Diggle-Cressie-Loosmore-Ford and Maximum Absolute Deviation Tests
spatstat::dclf.test(semi_arid_open_csr_small)

semi_arid_open_csr_medium <- readRDS(paste0(results, "/semi_arid_open_csr_medium.rds"))
spatstat::dclf.test(semi_arid_open_csr_medium)

semi_arid_open_csr_large <- readRDS(paste0(results, "/semi_arid_open_csr_large.rds"))
spatstat::dclf.test(semi_arid_open_csr_large)

# Dense plot #
semi_arid_dense_csr_small <- readRDS(paste0(results, "/semi_arid_dense_csr_small.rds"))
spatstat::dclf.test(semi_arid_dense_csr_small)

semi_arid_dense_csr_medium <- readRDS(paste0(results, "/semi_arid_dense_csr_medium.rds"))
spatstat::dclf.test(semi_arid_dense_csr_medium)

semi_arid_dense_csr_large <- readRDS(paste0(results, "/semi_arid_dense_csr_large.rds"))
spatstat::dclf.test(semi_arid_dense_csr_large)

# Mesic savanna #
# Open plot
mesic_open_csr_small <- readRDS(paste0(results, "/mesic_open_csr_small.rds"))
spatstat::dclf.test(mesic_open_csr_small)

mesic_open_csr_medium <- readRDS(paste0(results, "/mesic_open_csr_medium.rds"))
spatstat::dclf.test(mesic_open_csr_medium)

mesic_open_csr_large <- readRDS(paste0(results, "/mesic_open_csr_large.rds"))
spatstat::dclf.test(mesic_open_csr_large)

# Dense plot #
mesic_dense_csr_small <- readRDS(paste0(results, "/mesic_dense_csr_small.rds"))
spatstat::dclf.test(mesic_dense_csr_small)

mesic_dense_csr_medium <- readRDS(paste0(results, "/mesic_dense_csr_medium.rds"))
spatstat::dclf.test(mesic_dense_csr_medium)

mesic_dense_csr_large <- readRDS(paste0(results, "/mesic_dense_csr_large.rds"))
spatstat::dclf.test(mesic_dense_csr_large)


#### Combine dataframes ####
# Semi-arid savanna #

# Combine into one data frame
semi_arid_csr <- rbind(
  tibble::as.tibble(c(semi_arid_open_csr_small, Height_group="Small trees", Plot="Open plot")),
  tibble::as.tibble(c(semi_arid_open_csr_medium, Height_group="Medium trees", Plot="Open plot")),
  tibble::as.tibble(c(semi_arid_open_csr_large, Height_group="Large trees", Plot="Open plot")), 
  tibble::as.tibble(c(semi_arid_dense_csr_small, Height_group="Small trees", Plot="Dense plot")), 
  tibble::as.tibble(c(semi_arid_dense_csr_medium, Height_group="Medium trees", Plot="Dense plot")), 
  tibble::as.tibble(c(semi_arid_dense_csr_large, Height_group="Large trees", Plot="Dense plot"))) %>%
  dplyr::mutate(Type=factor(dplyr::if_else(obs<lo, 
                                           "Regularity", dplyr::if_else(obs>hi, 
                                                                         "Clustering","Randomness")), 
                            levels=c("Clustering", "Randomness", "Regularity")),
                Height_group=factor(Height_group, levels=c("Small trees", "Medium trees", "Large trees")), 
                Plot=factor(Plot, levels=c("Open plot", "Dense plot")))

# Mesic savanna # 
mesic_csr <- rbind(
  tibble::as.tibble(c(mesic_open_csr_small, Height_group="Small trees", Plot="Open plot")),
  tibble::as.tibble(c(mesic_open_csr_medium, Height_group="Medium trees", Plot="Open plot")),
  tibble::as.tibble(c(mesic_open_csr_large, Height_group="Large trees", Plot="Open plot")), 
  tibble::as.tibble(c(mesic_dense_csr_small, Height_group="Small trees", Plot="Dense plot")), 
  tibble::as.tibble(c(mesic_dense_csr_medium, Height_group="Medium trees", Plot="Dense plot")), 
  tibble::as.tibble(c(mesic_dense_csr_large, Height_group="Large trees", Plot="Dense plot"))) %>%
  dplyr::mutate(Type=factor(dplyr::if_else(obs<lo, 
                                           "Regularity", dplyr::if_else(obs>hi, 
                                                                        "Clustering","Randomness")), 
                            levels=c("Clustering", "Randomness", "Regularity")),
                Height_group=factor(Height_group, levels=c("Small trees", "Medium trees", "Large trees")), 
                Plot=factor(Plot, levels=c("Open plot", "Dense plot")))


#### Plots ####
# Semi-arid savanna #
semi_arid_csr_ggplot <- ggplot(semi_arid_csr) + 
  geom_line(aes(x=r, y=0, color=Type, group=2), size=10) + 
  facet_wrap(Height_group~Plot , nrow=3, ncol=2, scales="free") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylim(0, 0.1) +
  scale_color_viridis(name="", discrete = T) +
  labs(x="r [m]", y="")

# UtilityFunctions::Save.Function.ggplot(plot=semi_arid_csr_ggplot, 
#                                        path=figures,
#                                        filename="FIG3.tiff",
#                                        dpi=500, width=140, height=75, unit="mm")

semi_arid_csr_ggplot_appendix <- ggplot(semi_arid_csr) + 
  geom_ribbon(aes(x=r, ymin=lo, ymax=hi), alpha=0.3, col="grey") + 
  geom_line(aes(x=r, y=obs)) + 
  geom_line(aes(x=r, y=0, color=Type, group=2), size=2.5) + 
  facet_wrap(Height_group ~ Plot, nrow=3, ncol=2, scales="free") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank()) +
  scale_color_viridis(name="", discrete = T) +
  labs(x="r [m]", y="g(r)")

# UtilityFunctions::Save.Function.ggplot(plot=semi_arid_csr_ggplot_appendix, 
#                                        path=paste0(figures, "/Appendix"),
#                                        filename="FIGA1.tiff",
#                                        dpi=500, width=190, height=240, unit="mm")

# Mesic savanna # 
mesic_csr_ggplot <- ggplot(mesic_csr) + 
  geom_line(aes(x=r, y=0, color=Type, group=2), size=10) + 
  facet_wrap(Height_group ~ Plot , nrow=3, ncol=2, scales="free") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  #scale_x_continuous(breaks=seq(0,max(csr_all_mesic$r),2)) +
  ylim(0, 0.1) +
  scale_color_viridis(name="", discrete=T) +
  labs(x="r [m]", y="")

# UtilityFunctions::Save.Function.ggplot(plot=mesic_csr_ggplot, 
#                                        path=figures,
#                                        filename="FIG5.tiff",
#                                        dpi=500, width=140, height=75, unit="mm")

mesic_csr_ggplot_appendix <- ggplot(mesic_csr) + 
  geom_ribbon(aes(x=r, ymin=lo, ymax=hi), alpha=0.3, col="grey") + 
  geom_line(aes(x=r, y=obs)) + 
  geom_line(aes(x=r, y=0, color=Type, group=2), size=2.5) + 
  facet_wrap(Height_group ~ Plot, nrow=3, ncol=2, scales="free") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom", 
        strip.background = element_blank(), 
        strip.text = element_blank()) +
  scale_color_viridis(name="", discrete=T) +
  labs(x="r [m]", y="g(r)")

# UtilityFunctions::Save.Function.ggplot(plot=mesic_csr_ggplot_appendix,
#                                        path=paste0(figures, "/Appendix"),
#                                        filename="FIGA2.tiff",
#                                        dpi=500, width=190, height=240, unit="mm")

