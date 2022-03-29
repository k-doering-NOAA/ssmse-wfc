# Load packages set options ----

library(SSMSE)
library(r4ss)
library(dplyr)
library(ggplot)
library(nmfspalette)
# functions for convergence and performance metrics, get from other gh repo
source("https://raw.githubusercontent.com/k-doering-NOAA/ssmse-afs/master/code/get_metrics.R")
# path names ----


# read in summaries ----

summary <- vector(mode = "list", length = 2)
summary[[1]] <- read.csv("run_SSMSE-ex/results/SSMSE_ts.csv")
summary[[2]] <- read.csv("run_SSMSE-ex/results/SSMSE_scalar.csv")
names(summary) <- c("ts", "scalar")

summary$ts$modtype <- ifelse(grepl("OM", summary$ts$model_run), "OM", summary$ts$model_run)
summary$ts$scenario_format <- factor(summary$ts$scenario, levels = c("h-1", "h-ctl"), 
                                     labels = c("Scenario: Parameter mismatched", "Scenario: Parameter matched")
                                     )
summary$ts$SpawnBio_bil <- summary$ts$SpawnBio/ (10^9)
spaget_subset <- subset(summary$ts, 
                        model_run %in% c("cod_OM", "cod_SR_BH_steep_1_OM", "cod_EM_145"))
spaget_subset$modtype_fac <- factor(spaget_subset$modtype, 
                                    levels = c("OM", "cod_EM_145"), 
                                    labels = c("OM", "EM_145"))
# get values in billions of fish
# plot SSB by year and model run ----
ggplot2::ggplot(data = spaget_subset, ggplot2::aes(x = year, y = SpawnBio_bil)) +
  ggplot2::geom_vline(xintercept = 100, color = "gray") +
  ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = modtype_fac))+
  ggplot2::scale_color_manual(values = nmfspalette::nmfs_palette(palette = "oceans")(2)) +
  ggplot2::scale_linetype_manual(values = rep("solid", 50)) +
  ggplot2::guides(linetype = "none") +
  ggplot2::facet_wrap(. ~ scenario_format, ncol = 1) +
  ggplot2::labs(color = "Model Type")+
  ggplot2::xlab("Year") +
  ggplot2::ylab("SSB (billions of fish)") +
  ggplot2::ylim(0, 4.5)+
  ggplot2::theme_classic(base_size = 16)
ggsave(file.path("run_SSMSE-ex", "results", "SSB_spaghetti_plot.png"), width = 8, height = 8, units = "in")
#would be nice to refine this plot more to include in the paper; showing more than
# 5 iterations would probably be chaotic