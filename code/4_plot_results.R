# Look at results from all runs (50 each iter) ----

# Load packages set options ----

library(SSMSE)
library(r4ss)
library(dplyr)
library(ggplot2)
# functions for convergence and performance metrics, get from other gh repo
source("https://raw.githubusercontent.com/k-doering-NOAA/ssmse-afs/master/code/get_metrics.R")
# path names ----
mods_path <- "input_models"

# read in summaries ----

summary <- vector(mode = "list", length = 2)
summary[[1]] <- read.csv("model_runs/SSMSE_ts.csv")
summary[[2]] <- read.csv("model_runs/SSMSE_scalar.csv")
names(summary) <- c("ts", "scalar")


# define the scenarios ----
scen_red_tide <- c("no-red-tide", "low-red-tide", "hi-red-tide")
scen_HCR <- c("F-spr-30", "F-spr-45")

scenarios <- data.frame(
  scen_name = c(paste0(scen_red_tide, "-",scen_HCR[1]),
                paste0(scen_red_tide, "-", scen_HCR[2])),
  EM_path = rep(c(file.path(mods_path, scen_HCR)), times = c(3,3))
)

## check convergence ----

SSB_df <- check_convergence(summary, n_EMs = 6, max_yr = 150)
# no params on bounds, there are some relatively low or high SSB's, but they
# werent so far off that I thought the runs should be excluded


# calculate performance metrics ----
# look at catch in OM from yrs 125:150
OM_metrics <- NULL
for (i in scenarios$scen_name) { # scenarios$scen_name to make general
  
  iterations <- list.dirs(file.path("model_runs", i), recursive = FALSE, full.names = FALSE)
  OM_name <- grep("_OM$",
                  list.dirs(file.path("model_runs", i, iterations[1]), full.names = FALSE),
                  value = TRUE)
  OM_dat <- file.path("model_runs", i, iterations, OM_name, "ss3.dat")
  avg_catch <- unlist(lapply(OM_dat, function(x) get_avg_catch(x, yrs = 126:150)))
  catch_sd <- unlist(lapply(OM_dat, function(x) get_catch_sd(x, yrs = 126:150)))
  tmp_df <- data.frame(iteration = as.integer(iterations), scenario = i,
                       avg_catch = avg_catch, catch_sd = catch_sd)
  OM_metrics <- rbind(OM_metrics, tmp_df)
}
SSB_avg <- get_SSB_avg(summary, min_yr = 126, max_yr = 150)

all_metrics <- full_join(OM_metrics, SSB_avg)
all_metrics_long <- tidyr::gather(all_metrics, "metric", "value", 3:5)
all_metrics_long$value_bils <- all_metrics_long$value/1000000000
all_metrics_long$scen_fac <- factor(all_metrics_long$scenario,
                                    levels = c("no-red-tide-F-spr-30", "low-red-tide-F-spr-30", "hi-red-tide-F-spr-30",
                                               "no-red-tide-F-spr-45", "low-red-tide-F-spr-45", "hi-red-tide-F-spr-45" ),
                                    labels = c("no", "low", "high", "no", "low", "high"))

all_metrics_long <- all_metrics_long %>%
  tidyr::separate(col = scenario,
                  into = c("OM_scen", "MP"),
                  sep = "-F-",
                  remove = FALSE)

metrics <- unique(all_metrics_long$metric)

plots <- lapply(metrics, function(i, all_metrics_long) {
  title_lab <- switch(i,
                      avg_catch = "Long-term average catch",
                      avg_SSB = "Long-term average SSB",
                      catch_sd = "Long-term catch variability")
  yaxis_lab <- switch(i,
                      avg_catch = "Catch (billion metric tons)",
                      avg_SSB = "Biomass (billion metric tons)",
                      catch_sd = "Catch (billion metric tons)")
  plot <- ggplot(data = all_metrics_long[all_metrics_long$metric == i, ],
                 aes(x = scen_fac, y = value_bils)) 
  if(i == "avg_SSB") {
    plot <- plot + geom_hline(yintercept = 1342470000/1000000000)
  }
  plot <- plot +
    geom_violin(draw_quantiles = 0.5, aes(fill = MP)) +
    scale_y_continuous(limits = c(0, NA))+
    scale_fill_brewer(palette = "Set2", direction = -1)+
    labs(title = title_lab, x = "OM M pulses", y = yaxis_lab) +
    theme_classic(base_size = 22)
  plot
}, all_metrics_long = all_metrics_long)

for (i in seq_len(length(plots))) {
  ggsave(file.path("figures", paste0("run_red_tide_scens_", metrics[i], ".png")),
         plot = plots[[i]], width = 8, height = 6, units = "in", device = "png")
}


# get cv catch ----

catch_cv_df <- NULL
for (i in scenarios$scen_name) { # scenarios$scen_name to make general
  
  iterations <- list.dirs(file.path("model_runs", i), recursive = FALSE, full.names = FALSE)
  OM_name <- grep("_OM$",
                  list.dirs(file.path("model_runs", i, iterations[1]), full.names = FALSE),
                  value = TRUE)
  OM_dat <- file.path("model_runs", i, iterations, OM_name, "ss3.dat")
  catch_cv <- unlist(lapply(OM_dat, function(x) get_catch_cv(x, yrs = 126:150)))
  tmp_df <- data.frame(iteration = as.integer(iterations), scenario = i,
                       catch_cv = catch_cv)
  catch_cv_df <- rbind(catch_cv_df, tmp_df)
}
catch_cv_df$scen_fac <- factor(catch_cv_df$scenario,
                               levels = c("no-red-tide-F-spr-30", "low-red-tide-F-spr-30", "hi-red-tide-F-spr-30",
                                          "no-red-tide-F-spr-45", "low-red-tide-F-spr-45", "hi-red-tide-F-spr-45"),
                               labels = c("no", "low", "high", "no", "low", "high"))
catch_cv_df <- catch_cv_df %>%
  tidyr::separate(col = scenario,
                  into = c("OM_scen", "MP"),
                  sep = "-F-",
                  remove = FALSE)

plot_cv <- ggplot(data = catch_cv_df, aes(x = scen_fac, y = catch_cv)) +
  geom_violin(draw_quantiles = 0.5, aes(fill = MP)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_brewer(palette = "Set2", direction = -1)+
  labs(title = "Long-term catch variability (years 126-150)",
       x = "OM M pulses", y = "coefficient of variation") +
  theme_classic(base_size = 22)
ggsave(file.path("figures", paste0("run_sel_btarget_scens_", "catch_CV", ".png")),
       width = 8, height = 6, units = "in", device = "png")
