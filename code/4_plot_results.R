# Look at results from all runs (100 each iter) ----

# Load packages set options ----

library(SSMSE)
library(r4ss)
library(dplyr)
library(ggplot2)
library(patchwork)
# functions for convergence and performance metrics, get from other gh repo
source("https://raw.githubusercontent.com/k-doering-NOAA/ssmse-afs/master/code/get_metrics.R")
# path names ----
mods_path <- "input_models"

# read in summaries ----

summary <- vector(mode = "list", length = 2)
summary[[1]] <- read.csv("model_runs/SSMSE_ts.csv")
summary[[2]] <- read.csv("model_runs/SSMSE_scalar.csv")
names(summary) <- c("ts", "scalar")

# read in the output file -----
out <- readRDS("model_runs/run_SSMSE_out_15Dec2021.rda")



# define the scenarios ----
scen_red_tide <- c("no-red-tide", "low-red-tide", "hi-red-tide")
scen_HCR <- c("F-spr-30", "F-spr-45")

scenarios <- data.frame(
  scen_name = c(paste0(scen_red_tide, "-",scen_HCR[1]),
                paste0(scen_red_tide, "-", scen_HCR[2])),
  EM_path = rep(c(file.path(mods_path, scen_HCR)), times = c(3,3))
)

# get the files that had issuse running ----

error_mods <- lapply(out, function(x) {
  tmp <- x$errored_iterations
  if(isTRUE(tmp == "No errored iterations")) {
    tmp <- NULL
  }
  tmp
}
)

error_mods_df <- do.call(bind_rows, error_mods)
error_mods_key <- error_mods_df[,c("iteration", "scenario")]

# remove the models with issues

summary$ts <- dplyr::anti_join(summary$ts, error_mods_key)
summary$scalar <- dplyr::anti_join(summary$scalar, error_mods_key)

## check convergence ----
# also add a check for max_grad
check_scalar <- summary$scalar[,c("max_grad", "iteration", "scenario")]# make sure there aren't any crazy high (say,
too_high_max_grad_key <- na.omit(summary$scalar[summary$scalar$max_grad>2, c("iteration", "scenario")])
# greater than 2 max gradients)
summary$ts <- dplyr::anti_join(summary$ts, too_high_max_grad_key)
summary$scalar <- dplyr::anti_join(summary$scalar, too_high_max_grad_key)


SSB_df <- check_convergence(summary, n_EMs = 6, max_yr = 150)
summary(SSB_df$SSB_ratio)
# There is a bit of a spread, but I'm not exactly sure if 4 x different (or
# slightly less than half) isn't realistic. Will retain these values

SSB_df# no params on bounds, there are some relatively low or high SSB's.

# how many iterations per scenario are left? 

n_iters_per_scen <- summary$scalar[summary$scalar$model_run == "cod_OM", c("iteration", "scenario")] %>% 
  group_by(scenario) %>% 
  summarize(n = n())
write.csv(n_iters_per_scen, "model_runs/n_iter_per_scen.csv")

# write problem scenarios to afile
write.csv(too_high_max_grad_key, "model_runs/too_high_max_grad.csv")
write.csv(error_mods_key, "model_runs/error_mods_key.csv")

all_errors <- rbind(too_high_max_grad_key, error_mods_key)
# calculate performance metrics ----
# look at catch in OM from yrs 125:150
OM_metrics <- NULL
for (i in scenarios$scen_name) { # scenarios$scen_name to make general
  
  iterations <- list.dirs(file.path("model_runs", i), recursive = FALSE, full.names = FALSE)
  # remove iterations that had errors/convergence issues
  test_df <- data.frame( iteration = as.double(iterations), scenario = i)
  test_df <- dplyr::anti_join(test_df, all_errors)
  iterations <- as.character(as.integer(test_df$iteration))
  OM_name <- grep("_OM$",
                  list.dirs(file.path("model_runs", i, iterations[1]), full.names = FALSE),
                  value = TRUE)
  OM_dat <- file.path("model_runs", i, iterations, OM_name, "ss3.dat")
  avg_catch <- unlist(lapply(OM_dat, function(x) get_avg_catch(x, yrs = 126:150)))
  catch_sd <- unlist(lapply(OM_dat, function(x) get_catch_sd(x, yrs = 126:150)))
  short_term_catch <- unlist(lapply(OM_dat, function (x) get_avg_catch(x, yrs = 101:110)))
  tmp_df <- data.frame(iteration = as.integer(iterations), scenario = i,
                       avg_catch = avg_catch, catch_sd = catch_sd, short_term_catch = short_term_catch)
  OM_metrics <- rbind(OM_metrics, tmp_df)
}
SSB_avg <- get_SSB_avg(summary, min_yr = 126, max_yr = 150)

all_metrics <- full_join(OM_metrics, SSB_avg)
all_metrics_long <- tidyr::gather(all_metrics, "metric", "value", 3:ncol(all_metrics))
all_metrics_long$value_bils <- all_metrics_long$value/1000000000
all_metrics_long$scen_fac <- factor(all_metrics_long$scenario,
                                    levels = c("no-red-tide-F-spr-30", "low-red-tide-F-spr-30", "hi-red-tide-F-spr-30",
                                               "no-red-tide-F-spr-45", "low-red-tide-F-spr-45", "hi-red-tide-F-spr-45" ),
                                    labels = c("no", "low", "high", "no", "low", "high"))

all_metrics_long <- all_metrics_long %>%
  tidyr::separate(col = scenario,
                  into = c("OM_scen", "MS"),
                  sep = "-F-",
                  remove = FALSE)

all_metrics_long$MS <- factor(all_metrics_long$MS, levels = c("spr-30", "spr-45"), 
                              labels = c("spr-30 (less precautionary)", "spr-45 (more precautionary)"))

metrics <- unique(all_metrics_long$metric)

plots <- lapply(metrics, function(i, all_metrics_long) {
  title_lab <- switch(i,
                      avg_catch = "Long-term average catch",
                      avg_SSB = "Long-term average SSB",
                      catch_sd = "Long-term catch variability",
                      short_term_catch = "Short-term average catch")
  yaxis_lab <- switch(i,
                      avg_catch = "Catch (billion metric tons)",
                      avg_SSB = "Biomass (billion metric tons)",
                      catch_sd = "Catch (billion metric tons)", 
                      short_term_catch = "Catch (billion metric tons)")
  plot <- ggplot(data = all_metrics_long[all_metrics_long$metric == i, ],
                 aes(x = scen_fac, y = value_bils)) 
  if(i == "avg_SSB") {
    plot <- plot + geom_hline(yintercept = 1342470000/1000000000)
  }
  plot <- plot +
    geom_violin(draw_quantiles = 0.5, aes(fill = MS)) +
    scale_y_continuous(limits = c(0, NA))+
    scale_fill_brewer(palette = "Set2", direction = -1)+
    labs(title = title_lab, x = "OM natural mortality pulses", y = yaxis_lab) +
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
  test_df <- data.frame( iteration = as.double(iterations), scenario = i)
  test_df <- dplyr::anti_join(test_df, all_errors)
  iterations <- as.character(as.integer(test_df$iteration))
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
                  into = c("OM_scen", "MS"),
                  sep = "-F-",
                  remove = FALSE)
catch_cv_df$MS <- factor(catch_cv_df$MS, levels = c("spr-30", "spr-45"), 
                              labels = c("spr-30 (less precautionary)", "spr-45 (more precautionary)"))


plot_cv <- ggplot(data = catch_cv_df, aes(x = scen_fac, y = catch_cv)) +
  geom_violin(draw_quantiles = 0.5, aes(fill = MS)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_brewer(palette = "Set2", direction = -1)+
  labs(title = "Long-term catch variability",
       x = "OM natural mortality pulses", y = "coefficient of variation") +
  theme_classic(base_size = 22)
ggsave(file.path("figures", paste0("run_sel_btarget_scens_", "catch_CV", ".png")),
       width = 8, height = 6, units = "in", device = "png")

plots_no_legend <- lapply(plots, function(x) x + theme(legend.position = "none"))
patchwork_plot <- (plots_no_legend[[1]]+ plot_cv) / (plots_no_legend[[3]] + plots_no_legend[[4]])

ggsave("figures/run_red_tide_scens_perf_metrics.png", patchwork_plot, width = 6*2.5, height = 4*2.5, units = "in")


#would adding a metric for short term catch be helpful to illustrate tradeoffs?

