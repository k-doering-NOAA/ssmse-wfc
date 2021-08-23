# Run scenarios relating to q 

# load pkgs set options ----
#devtools::install_github("r4ss/r4ss", ref = "64b9e62")
#devtools::install_github("nmfs-fish-tools/SSMSE", ref = "db43ab4")
library(SSMSE)
library(r4ss)
library(dplyr)
library(ggplot2)
# functions for convergence and performance metrics, get from other gh repo
source("https://raw.githubusercontent.com/k-doering-NOAA/ssmse-afs/master/code/get_metrics.R")

# specify locations, create folders ----
cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
datfile_path <- file.path(cod_mod_path, "ss3.dat")
fig_path <- "figures"
runs_path <- "model_runs"
mods_path <- "input_models"
dir.create(fig_path)
dir.create(runs_path)
dir.create(mods_path)

# define the scenarios ----
niters <- 10
# the scenarios are: 
# three levels of M changes in the OM (none, more frequent, less frequent)
# 2 different management scenarios
# in all scenarios, uncertainty in the selectivity moving forward
# metrics: long term avg catch, long term catch variability, long term biomass

scen_red_tide <- c("no-red-tide", "low-red-tide", "hi-red-tide")
scen_HCR <- c("F-spr-30", "F-spr-45")

scenarios <- data.frame(
  scen_name = c(paste0(scen_red_tide, "-",scen_HCR[1]),
                paste0(scen_red_tide, "-", scen_HCR[2])),
  EM_path = rep(c(file.path(mods_path, scen_HCR)), times = c(3,3))
)

# manipulate EM Forecasting ----
# no need to re-run model for the EM, 
for (i in scen_HCR) {
  tmp_cod_path <- file.path(mods_path, i)
  file.copy(from = cod_mod_path, to = mods_path, recursive = TRUE)
  file.rename(from = file.path(mods_path, "cod"), to = tmp_cod_path)

  fore <- r4ss::SS_readforecast(file.path(tmp_cod_path, "forecast.ss"), 
                                verbose = FALSE)
  forecast_method <- switch(i, 
                           "F-msy" = 2,
                           "F-spr-30" = 1,
                           "F-spr-45" = 1)
  fcast_target <- switch(i, 
                         "F-msy" = 0.45,
                         "F-spr-30" = 0.3,
                         "F-spr-45" = 0.45)
  # manipulate the forecasting file.
  fore$MSY <- 2 # calculate Fmsy, needed for F-msy scenario
  fore$SPRtarget <- fcast_target # differs between scenarios
  fore$Forecast <- forecast_method # differs between scenarios 
  fore$ControlRuleMethod <- 0 # don't use a ramp HCR at all
  r4ss::SS_writeforecast(fore, tmp_cod_path, verbose = FALSE, overwrite = TRUE)
  file.remove(file.path(tmp_cod_path, "forecast.ss_new")) # to make sure it is not used.
}

# set up the future om deviations ----
# Set this up for the 3 different operating mode scenarios
# in all cases, we want to use random fluctuations on selectivity
# changing M depends on the scenario.


# put together the change for selectivity (random values around the orig val, with
# an sd of 0.2)
template_mod_change <- create_future_om_list()
mod_change_sel <- template_mod_change[[1]]
mod_change_sel$scen[2] <- "all"
mod_change_sel$input$last_yr_orig_val <- 100
mod_change_sel$input$first_yr_final_val <- 101
mod_change_sel$input$ts_param <- "sd"
mod_change_sel$input$value <- 0.2

# put together change for M
# more stochasisity could be added, but starting with this is still interesting
template_custom_change <- create_future_om_list(example_type = "custom")

mod_change_M <- template_custom_change[[1]]


M_no_scen <- rep(rep(0.2, 50), times = niters)
M_low_scen <- rep(rep(c(0.2, 0.2, 0.2, 0.2, 0.3), length.out = 50), times = niters)
M_hi_scen <- rep(rep(c(0.2, 0.2, 0.2, 0.2, 0.4), length.out = 50), times = niters)
M_custom_dataframe <- data.frame(
  par = "NatM_p_1_Fem_GP_1", 
  scen = rep(scenarios$scen_name, times = rep(50*niters, 6)), 
  iter = rep(rep(seq_len(niters), times = rep(50, niters)), times = 6), 
  yr = rep(101:150, times = 6*niters), 
  value = c(M_no_scen, M_low_scen, M_hi_scen,
            M_no_scen, M_low_scen, M_hi_scen))
mod_change_M$pars <- "NatM_p_1_Fem_GP_1"
mod_change_M$scen <- c("replicate", "all")
mod_change_M$input <- M_custom_dataframe

# add recruitment deviations
rec_dev_specify <- template_mod_change[[1]]
rec_dev_specify$pars <- "rec_devs"
rec_dev_specify$scen <- c("replicate", "all")
# rec_dev_specify$input$first_yr_averaging <- 1
# rec_dev_specify$input$last_yr_averaging <- 100
rec_dev_specify$input$last_yr_orig_val <- 100
rec_dev_specify$input$first_yr_final_val <- 101
rec_dev_specify$input$ts_param <- "sd"
rec_dev_specify$input$value <- 0.1

# put together a complete list
future_om_list <- list(mod_change_M, mod_change_sel, rec_dev_specify)

# get the sampling scheme ----
# use the historical sampling scheme, so don' t need to create one


# for sampling scheme in the projections, use the historical sampling scheme to
# the extent possible; if no pattern found, then create a manual one.
sample_struct <- SSMSE::create_sample_struct(dat = datfile_path, nyrs = 50)
sample_struct$meanbodywt <- NULL
sample_struct$MeanSize_at_Age_obs <- NULL
# modify, because there were NAs
sample_struct$lencomp <- data.frame(Yr = seq(105, 150, by = 5), 
                                    Seas = sample_struct$lencomp$Seas,
                                    FltSvy = sample_struct$lencomp$FltSvy, 
                                    Sex = sample_struct$lencomp$Sex, 
                                    Part = sample_struct$lencomp$Part,
                                    Nsamp = sample_struct$lencomp$Nsamp)
sample_struct_list <- list(sample_struct, 
                           sample_struct,
                           sample_struct,
                           sample_struct, 
                           sample_struct,
                           sample_struct
                           )
# call SSSMSE ----
out <- SSMSE::run_SSMSE(out_dir_scen_vec = rep("model_runs", 6),
                        scen_name_vec = scenarios$scen_name,
                        iter_vec = rep(niters, 6),
                        OM_name_vec = rep("cod", 6),
                        OM_in_dir_vec = NULL,
                        EM_in_dir_vec = scenarios$EM_path,
                        run_EM_last_yr = FALSE,
                        MS_vec = "EM",
                        use_SS_boot_vec = TRUE,
                        nyrs_vec = rep(50, 6),
                        nyrs_assess_vec = rep(5, 6),
                        sample_struct_list = sample_struct_list,
                        future_om_list = future_om_list,
                        verbose = FALSE,
                        seed = 123,
                        run_parallel = TRUE,
                        
                        )
saveRDS(out, file = file.path("model_runs", "run_SSMSE_out_23Aug2021.rda"))
# 
# # look at results ----
summary <- SSMSE::SSMSE_summary_all(dir = "model_runs")
# 
# summary <- list()
# summary$ts <- read.csv("model_runs/ss3sim_ts.csv")
# summary$scalar <- read.csv("model_runs/ss3sim_scalar.csv")
# 
# #check for errored iterations
# lapply(out, function(x) x$errored_iterations)

SSB_df <- check_convergence(summary, n_EMs = 6, max_yr = 150)

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
                  into = c("OM_scen", "HCR"),
                  sep = "-F-",
                  remove = FALSE)

metrics <- unique(all_metrics_long$metric)

plots <- lapply(metrics, function(i, all_metrics_long) {
  title_lab <- switch(i,
                      avg_catch = "Long-term average catch (years 126-150)",
                      avg_SSB = "Long-term average SSB (years 126-150)",
                      catch_sd = "Long-term catch variability (years 126-150)")
  yaxis_lab <- switch(i,
                      avg_catch = "Catch (billion metric tons)",
                      avg_SSB = "Biomass (billion metric tons)",
                      catch_sd = "Catch (billion metric tons)")
  plot <- ggplot(data = all_metrics_long[all_metrics_long$metric == i, ],
                 aes(x = scen_fac, y = value_bils)) +
    geom_violin(draw_quantiles = 0.5, aes(fill = HCR)) +
    scale_y_continuous(limits = c(0, NA))+
    scale_fill_brewer(palette = "Set2", direction = -1)+
    labs(title = title_lab, x = "OM selectivity", y = yaxis_lab) +
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
                  into = c("OM_scen", "HCR"),
                  sep = "-F-",
                  remove = FALSE)

plot_cv <- ggplot(data = catch_cv_df, aes(x = scen_fac, y = catch_cv)) +
  geom_violin(draw_quantiles = 0.5, aes(fill = HCR)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_brewer(palette = "Set2", direction = -1)+
  labs(title = "Long-term catch variability (years 126-150)",
       x = "OM selectivity", y = "coefficient of variation") +
  theme_classic(base_size = 22)
ggsave(file.path("figures", paste0("run_sel_btarget_scens_", "catch_CV", ".png")),
       width = 8, height = 6, units = "in", device = "png")
