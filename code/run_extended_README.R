

## remotes::install_github("nmfs-fish-tools/SSMSE")

## ?SSMSE

library(SSMSE) #load the package
library(r4ss) #install using remotes::install_github("r4ss/r4ss)
library(foreach) #if using run_parallel = TRUE
library(doParallel) #if using run_parallel = TRUE

# Create a folder for the output in the working directory.
run_SSMSE_dir <- file.path("run_SSMSE-ex")
dir.create(run_SSMSE_dir)

cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
# develop_OMs will save a model called "cod_SR_BH_steep_1" in the out_dir
# specified
develop_OMs(OM_name = "cod", out_dir = run_SSMSE_dir, par_name = "SR_BH_steep",
            par_vals = 1, refit_OMs = FALSE, hess = FALSE)
# OM model for scenario 2
cod_1_path <- file.path(run_SSMSE_dir, "cod_SR_BH_steep_1")

# Start from a list created by a helper function
template_mod_change <- create_future_om_list() 
# add recruitment deviations
rec_dev_specify <- template_mod_change[[1]]
rec_dev_specify$pars <- "rec_devs" # apply change to rec devs
rec_dev_specify$scen <- c("replicate", "all")
# using 1 to 100 means the sd or mean will be calculated by taking the sd across years
# from 1 to 100
rec_dev_specify$input$first_yr_averaging <- 1
rec_dev_specify$input$last_yr_averaging <- 100
# The following 2 lines suggest that this change is immediately applied in year
# 101, with no transitory period for using sd 0 to the new sd.
rec_dev_specify$input$last_yr_orig_val <- 100
rec_dev_specify$input$first_yr_final_val <- 101
rec_dev_specify$input$ts_param <- "sd" # this change is for the sd
# no input value needed since it will be calclated from the historical rec devs.
rec_dev_specify$input$value <- NA
rec_dev_specify

# put together the change for selectivity (random values around the orig val, with
# an sd of 0.2)
mod_change_sel <- template_mod_change[[1]]
mod_change_sel$scen[2] <- "all" # apply to all scenarios
# The following 2 lines suggest that this change is immediately applied in year
# 101, with no transitory period for using sd 0 to the new sd.
# historical values are NA in this case, because they are not used to determine
# the sd to use.
mod_change_sel$input$last_yr_orig_val <- 100
mod_change_sel$input$first_yr_final_val <- 101
mod_change_sel$input$ts_param <- "sd" # this change is for the sd
mod_change_sel$input$value <- 0.2 # se to use in the projection period
mod_change_sel

future_om_list_recdevs_sel <- list(rec_dev_specify, 
                                   mod_change_sel) 
nyrs <- 50
datfile <- system.file("extdata", "models", "cod", "ss3.dat", package = "SSMSE")
sample_struct_1_scen <- create_sample_struct(dat = datfile, nyrs = 50) # note warning
sample_struct_1_scen

sample_struct_1_scen$lencomp <- NULL # don't use length sampling

sample_struct_list_all <- list("h-ctl" = sample_struct_1_scen, "h-1" = sample_struct_1_scen)

run_res_path <- file.path(run_SSMSE_dir, "results")
dir.create(run_res_path)
res <- run_SSMSE(
    scen_name_vec = c("h-ctl", "h-1"),# name of the scenario
    out_dir_scen_vec = run_res_path, # directory in which to run the scenario
    iter_vec = c(5,5), # run with 5 iterations each
    OM_name_vec = NULL, # specify directories instead
    OM_in_dir_vec = c(cod_mod_path, normalizePath(cod_1_path)), # OM files
    EM_name_vec = c("cod", "cod"), # cod is included in package data
    MS_vec = c("EM","EM"), # The management strategy is specified in the EM
    nyrs_vec = c(nyrs, nyrs), # Years to project OM forward
    nyrs_assess_vec = c(5, 5), # Years between assessments
    future_om_list = future_om_list_recdevs_sel,
    run_parallel = TRUE, # Run iterations in parallel
    sample_struct_list = sample_struct_list_all, # How to sample data for running the EM.
    sample_struct_hist_list = NULL, # because this is null, will just use sampling
    # as in the current OM data file for the historical period.
    seed = 12345) #Set a fixed integer seed that allows replication 

list.dirs(run_res_path, recursive = FALSE)

# See folders for scenario 1.
list.dirs(file.path(run_res_path, "h-ctl"), recursive = FALSE)

# see folders for the first iteration of scenario 1
list.dirs(file.path(run_res_path, "h-ctl", "1"), recursive = FALSE)

# Summarize 1 iteration of output
summary <- SSMSE_summary_all(run_res_path)

library(ggplot2) # use install.packages("ggplot2") to install package if needed
library(tidyr) # use install.packages("tidyr") to install package if needed
library(dplyr) # use install.packages("dplyr") to install package if needed

check_convergence <- function(summary, min_yr = 101, max_yr = 120) {
  require(dplyr) # note: not the best way to do this
  if(any(!is.na(summary$scalar$params_on_bound))) {
    warning("Params on bounds")
  } else {
    message("No params on bounds")
  }
  summary$ts$model_type <- ifelse(grepl("_EM_", summary$ts$model_run), "EM", "OM")
  calc_SSB <- summary$ts %>%
    filter(year >= min_yr & year <= max_yr) %>%
    select(iteration, scenario, year, model_run, model_type, SpawnBio)
  OM_vals <- calc_SSB %>%
              filter(model_type == "OM") %>%
              rename(SpawnBio_OM = SpawnBio ) %>%
              select(iteration, scenario, year, SpawnBio_OM)
  EM_vals <- calc_SSB %>%
               filter(model_type == "EM") %>%
               rename(SpawnBio_EM = SpawnBio) %>%
               select(iteration, scenario, year, model_run, SpawnBio_EM)
  bind_vals <- full_join(EM_vals, OM_vals, by = c("iteration", "scenario", "year")) %>%
                  mutate(SSB_ratio = SpawnBio_EM/SpawnBio_OM)
  filter_SSB <- bind_vals %>%
    filter(SSB_ratio > 2 | SSB_ratio < 0.5)
  if(nrow(filter_SSB) > 0 ) {
    warning("Some large/small SSBs relative to OM")
  } else {
    message("All SSBs in EM are no greater than double and no less than half SSB vals in the OM")
  }
  return_val <- bind_vals
}
values <- check_convergence(summary = summary, min_yr = 101, max_yr = 150)



# get_SSB_avg calculates the SSB in each year for each
# iteration of the operating model, then takes the average over the years from
# min_yr, to max_year. It uses the summary object as input to do these
# calculations.
get_SSB_avg <- function(summary, min_yr, max_yr) {
  OM_vals <- unique(summary$ts$model_run)
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  SSB_yr <- summary$ts %>%
          filter(year >= min_yr & year <= max_yr) %>%
          filter(model_run %in% OM_vals) %>%
          select(iteration, scenario, year, SpawnBio) %>%
          group_by(iteration, scenario) %>%
          summarize(avg_SSB = mean(SpawnBio), .groups = "keep") %>%
          ungroup()
  SSB_yr
}
avg_SSB <- get_SSB_avg(summary, min_yr = 104, max_yr = 100+nyrs)

# function to summarize data in plot
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))
}
# Now, plot the average relative spawning stock biomass for years 104 - 106
ggplot(data = avg_SSB, aes(x = scenario, y = avg_SSB)) +
  stat_summary(fun.data = data_summary,
               position = position_dodge(width = 0.9), color = "blue") +
  labs(title = "Long-term average  SSB\n(years 104-106)",
       x = "Scenario", y = "SSB") +
  theme_classic()