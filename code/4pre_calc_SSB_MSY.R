# Calculate SSB MSY for the models
library(r4ss)
library(SSMSE)

# was not originally calculated during the SSMSE routine, so need to rerun
# OMs without estimation.
scenarios <- list.dirs(file.path("model_runs"), recursive = FALSE,
                       full.names = FALSE)
bin <- get_bin()
# bin <- strsplit(bin, split = "\"")[[1]][2]
all_iters <- lapply(scenarios, function(x) {
  iters <- list.dirs(file.path("model_runs", x), recursive = FALSE,
                                   full.names = TRUE)
  iters
  })

all_iters <- unlist(all_iters)
all_iters <- file.path(all_iters, "cod_OM")
#change the forecast files
iters_run <- lapply(all_iters, function(y) {
  tmp_fore <- SS_readforecast(file.path(y, "forecast.ss"), readAll = TRUE)
  tmp_fore[["benchmarks"]] <- 1
  tmp_fore[["MSY"]] <- 2
  SS_writeforecast(tmp_fore, dir = file.path(y), overwrite = TRUE, 
                   writeAll = TRUE)
  invisible(y)
})

# run the models without estimation
ran_mods <- lapply(all_iters, function(i) {
  run_ss_model(dir = i, admb_options = "-stopph 0 -nohess")
  invisible(i)
})

# Get MSY values for each operating model
# pull from the ss_summary file since shorter to read

msy_list <- lapply(all_iters, function (d) {
  tmp_sum <- SS_read_summary(file = file.path(d, "ss_summary.sso"), verbose = FALSE)
  tmp_MSY <- tmp_sum$derived_quants[which(rownames(tmp_sum$derived_quants) == "SSB_MSY"),"Value"]
  tmp_df <- data.frame(Scenario = basename(dirname(dirname(d))),
                       Iteration = basename(dirname(d)), 
                       Model = "cod_OM", Metric = "SSB_MSY", Value = tmp_MSY)
  tmp_df
})

#this is just the same number, not sure if valid.
msy_df <- do.call(rbind, msy_list)
  
write.csv(msy_df, file.path("model_runs", "OM_MSY.csv"))

# It turns out OM_MSY is the same for all models...1342470000/1000000000 billion metric tons.
# I was expecting it to be different for each one, but perhaps I am not thinking about this correctly?
# another possibility is that running the model without estimation doesn't correctly calculate
# MSY, although it seems like it should to me....
