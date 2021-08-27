# make a plot to better illustrate selectivity
library(r4ss)

# path to OM model for 1 iteration of 1 scenario
om_mod <- file.path("model_runs", "no-red-tide-F-spr-45", "1", "cod_OM")
out <- r4ss::SS_output(om_mod)
SS_plots(out, plot = 2, printfolder = "../../../../figures/r4ss_plots")
