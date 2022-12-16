library("pipeR")
library("jsonlite")
library("stringr")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines("./params.json"),collapse=""))
names(params) = tolower(names(params))

source("save_custom_forecasts.R")
source("save_observed_values.R")
if (tolower(params[["calibration_method"]]) == "ensemble") {
  source("calculate_ensemble_weights.R")
}
source("calibrate_forecasts.R")
source("write_recalibrated_forecasts.R")
