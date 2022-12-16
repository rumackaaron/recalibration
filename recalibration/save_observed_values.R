library("pipeR")
library("stringr")
library("jsonlite")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines("./params.json"),collapse=""))
names(params) = tolower(names(params))

historical_forecast_dir = params[["training"]][["training_dir"]]
experiment_cache_dir = params[["experiment_cache_dir"]]

s.retro.seasons = params[["training"]][["seasons"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
w.retro.model.weeks = params[["training"]][["weeks"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
g.epigroups = params[["training"]][["locations"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Location")
t.targets = params[["targets"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Target")
f.forecasters = params[["forecasters"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Forecaster")

target_numbins = params[["num_bins"]] %>>%
  stats::setNames(names(t.targets))

observed_df = read.csv(file.path(historical_forecast_dir,"observed_bins.csv"))
names(observed_df) = tolower(names(observed_df))

df_to_val = function(season, week, loc, target) {
  mask = which(season==observed_df[["season"]] & week==observed_df[["week"]] & observed_df[["location"]]==loc & observed_df[["target"]]==target)
  result = rep(0, target_numbins[[target]])
  result[observed_df[[mask,"bin"]]] = 1
  return(result)
}

swgt.observed.values = map_join(df_to_val,s.retro.seasons,w.retro.model.weeks,g.epigroups,t.targets)

saveRDS(swgt.observed.values,file=file.path(experiment_cache_dir,"swgt.observed.values.rds"))

map_join_tc = function(f, ...) {
  map_join(function(...) { tryCatch(f(...), error = function(e) { NA })}, ...)
}

swgtf.forecast.values = readRDS(file.path(experiment_cache_dir,"swgtf.training.forecast.values.rds"))
swgtf.retro.quantiles = map_join(get_pit_cdc, swgtf.forecast.values, swgt.observed.values)
mode(swgtf.retro.quantiles) = "numeric"

saveRDS(swgtf.retro.quantiles,file=file.path(experiment_cache_dir,"swgtf.retro.quantiles.rds"))
