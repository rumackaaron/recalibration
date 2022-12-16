library("pipeR")
library("stringr")
library("jsonlite")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines("./params.json"),collapse=""))
names(params) = tolower(names(params))

experiment_cache_dir = params[["experiment_cache_dir"]]

t.targets = params[["targets"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Target")
f.forecasters = params[["forecasters"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Forecaster")

load_file = function(season, model.week, forecaster) {
  filename = sprintf("%s/%s/%s_%s.csv",
                     historical_forecast_dir,forecaster,season,model.week)
  result = tryCatch({
    read.csv(filename)
  }, error = function(e) {
    NA
  })
  return(result)
}

df_to_val = function(forecast.df, loc, target) {
  if(length(forecast.df) == 1 && is.na(forecast.df)) {
    return(NA)
  }
  names(forecast.df) = tolower(names(forecast.df))
  mask = which(forecast.df[["location"]]==loc & forecast.df[["target"]]==target)
  return(forecast.df[mask,"value"])
}

## Save training forecasts in .rds file in cache directory
historical_forecast_dir = params[["training"]][["dir"]]
s.retro.seasons = params[["training"]][["seasons"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
w.retro.model.weeks = params[["training"]][["weeks"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
g.epigroups = params[["training"]][["locations"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Location")
swf.forecast.dfs = map_join(load_file,
                            s.retro.seasons, w.retro.model.weeks, f.forecasters)
swgtf.forecast.values = map_join(df_to_val,
  swf.forecast.dfs,g.epigroups,t.targets) %>>%
  aperm(c(1:2,4:5,3))

saveRDS(swgtf.forecast.values,file=file.path(experiment_cache_dir,"swgtf.training.forecast.values.rds"))

## Save test forecasts in .rds file in cache directory
historical_forecast_dir = params[["test"]][["dir"]]
s.retro.seasons = params[["test"]][["seasons"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
w.retro.model.weeks = params[["test"]][["weeks"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
g.epigroups = params[["test"]][["locations"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Location")
swf.forecast.dfs = map_join(load_file,
                            s.retro.seasons, w.retro.model.weeks, f.forecasters)
swgtf.forecast.values = map_join(df_to_val,
                                 swf.forecast.dfs,g.epigroups,t.targets) %>>%
  aperm(c(1:2,4:5,3))

saveRDS(swgtf.forecast.values,file=file.path(experiment_cache_dir,"swgtf.test.forecast.values.rds"))
