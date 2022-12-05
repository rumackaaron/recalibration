library("pipeR")
library("stringr")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines(sprintf("%s/params.json",historical_forecast_dir)),collapse=""))
names(params) = tolower(names(params))

historical_forecast_dir = params[["training_dir"]]
experiment_cache_dir = params[["experiment_cache_dir"]]

s.retro.seasons = params[["seasons"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
w.retro.model.weeks = params[["weeks"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
g.epigroups = params[["locations"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Location")
t.targets = params[["targets"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Target")
f.forecasters = params[["forecasters"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Forecaster")

#forecasters = list.dirs(path=historical_forecast_dir,full_names=FALSE,recursive=FALSE)
#seasons = c()
#weeks = c()
#for (f in forecasters) {
#  forecast_files = Sys.glob(sprintf("%s/%s/*_*.csv",historical_forecast_dir,f))
#  file_names = str_split(forecast_files, "/", simplify=TRUE)[,3]
#  file_names = str_sub(file_names, end=-5)
#  season_weeks = str_split(file_names,"_",simplify=TRUE)
#  seasons = sort(unique(c(seasons,season_weeks[,1])))
#  weeks = sort(unique(c(weeks, season_weeks[,2])))
#}

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

swf.forecast.dfs = map_join(load_file,
  s.retro.seasons, w.retro.model.weeks, f.forecasters)

#locations = c()
#targets = c()
#for (i in length(swf.forecast.dfs)) {
#  df = swf.forecast.dfs[[i]]
#  names(df) = tolower(names(df))
#  locations = sort(unique(c(locations,df[["location"]])))
#  targets = sort(unique(c(targets,df[["target"]])))
#}

df_to_val = function(forecast.df, loc, target) {
  if(length(forecast.df) == 1 && is.na(forecast.df)) {
    return(NA)
  }
  names(forecast.df) = tolower(names(forecast.df))
  mask = which(forecast.df[["location"]]==loc & forecast.df[["target"]]==target)
  return(forecast.df[mask,"value"])
}

swgtf.forecast.values = map_join(df_to_val,
  swf.forecast.dfs,g.epigroups,t.targets) %>>%
  aperm(c(1:2,4:5,3))

saveRDS(swgtf.forecast.values,file=file.path(experiment.cache.dir,"swgtf.forecast.values.rds"))