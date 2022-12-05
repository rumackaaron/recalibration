library("pipeR")
library("rjson")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines("./params.json"),collapse=""))
names(params) = tolower(names(params))

s.retro.seasons = str_split(params[["seasons"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
w.retro.model.weeks = str_split(params[["weeks"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
g.epigroups = str_split(params[["locations"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Location")
t.targets = str_split(params[["targets"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Target")
f.forecasters = str_split(params[["forecasters"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Forecaster")

historical_dir = params[["historical_dir"]]
experiment_cache_dir = params[["experiment_cache_dir"]]
test_forecast_dir = params[["test_forecast_dir"]]
calibration_method = tolower(params[["calibration_method"]])

swgtf.retro.quantiles = readRDS(file.path(experiment_cache_dir,"swgtf.retro.quantiles.rds"))

named_idx_list = function(lst) {
  return(with_dimnames(1:length(lst),dimnames(lst)))
}
s.idx.seasons = named_idx_list(s.retro.seasons)
w.idx.weeks = named_idx_list(w.retro.model.weeks)
g.idx.groups = named_idx_list(g.epigroups)
t.idx.targets = named_idx_list(t.target.specs)
f.idx.forecasters = named_idx_list(f.forecasters)

c.calibrations = list('nonparametric'=calibrate_forecast,
                      'parametric'=calibrate_forecast_beta,
                      'none'=calibrate_forecast_null) %>>%
  with_dimnamesnames("Calibration")

week_window = params[["week_window"]]
if (week_window == "all") {
  week_window = length(w.idx.weeks)
}

map_join_tc = function(f, ...) {
  map_join(function(...) { tryCatch(f(...), error = function(e) { NA })}, ...)
}

wtfc.weights.file = file.path(experiment.cache.dir,"wtfc.weights.rds")
if (file.exists(wtfc.weights.file)) {
  wtfc.weights = readRDS(wtfc.weights.file)
}

load_file = function(season, model.week, forecaster) {
  filename = sprintf("%s/%s/%s_%s.csv",
                     test_forecast_dir,forecaster,season,model.week)
  result = tryCatch({
    read.csv(filename)
  }, error = function(e) {
    NA
  })
  return(result)
}

swf.forecast.dfs = map_join(load_file,
                            s.retro.seasons, w.retro.model.weeks, f.forecasters)

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

swgtf.calibrated.forecast.values = 
  if (calibration_method %in% c("none","nonparametric","parametric")) {
    map_join_tc(function(s,w,g,t,f) {
      week.idxs = w + -week_window:week_window
      week.idxs = week.idxs[1 <= week.idxs & week.idxs <= length(w.retro.model.weeks)]
      qs = swgtf.retro.quantiles[,week.idxs,,t,f]
      qs = qs[!is.na(qs)]
      return(c.calibrations[[calibration_method]](swgtf.forecast.values[[s,w,g,t,f]],qs))
    }, s.test.idx.seasons,w.test.idx.weeks,g.test.idx.groups,t.test.idx.targets,f.test.idx.forecasters)
  } else if (calibration_method == "ensemble") {
    swgtfc.forecast.values = map_join_tc(function(s,w,g,t,f,cal) {
      week.idxs = w + -week_window:week_window
      week.idxs = week.idxs[1 <= week.idxs & week.idxs <= length(w.retro.model.weeks)]
      qs = swgtf.retro.quantiles[,week.idxs,,t,f]
      qs = qs[!is.na(qs)]
      return(cal(swgtf.forecast.values[[s,w,g,t,f]],qs))
    }, s.test.idx.seasons,w.test.idx.weeks,g.test.idx.groups,t.test.idx.targets,f.test.idx.forecasters,
    c.calibrations)
    
    map_join_tc("*",swgtfc.forecast.values,wtfc.weights,lapply_variant=lapply) %>>%
      apply(1:5,Reduce,f="+")
  }

swgtf.calibrated.forecasts.file = file.path(experiment.cache.dir,"swgtf.calibrated.forecasts.rds")
saveRDS(swgtf.calibrated.forecast.values, swgtf.calibrated.forecasts.file)