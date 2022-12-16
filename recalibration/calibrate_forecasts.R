library("pipeR")
library("jsonlite")
library("stringr")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines("./params.json"),collapse=""))
names(params) = tolower(names(params))

s.training.seasons = str_split(params[["training"]][["seasons"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
s.test.seasons = str_split(params[["test"]][["seasons"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
w.training.model.weeks = str_split(params[["training"]][["weeks"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
w.test.model.weeks = str_split(params[["test"]][["weeks"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
g.epigroups = str_split(params[["test"]][["locations"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Location")
t.targets = str_split(params[["targets"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Target")
f.forecasters = str_split(params[["forecasters"]],",") %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Forecaster")

experiment_cache_dir = params[["experiment_cache_dir"]]
calibration_method = tolower(params[["calibration_method"]])

swgtf.retro.quantiles = readRDS(file.path(experiment_cache_dir,"swgtf.retro.quantiles.rds"))

named_idx_list = function(lst) {
  return(with_dimnames(1:length(lst),dimnames(lst)))
}
s.idx.seasons = named_idx_list(s.test.seasons)
w.idx.training.weeks = named_idx_list(w.training.model.weeks)
g.idx.groups = named_idx_list(g.epigroups)
t.idx.targets = named_idx_list(t.targets)
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

swgtf.test.forecast.values = readRDS(file.path(experiment_cache_dir,"swgtf.test.forecast.values.rds"))

wtfc.weights.file = file.path(experiment_cache_dir,"wtfc.weights.rds")
if (file.exists(wtfc.weights.file)) {
  wtfc.weights = readRDS(wtfc.weights.file)
}

swgtf.calibrated.forecast.values = 
  if (calibration_method %in% c("none","nonparametric","parametric")) {
    map_join_tc(function(s,w,g,t,f) {
      week.idxs = w + -week_window:week_window
      week.idxs = week.idxs[week.idxs %in% w.training.model.weeks]
      qs = swgtf.retro.quantiles[,week.idxs,,t,f]
      qs = qs[!is.na(qs)]
      return(c.calibrations[[calibration_method]](swgtf.test.forecast.values[[s,w,g,t,f]],qs))
    }, s.idx.seasons,w.test.model.weeks,g.idx.groups,t.idx.targets,f.idx.forecasters)
  } else if (calibration_method == "ensemble") {
    swgtfc.forecast.values = map_join_tc(function(s,w,g,t,f,cal) {
      week.idxs = w + -week_window:week_window
      week.idxs = week.idxs[week.idxs %in% w.training.model.weeks]
      qs = swgtf.retro.quantiles[,week.idxs,,t,f]
      qs = qs[!is.na(qs)]
      return(cal(swgtf.test.forecast.values[[s,w,g,t,f]],qs))
    }, s.idx.seasons,w.test.model.weeks,g.idx.groups,t.idx.targets,f.idx.forecasters,c.calibrations)
    mult = function(a,b) { a*b }
    
    tmp = map_join_tc(mult,swgtfc.forecast.values,wtfc.weights,lapply_variant=lapply) %>>%
      apply(1:5,Reduce,f="+")
    map_join(function(s,w,g,t,f) { tmp[,s,w,g,t,f]},
      s.idx.seasons,w.idx.weeks,g.idx.groups,t.idx.targets,f.idx.forecasters)
  }

swgtf.calibrated.forecasts.file = file.path(experiment_cache_dir,"swgtf.calibrated.forecasts.rds")
saveRDS(swgtf.calibrated.forecast.values, swgtf.calibrated.forecasts.file)
