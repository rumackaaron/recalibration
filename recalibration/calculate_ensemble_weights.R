library("pipeR")
library("stringr")
library("jsonlite")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines("./params.json"),collapse=""))
names(params) = tolower(names(params))

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

experiment_cache_dir = params[["experiment_cache_dir"]]

named_idx_list = function(lst) {
  return(with_dimnames(1:length(lst),dimnames(lst)))
}
s.idx.seasons = named_idx_list(s.retro.seasons)
w.idx.weeks = named_idx_list(w.retro.model.weeks)
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

swgtf.forecast.values = readRDS(file.path(experiment_cache_dir,"swgtf.training.forecast.values.rds"))
swgt.observed.values = readRDS(file.path(experiment_cache_dir,"swgt.observed.values.rds"))
swgtf.retro.quantiles = readRDS(file.path(experiment_cache_dir,"swgtf.retro.quantiles.rds"))

swgtfc.forecast.values.file = file.path(experiment_cache_dir,"swgtfc.forecast.values.rds")
swgtfc.forecast.values =
  if (file.exists(swgtfc.forecast.values.file)) {
    readRDS(swgtfc.forecast.values.file)
  } else {
    map_join_tc(function(s,w,g,t,f,cal) {
      week_idxs = w + -week_window:week_window
      week_idxs = week_idxs[1 <= week_idxs & week_idxs <= length(w.idx.weeks)]
      qs = swgtf.retro.quantiles[-s,week_idxs,,t,f]
      qs = qs[!is.na(qs)]
      return(cal(swgtf.forecast.values[[s,w,g,t,f]],qs))
    },
    s.idx.seasons,w.idx.weeks,g.idx.groups,t.idx.targets,f.idx.forecasters,c.calibrations)
  }

if (!file.exists(swgtfc.forecast.values.file)) {
  saveRDS(swgtfc.forecast.values,swgtfc.forecast.values.file)
}

gc()

swgtfc.forecast.evaluations.file = file.path(experiment_cache_dir,"swgtfc.forecast.evaluations.rds")
swgtfc.forecast.evaluations =
  if (file.exists(swgtfc.forecast.evaluations.file)) {
    readRDS(swgtfc.forecast.evaluations.file)
  } else {
    map_join_tc(unibin_log_score,swgtfc.forecast.values,swgt.observed.values)
  }
mode(swgtfc.forecast.evaluations) = "numeric"

if (!file.exists(swgtfc.forecast.evaluations.file)) {
  saveRDS(swgtfc.forecast.evaluations,swgtfc.forecast.evaluations.file)
}

swgtfc.indexer = list(all=NULL,smear=-3:3,all=NULL,each=NULL,each=NULL,all=NULL)

wtfc.weights =
    cv_apply(swgtfc.forecast.evaluations,
             swgtfc.indexer,
             function(train, test) {
               train = R.utils::wrap(train,list(1:5,6))
               train = pmax(train[apply(train,1,function(lst) { all(!is.na(lst))}),],-10)
               degenerate.em.weights = degenerate_em_weights(exp(train))
               return(degenerate.em.weights)
             })[,,,,,,1]
dim(wtfc.weights) = c(length(c.calibrations),length(w.retro.model.weeks),length(t.targets),length(f.forecasters))
wtfc.weights = aperm(wtfc.weights, c(2:4,1))
dimnames(wtfc.weights) <- dimnames(swgtfc.forecast.evaluations)[c(2,4,5,6)]

wtfc.weights.file = file.path(experiment_cache_dir,"wtfc.weights.rds")
if (!file.exists(wtfc.weights.file)) {
  saveRDS(wtfc.weights,wtfc.weights.file)
}
