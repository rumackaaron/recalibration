library("pipeR")
library("stringr")
library("jsonlite")
devtools::load_all("../epiforecast")

options(mc.cores=parallel::detectCores()-1L)

params = fromJSON(paste(readLines("./params.json"),collapse=""))
names(params) = tolower(names(params))

s.retro.seasons = params[["test"]][["seasons"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Season")
w.retro.model.weeks = params[["test"]][["weeks"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Model Week")
g.epigroups = params[["test"]][["locations"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Location")
t.targets = params[["targets"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Target")
f.forecasters = params[["forecasters"]] %>>%
  stats::setNames(.) %>>%
  with_dimnamesnames("Forecaster")

historical_forecast_dir = params[["test"]][["dir"]]
experiment_cache_dir = params[["experiment_cache_dir"]]
swgtf.calibrated.forecasts.file = file.path(experiment_cache_dir,"swgtf.calibrated.forecasts.rds")
swgtf.calibrated.forecast.values = readRDS(swgtf.calibrated.forecasts.file)

for (f in f.forecasters) {
  recal_dir = file.path(historical_forecast_dir,paste0(f,"_Recalibrated"))
  if (!dir.exists(recal_dir)) {
    dir.create(recal_dir)
  }
  for (s in s.retro.seasons) {
    for (w in w.retro.model.weeks) {
      out_file = file.path(recal_dir,paste0(s,"_",w,".csv"))
      out_df = data.frame(Location=c(),Target=c(),Value=c())
      for (g in g.epigroups) {
        for (t in t.targets) {
          forecast_df = data.frame(Location=g, Target=t, Value = swgtf.calibrated.forecast.values[[s,w,g,t,f]])
          out_df = rbind(out_df, forecast_df)
        }
      }
      write.csv(out_df,out_file,quote=FALSE,row.names=FALSE)
    }
  }
}
