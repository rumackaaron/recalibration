# Inverse cdf function for a histogram distribution
# Inputs:
#   dist: Vector of length n, dist[i] is probability of ith bin
#   qs: Vector of length m, cumulative probabilities for which value is desired
#   bins: Vector of length n+1, values of bins
# Returns:
#   Vector of length m, representing the inverse cdf of qs
inv_cdf = function(dist, qs, bins) {
  d = c(0,cumsum(dist))
  idxs = apply(outer(qs,d,FUN="<"),1,function(x) { which(x)[1]})
  idxs[is.na(idxs)] = length(d)
  p = (qs - d[idxs-1]) / dist[idxs-1]
  result = bins[idxs-1] + p*(bins[idxs]-bins[idxs-1])
  return(result)
}

# Create a histogram pdf from series of values and their quantiles
# Inputs:
#   qs: Vector of length m, series of values
#   qvals: Vector of length m, the cumulative probabilities of qs in output distribution
#   bins: Vector of length n+1, values of bins
# Returns:
#   Vector of length n, histogram pdf such that cdf(qs) = qvals
qdistribution = function(qs, qvals, bins) {
  result = (2:length(bins)) %>>%
    lapply(function(bi) {
      idxs = which(bins[bi-1] <= qs & qs < bins[bi])
      if (!(1 %in% idxs)) {
        left.wt = ((qs[idxs[1]] - bins[bi-1]) / (qs[idxs[1]] - qs[idxs[1]-1])) * (qvals[idxs[1]] - qvals[idxs[1]-1])
      } else {
        left.wt = 0
      }
      if (!(length(qs) %in% idxs)) {
        right.wt = (bins[bi] - qs[idxs[length(idxs)]]) / (qs[idxs[length(idxs)]+1] - qs[idxs[length(idxs)]]) * (qvals[idxs[length(idxs)]+1] - qvals[idxs[length(idxs)]])
      } else {
        right.wt = 0
      }
      if (length(idxs) > 0) {
        return(left.wt + right.wt + qvals[idxs[length(idxs)]] - qvals[idxs[1]])
      } else {
        idx = which(bins[bi-1] < qs)[1]
        wt = (bins[bi]-bins[bi-1]) / (qs[idx]-qs[idx-1]) * (qvals[idx]-qvals[idx-1])
        return(wt)
      }
    })
  mode(result) = "numeric"
  return(result)
}

# Returns PIT value of forecast and observed value
# Inputs:
#   forecast: Vector of length n, histogram pdf
#   val: Scalar, observed value
#   bins: Vector of length n+1, values of bins
# Returns:
#   Scalar, the cdf of forecast at val

get_pit = function(forecast, val, bins) {
  low.bins = which(bins <= val)
  right.idx = which(bins > val)[1]
  right.wt = forecast[right.idx-1]*(bins[right.idx]-val)/(bins[right.idx]-bins[right.idx-1])
  return(sum(forecast[low.bins]) - right.wt)
}

# Same as get_pit, but accounts for CDC's rounding
# Observed value is considered to be in middle of correct bin
get_pit_cdc = function(forecast, val) {
  correct.bin = sum(1:length(val) * val)
  if (correct.bin > 1) {
    low.bins = forecast[1:(correct.bin-1)]
  }  else {
    low.bins = c()
  }
  return(sum(low.bins) + 0.5*forecast[[correct.bin]])
}

# Null correction, do not recalibrate forecast
calibrate_forecast_null = function(forecast, qqs, bins) {
  forecast
}

# Nonparametric Recalibration
# Inputs:
#   forecast: Vector of length n, histogram pdf to be recalibrated
#   qqs: PIT values used in recalibration training
#   bins: Vector of length n+1, values of bins. If -1, defaults to 1:(n+1)
#   alpha: Smoothing factor to blend recalibrated forecast with original forecast.
#          0 will return full recalibrated forecast, 1 returns original forecast
#   fit_spline: If TRUE, apply smoothing spline to qqs
# Returns:
#   Vector of length n, histogram pdf of recalibrated forecast
calibrate_forecast = function(forecast, qqs, bins=-1, alpha=0, fit_spline=FALSE) {
  if (bins == -1) {
    bins = 1:(length(forecast)+1)
  }
  qqs = c(qqs, seq(0,1,length.out=(alpha/(1-alpha))*length(qqs)))
  old.quantiles = seq(0,1,length.out=1001)
  if (fit_spline) {
    sf = splinefun(seq(0,1,length.out=length(qqs)), sort(qqs), method="hyman")
    new.quantiles = sf(old.quantiles)
  } else {
    new.quantiles = quantile(qqs,old.quantiles)
  }
  new.quantiles = pmin(pmax(0,new.quantiles),1)
  new.quantiles[1] = 0
  new.quantiles[length(new.quantiles)] = 1
  forecast.quantiles = inv_cdf(forecast,new.quantiles,bins)
  forecast.quantiles[1] = bins[[1]]
  forecast.quantiles[length(forecast.quantiles)] = bins[[length(bins)]]
  forecast.quantiles = forecast.quantiles[order(forecast.quantiles)] # Sometimes precision problems
  calibrated.forecast = qdistribution(forecast.quantiles,old.quantiles,bins)
  return(calibrated.forecast)
}

# Nonparametric Recalibration
# Similar to above, allows adding k uniformly distributed PIT values to
# training set to lower recalibration training variance
calibrate_forecast_pseudocount = function(k) {
  function(forecast, qqs, bins) {
    qqs = c(qqs,seq(0,1,length.out=k))
    return(calibrate_forecast(forecast,qqs,bins))
  }
}

# Parametric Recalibration, assumes PIT values are drawn from beta distribution
# Inputs:
#   forecast: Vector of	length n, histogram pdf	to be recalibrated
#   qqs: PIT values used in recalibration training
#   bins: Vector of length n+1,	values of bins.	If -1, defaults	to 1:(n+1)
#   alpha: Smoothing factor to blend recalibrated forecast with	original forecast.
#          0 will return full recalibrated forecast, 1 returns original	forecast
# Returns:
#   Vector of length n,	histogram pdf of recalibrated forecast
calibrate_forecast_beta = function(forecast, qqs, bins=-1, alpha=0) {
  if (bins == -1) {
    bins = 1:(length(forecast)+1)
  }
  qqs = pmin(pmax(qqs,0.001),0.999)
  f = function(ab) {
    sum(log(qqs))*(ab[[1]]-1) + sum(log(1-qqs))*(ab[[2]]-1) - length(qqs)*lbeta(ab[[1]],ab[[2]]) }
  ab = stats::optim(c(1,1),f,control=list(fnscale=-1))[["par"]]
  calibrated.forecast = diff((1-alpha)*pbeta(cumsum(c(0,forecast)),ab[[1]],ab[[2]])) + alpha*forecast
  return(calibrated.forecast)
}

# Helper method
calibrate_forecast_uniform_smoothing = function(cal, alpha, fit_spline=FALSE) {
  function(forecast, qqs, bins=-1) {
    return(cal(forecast, qqs, bins=bins, alpha=alpha, fit_spline=fit_spline))
  }
}
