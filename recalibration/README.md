# Summary
This directory contains scripts that allow a user to apply the recalibration methods described in this [paper](https://arxiv.org/abs/2112.06305) to his or her own forecasts.

## Params file

The user must provide forecasts for training and testing, as well as observed values for training. The file `params.json` has the following fields, which specify how the training and testing are performed:

* A dictionary labeled `"training"`:
    * `"seasons"`: A list of seasons to train on
    * `"weeks"`: An integer list of weeks to train on
    * `"locations"`: A list of locations to train on
    * `"dir"`: A directory containing forecasts for those pairs of seasons, weeks, and locations. The file format of these forecasts is described below.
* A dictionary labeled `"test"`:
    * `"seasons"`: A list of seasons to test on
    * `"weeks"`: An integer list of weeks to test on
    * `"locations"`: A list of locations to test on
    * `"dir"`: A directory containing forecasts for those pairs of seasons, weeks, and locations. The file format of these forecasts is described below.
* `"target"`: A list of forecasting targets for training and testing
* `"num_bins"`: An integer list of the same length as `"target"` with the number of bins for each probabilistic forecast for that individual target
* `"forecasters"`: A string list of forecasters for training and testing
* `"week_window"`: An integer representing the week window for recalibration training (see Sections 2.6, 3.1) or `"all"` to use all weeks for frecalibration training
* `"calibration_method"`: Which calibration method to be used. One of `{"nonparametric","parametric","none","ensemble"}` (see Section 2)
* `"experiment_cache_dir"`: A directory in which to write intermediate results of the recalibration method

The forecast files must be in the correct format. There should be a separate `.csv` file for each season, week, and forecaster, located in `<training/test dir>/<Forecaster>/<Season>_<Week>.csv`. Each forecast file must have a header with the columns `"Location"`, `"Target"`, and `"Value"`, and `num_bins` rows for each location and target. The values are the probabilities assigned to the appropriate bin and must be ordered.

The observed bins file must also be in the correct format. It must be located in `<training dir>/observed_bins.csv` and must have a header with the columns `"Season"`, `"Week"`, `"Location"`, `"Target"`, and `"Bin"`. The Bin column contains the index of the observed value's bin for the given season, week, location, and target and is 1-indexed.

## Run Recalibration Tool
To actually run the recalibration tool, first ensure that the data in `params.json` is correct, that the forecasts are in the proper format and location, and that the cache directory has been created. Then run `Rscript recalibrate_main.R` from this directory.

The recalibrated forecasts will be located in the same directory and with the same format as the test forecasts, except `"_Recalibrated"` will be appended to each forecaster's name.

## Sample Forecasts
A sample set of forecasts is provided in the `sample` directory. There are 21 weeks in each season, and for each location and season, the true value for week `w` follows a beta distribution parameterized by $\alpha_w = 1 + (w-1)/20$ and $\beta_w = 2 - (w-1)/20$.

We provide three sample forecasters:
* Climatological: Produces the same forecast every week, the average density over all weeks.
* Past: Produces the true distribution for the previous week.
* Oracle: Produces the true distribution for the current week.

The number of bins for discretization is 20, and sample observed bins are provided in `sample/training/observed_bins.csv`.