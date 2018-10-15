library(here)
library(prophet)
library(tidyverse)

# Prophet includes functionality for time series cross validation to measure 
# forecast error using historical data. This is done by selecting cutoff points 
# in the history, and for each of them fitting the model using data only up to 
# that cutoff point. We can then compare the forecasted values to the actual 
# values.

# This cross validation procedure can be done automatically for a range of 
# historical cutoffs using the cross_validation function. We specify:
#    - the forecast horizon (horizon)
# and then optionally:
#    - the size of the initial training period (initial) 
#    - and the spacing between cutoff dates (period). 
#
# By default:
#   - the initial training period is set to three times the horizon
#   - cutoffs are made every half a horizon.

# The output of cross_validation is a dataframe with:
#   - the true values y and 
#   - the out-of-sample forecast values yhat
#         - at each simulated forecast date 
#         - and for each cutoff date. 
#
# In particular, a forecast is made for every observed point between cutoff and 
# cutoff + horizon. 
#
# This dataframe can then be used to compute error measures of yhat vs. y.

# Peyton Manning dataset
if(!file.exists(here("data", "tidy_data", "example_wp_log_peyton_manning.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_peyton_manning.csv",
    destfile = here("data", "tidy_data", "example_wp_log_peyton_manning.csv")
  )
}

tb_data <- read.csv(here("data", "tidy_data", "example_wp_log_peyton_manning.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.Date(ds))

m <- prophet(tb_data)

# Here we do cross-validation to assess prediction performance
#   - on a horizon of 365 days
#   - starting with 730 days of training data in the first cutoff 
#   - and then making predictions every 180 days. 
#
# On this 8 year time series, this corresponds to 11 total forecasts.

tb_data_cv <- cross_validation(m, 
                               initial = 730, period = 180, horizon = 365, 
                               units = 'days')
head(tb_data_cv)
tb_data_cv %>% select(cutoff) %>% unique
tb_data_cv %>% group_by(cutoff) %>% summarise(n = n()) 

# The performance_metrics utility can be used to compute some useful statistics 
# of the prediction performance (yhat, yhat_lower, and yhat_upper compared to y), 
# as a function of the distance from the cutoff (how far into the future the 
# prediction was). 
# These are computed on a rolling window of the predictions in tb_data_cv after 
# sorting by horizon (ds minus cutoff). By default 10% of the predictions will 
# be included in each window, but this can be changed with the rolling_window 
# argument.
tb_data_p <- performance_metrics(tb_data_cv)
head(tb_data_p)

tb_data_p_by_h <- tb_data_p %>% group_by(horizon) %>% summarise_all(mean)

with(tb_data_p_by_h,
     plot(horizon, mse, type = "l"))
with(tb_data_p_by_h,
     plot(horizon, rmse, type = "l"))
with(tb_data_p_by_h,
     plot(horizon, mae, type = "l"))
with(tb_data_p_by_h,
     plot(horizon, mape, type = "l"))
with(tb_data_p_by_h,
     plot(horizon, coverage, type = "l"))

# Cross validation performance metrics can be visualized with 
# plot_cross_validation_metric, here shown for MAPE. 
# Dots show the absolute percent error for each prediction in tb_data_cv. 
# The blue line shows the MAPE, where the mean is taken over a rolling window 
# of the dots. 
# We see for this forecast that errors around 5% are typical for predictions one 
# month into the future, and that errors increase up to around 11% for 
# predictions that are a year out (also see plot above).
plot_cross_validation_metric(tb_data_cv, metric = 'mape')

# The size of the rolling window in the figure can be changed with the optional 
# argument rolling_window, which specifies the proportion of forecasts to use in 
# each rolling window. The default is 0.1, corresponding to 10% of rows from 
# tb_data_cv included in each window; increasing this will lead to a smoother 
# average curve in the figure.

# The initial period should be long enough to capture all of the components of 
# the model, in particular seasonalities and extra regressors: at least a year 
# for yearly seasonality, at least a week for weekly seasonality, etc.
