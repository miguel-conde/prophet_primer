library(here)
library(prophet)
library(tidyverse)

if(!file.exists(here("data", "tidy_data", "example_wp_log_peyton_manning.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_peyton_manning.csv",
    destfile = here("data", "tidy_data", "example_wp_log_peyton_manning.csv")
  )
}

tb_data <- read.csv(here("data", "tidy_data", "example_wp_log_peyton_manning.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.Date(ds))


# 1 - HOLIDAYS ------------------------------------------------------------


playoffs <- data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2008-01-13', '2009-01-03', '2010-01-16',
                 '2010-01-24', '2010-02-07', '2011-01-08',
                 '2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
superbowls <- tibble(
  holiday = 'superbowl',
  ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holidays <- bind_rows(playoffs, superbowls)
holidays

# Fit the model
m <- prophet(tb_data, holidays = holidays)

# Make predictions
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

forecast %>% 
  select(ds, playoff, superbowl) %>% 
  filter(abs(playoff + superbowl) > 0) %>%
  tail(10)

prophet_plot_components(m, forecast)


# 2 - SEASONALITIES ------------------------------------------------------

# The number of terms in the partial sum (the order) is a parameter that 
# determines how quickly the seasonality can change.
m <- prophet(tb_data)
prophet:::plot_yearly(m)

# The default values are often appropriate, but they can be increased when the 
# seasonality needs to fit higher-frequency changes, and generally be less 
# smooth. The Fourier order can be specified for each built-in seasonality when 
# instantiating the model, here it is increased to 20
m <- prophet(tb_data, yearly.seasonality = 20)
prophet:::plot_yearly(m)

# Increasing the number of Fourier terms allows the seasonality to fit faster 
# changing cycles, but can also lead to overfitting: N Fourier terms corresponds 
# to 2N variables used for modeling the cycle


# 2.1 - Specifying Custom Seasonalities -----------------------------------

# Prophet will by default fit weekly and yearly seasonalities, if the time 
# series is more than two cycles long. It will also fit daily seasonality for 
# a sub-daily time series. You can add other seasonalities (monthly, quarterly, 
# hourly) using the add_seasonality function

# Replace the weekly seasonality with monthly seasonality. The monthly 
# seasonality then will appear in the components plot
m <- prophet(weekly.seasonality=FALSE)
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- fit.prophet(m, tb_data)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)


# 3 - Prior scale for holidays and seasonality ----------------------------

# If you find that the holidays are overfitting, you can adjust their prior 
# scale to smooth them using the parameter holidays_prior_scale. 
# By default this parameter is 10, which provides very little regularization. 
# Reducing this parameter dampens holiday effects:
m <- prophet(tb_data, holidays = holidays, holidays.prior.scale = 0.05)
forecast <- predict(m, future)
forecast %>% 
  select(ds, playoff, superbowl) %>% 
  filter(abs(playoff + superbowl) > 0) %>%
  tail(10)

# The magnitude of the holiday effect has been reduced compared to before, 
# especially for superbowls, which had the fewest observations. There is a 
# parameter seasonality_prior_scale which similarly adjusts the extent to which 
# the seasonality model will fit the data.

# Prior scales can be set separately for individual holidays by including a 
# column prior_scale in the holidays dataframe. Prior scales for individual 
# seasonalities can be passed as an argument to add_seasonality. For instance, 
# the prior scale for just weekly seasonality can be set using:
m <- prophet()
m <- add_seasonality(
  m, name='weekly', period=7, fourier.order=3, prior.scale=0.1)


# 4 - Additional regressors -----------------------------------------------

# Additional regressors can be added to the linear part of the model using the 
# add_regressor method or function. A column with the regressor value will need 
# to be present in both the fitting and prediction dataframes. For example, we 
# can add an additional effect on Sundays during the NFL season. On the 
# components plot, this effect will show up in the ‘extra_regressors’ plot:
nfl_sunday <- function(ds) {
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  as.numeric(((weekdays(dates) == "domingo") | (weekdays(dates) == "Sunday")) & 
               (month > 8 | month < 2))
}
tb_data$nfl_sunday <- nfl_sunday(tb_data$ds)

m <- prophet()
m <- add_regressor(m, 'nfl_sunday')
m <- fit.prophet(m, tb_data)

future$nfl_sunday <- nfl_sunday(future$ds)

forecast <- predict(m, future)
prophet_plot_components(m, forecast)

# NFL Sundays could also have been handled using the “holidays” interface 
# described above, by creating a list of past and future NFL Sundays. 
# The add_regressor function provides a more general interface for defining 
# extra linear regressors, and in particular does not require that the regressor 
# be a binary indicator. Another time series could be used as a regressor, 
# although its future values would have to be known.

# The add_regressor function has optional arguments for specifying the prior 
# scale (holiday prior scale is used by default) and whether or not the 
# regressor is standardized - see the docstring with help(Prophet.add_regressor) 
# in Python and ?add_regressor in R. Note that regressors must be added prior to 
# model fitting.

# The extra regressor must be known for both the history and for future dates. 
# It thus must either be something that has known future values (such as 
# nfl_sunday), or something that has separately been forecasted elsewhere. 
# Prophet will also raise an error if the regressor is constant throughout the 
# history, since there is nothing to fit from it.

# Extra regressors are put in the linear component of the model, so the 
# underlying model is that the time series depends on the extra regressor as 
# either an additive or multiplicative factor (see the next section for 
# multiplicativity).