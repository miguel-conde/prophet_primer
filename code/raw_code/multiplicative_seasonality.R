library(here)
library(prophet)
library(tidyverse)

if(!file.exists(here("data", "tidy_data", "example_air_passengers.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_air_passengers.csv",
    destfile = here("data", "tidy_data", "example_air_passengers.csv")
  )
}

tb_data <- read.csv(here("data", "tidy_data", "example_air_passengers.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.Date(ds))

# an example of when additive seasonality does not work:
m <- prophet(tb_data)
future <- make_future_dataframe(m, 50, freq = 'm')
forecast <- predict(m, future)
plot(m, forecast)

# This time series has a clear yearly cycle, but the seasonality in the forecast 
# is too large at the start of the time series and too small at the end. In this 
# time series, the seasonality is not a constant additive factor as assumed by 
# Prophet, rather it grows with the trend. This is multiplicative seasonality.

# Prophet can model multiplicative seasonality by setting 
# seasonality_mode='multiplicative' in the input arguments:
m <- prophet(tb_data, seasonality.mode = 'multiplicative')
forecast <- predict(m, future)
plot(m, forecast)

# The components figure will now show the seasonality as a percent of the trend:
prophet_plot_components(m, forecast)

# With seasonality_mode='multiplicative', holiday effects will also be modeled 
# as multiplicative. Any added seasonalities or extra regressors will by default 
# use whatever seasonality_mode is set to, but can be overriden by specifying 
# mode='additive' or mode='multiplicative' as an argument when adding the 
# seasonality or regressor.

# For example, this block sets the built-in seasonalities to multiplicative, 
# but includes an additive quarterly seasonality and an additive regressor:
m <- prophet(seasonality.mode = 'multiplicative')
m <- add_seasonality(m, 'quarterly', period = 91.25, fourier.order = 8, mode = 'additive')
# m <- add_regressor(m, 'regressor', mode = 'additive')

m <- fit.prophet(m, tb_data)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)
