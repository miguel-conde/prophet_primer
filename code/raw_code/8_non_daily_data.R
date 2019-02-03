library(here)
library(prophet)
library(tidyverse)


# Sub-daily data ----------------------------------------------------------

# Prophet can make forecasts for time series with sub-daily observations by 
# passing in a dataframe with timestamps in the ds column. The format of the 
# timestamps should be YYYY-MM-DD HH:MM:SS - see the example csv here. When 
# sub-daily data are used, daily seasonality will automatically be fit. Here we 
# fit Prophet to data with 5-minute resolution (daily temperatures at Yosemite):

if(!file.exists(here("data", "tidy_data", "example_yosemite_temps.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_yosemite_temps.csv",
    destfile = here("data", "tidy_data", "example_yosemite_temps.csv")
  )
}

tb_data <- read.csv(here("data", "tidy_data", "example_yosemite_temps.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.POSIXct(ds, tz="GMT"))

m <- prophet(tb_data, changepoint.prior.scale=0.01)
future <- make_future_dataframe(m, periods = 300, freq = 60 * 60)
fcst <- predict(m, future)
plot(m, fcst)

# The daily seasonality will show up in the components plot:
prophet_plot_components(m, fcst)


# Data with regular gaps --------------------------------------------------

# Suppose the dataset above only had observations from 12a to 6a:
tb_data_2 <- tb_data %>%
  filter(as.numeric(format(tb_data$ds, "%H")) < 6)
m <- prophet(tb_data)
future <- make_future_dataframe(m, periods = 300, freq = 60 * 60)
fcst <- predict(m, future)
plot(m, fcst)

# The forecast seems quite poor, with much larger fluctuations in the future 
# than were seen in the history. The issue here is that we have fit a daily 
# cycle to a time series that only has data for part of the day (12a to 6a). 
# The daily seasonality is thus unconstrained for the remainder of the day and 
# is not estimated well. The solution is to only make predictions for the time 
# windows for which there are historical data. Here, that means to limit the 
# future dataframe to have times from 12a to 6a:
future_2 <- future %>% 
  filter(as.numeric(format(ds, "%H")) < 6)
fcst <- predict(m, future_2)
plot(m, fcst)

# The same principle applies to other datasets with regular gaps in the data. 
# For example, if the history contains only weekdays, then predictions should 
# only be made for weekdays since the weekly seasonality will not be well 
# estimated for the weekends.


# Monthly data ------------------------------------------------------------

# You can use Prophet to fit monthly data. However, the underlying model is 
# continuous-time, which means that you can get strange results if you fit the 
# model to monthly data and then ask for daily forecasts. Here we forecast US 
# retail sales volume for the next 10 years:
if(!file.exists(here("data", "tidy_data", "example_retail_sales.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_retail_sales.csv",
    destfile = here("data", "tidy_data", "example_retail_sales.csv")
  )
}

tb_data <- read.csv(here("data", "tidy_data", "example_retail_sales.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.POSIXct(ds, tz="GMT"))

m <- prophet(tb_data, seasonality.mode = 'multiplicative')
future <- make_future_dataframe(m, periods = 3652)
fcst <- predict(m, future)
plot(m, fcst)

# This is the same issue from above where the dataset has regular gaps. When we 
# fit the yearly seasonality, it only has data for the first of each month and 
# the seasonality components for the remaining days are unidentifiable and 
# overfit. This can be clearly seen by doing MCMC to see uncertainty in the 
# seasonality
m <- prophet(tb_data, seasonality.mode = 'multiplicative', mcmc.samples = 300)
fcst <- predict(m, future)
prophet_plot_components(m, fcst)

# The seasonality has low uncertainty at the start of each month where there are 
# data points, but has very high posterior variance in between. When fitting 
# Prophet to monthly data, only make monthly forecasts, which can be done by 
# passing the frequency into make_future_dataframe:
future <- make_future_dataframe(m, periods = 120, freq = 'month')
fcst <- predict(m, future)
plot(m, fcst)
