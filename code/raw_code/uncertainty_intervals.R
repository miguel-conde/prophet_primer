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


# Uncertainty in the trend ------------------------------------------------


# The width of the uncertainty intervals (by default 80%) can be set using the 
# parameter interval_width
# Fit the model
m <- prophet(tb_data, interval.width = 0.95)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future))

# These intervals assume that the future will see the same frequency and 
# magnitude of rate changes as the past. This assumption is probably not true, 
# so you should not expect to get accurate coverage on these uncertainty 
# intervals.
# One property of this way of measuring uncertainty is that allowing higher 
# flexibility in the rate, by increasing changepoint_prior_scale, will increase 
# the forecast uncertainty. This is because if we model more rate changes in the 
# history then we will expect more in the future, and makes the uncertainty 
# intervals a useful indicator of overfitting.


# Uncertainty in seasonality ----------------------------------------------

# By default Prophet will only return uncertainty in the trend and observation 
# noise. To get uncertainty in seasonality, you must do full Bayesian sampling. 
# This is done using the parameter mcmc.samples (which defaults to 0). 
m <- prophet(tb_data, mcmc.samples = 300)
forecast <- predict(m, future)

# This replaces the typical MAP estimation with MCMC sampling, and can take much 
# longer depending on how many observations there are - expect several minutes 
# instead of several seconds. If you do full sampling, then you will see the 
# uncertainty in seasonal components when you plot them:
prophet_plot_components(m, forecast)

# You can access the raw posterior predictive samples in R using the function 
# predictive_samples(m, future)
predictive_samples(m, future) %>%  str
