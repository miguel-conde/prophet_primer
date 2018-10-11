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

# Fit the model
m <- prophet(tb_data)
future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Plot
plot(m, forecast) + add_changepoints_to_plot(m)


# If the trend changes are being overfit (too much flexibility) or underfit 
# (not enough flexibility), you can adjust the strength of the sparse prior 
# using the input argument changepoint_prior_scale.

# MORE flexible:
m <- prophet(tb_data, changepoint.prior.scale = 0.5)
forecast <- predict(m, future)
plot(m, forecast) + add_changepoints_to_plot(m)

# LESS flexible:
m <- prophet(tb_data, changepoint.prior.scale = 0.001)
forecast <- predict(m, future)
plot(m, forecast) + add_changepoints_to_plot(m)

# If you wish, rather than using automatic changepoint detection you can 
# manually specify the locations of potential changepoints with the changepoints 
# argument. Slope changes will then be allowed only at these points, with the 
# same sparse regularization as before. 
#   - One could, for instance, create a grid of points as is done automatically, 
#     but then augment that grid with some specific dates that are known to be 
#     likely to have changes. 
#   - As another example, the changepoints could be entirely limited to a small 
#     set of dates, as is done here:
m <- prophet(tb_data, changepoints = c('2014-01-01'))
forecast <- predict(m, future)
plot(m, forecast)  + add_changepoints_to_plot(m)
