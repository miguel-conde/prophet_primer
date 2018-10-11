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

# Make predictions
future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Plots
plot(m, forecast)

prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)
