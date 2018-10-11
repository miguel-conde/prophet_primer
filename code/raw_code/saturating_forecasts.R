library(here)
library(prophet)
library(tidyverse)

if(!file.exists(here("data", "tidy_data", "example_wp_log_R.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_R.csv",
    destfile = here("data", "tidy_data", "example_wp_log_R.csv")
  )
}

tb_data <- read.csv(here("data", "tidy_data", "example_wp_log_R.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.Date(ds))

# We must specify the carrying capacity in a column cap. Here we will assume a 
# particular value, but this would usually be set using data or expertise about 
# the market size.
tb_data$cap <- 8.5

m <- prophet(tb_data, growth = 'logistic')
future <- make_future_dataframe(m, periods = 1826)
future$cap <- 8.5
fcst <- predict(m, future)
plot(m, fcst)

# The logistic growth model can also handle a saturating minimum, which is 
# specified with a column floor in the same way as the cap column specifies 
# the maximum:
tb_data$y <- 10 - tb_data$y
tb_data$cap <- 6
tb_data$floor <- 1.5
future$cap <- 6
future$floor <- 1.5
m <- prophet(tb_data, growth = 'logistic')
fcst <- predict(m, future)
plot(m, fcst)

