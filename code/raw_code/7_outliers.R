library(here)
library(prophet)
library(tidyverse)

if(!file.exists(here("data", "tidy_data", "example_wp_log_R_outliers1.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_R_outliers1.csv",
    destfile = here("data", "tidy_data", "example_wp_log_R_outliers1.csv")
  )
}

# There are two main ways that outliers can affect Prophet forecasts. Here we 
# make a forecast on the logged Wikipedia visits to the R page from before, but 
# with a block of bad data
tb_data <- read.csv(here("data", "tidy_data", "example_wp_log_R_outliers1.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.Date(ds))

m <- prophet(tb_data)
future <- make_future_dataframe(m, periods = 1096)
forecast <- predict(m, future)
plot(m, forecast)

# The trend forecast seems reasonable, but the uncertainty intervals seem way 
# too wide. Prophet is able to handle the outliers in the history, but only by 
# fitting them with trend changes. The uncertainty model then expects future 
# trend changes of similar magnitude.

# The best way to handle outliers is to remove them - Prophet has no problem 
# with missing data. If you set their values to NA in the history but leave the 
# dates in future, then Prophet will give you a prediction for their values.
outliers <- (as.Date(tb_data$ds) > as.Date('2010-01-01')
             & as.Date(tb_data$ds) < as.Date('2011-01-01'))
tb_data$y[outliers] = NA
m <- prophet(tb_data)
forecast <- predict(m, future)
plot(m, forecast)

# In the above example the outliers messed up the uncertainty estimation but 
# did not impact the main forecast yhat. This isn’t always the case, as in this 
# example with added outliers:
if(!file.exists(here("data", "tidy_data", "example_wp_log_R_outliers2.csv"))) {
  utils::download.file(
    url = "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_R_outliers2.csv",
    destfile = here("data", "tidy_data", "example_wp_log_R_outliers2.csv")
  )
}

# There are two main ways that outliers can affect Prophet forecasts. Here we 
# make a forecast on the logged Wikipedia visits to the R page from before, but 
# with a block of bad data
tb_data <- read.csv(here("data", "tidy_data", "example_wp_log_R_outliers2.csv")) %>% 
  as.tibble() %>% 
  mutate(ds = as.Date(ds))

m <- prophet(tb_data)
future <- make_future_dataframe(m, periods = 1096)
forecast <- predict(m, future)
plot(m, forecast)

# Here a group of extreme outliers in June 2015 mess up the seasonality 
# estimate, so their effect reverberates into the future forever. Again the 
# right approach is to remove them
outliers <- (as.Date(tb_data$ds) > as.Date('2015-06-01')
             & as.Date(tb_data$ds) < as.Date('2015-06-30'))
df$y[outliers] = NA
m <- prophet(tb_data)
forecast <- predict(m, future)
plot(m, forecast)