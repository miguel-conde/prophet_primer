library(here)
library(prophet)
library(tidyverse)


# load(here("data", "tidy_data", "BBDD_Germany_20180619.RData"))
# BBDD_Germany <- BBDD_Germany %>% as.tibble()
# 
# tb_data <- BBDD_Germany %>% 
#   transmute(ds = fecha, 
#             y = Reservas_Ocio_Berlin) 

load(here("data", "tidy_data", "all_V3.RData"))
rm(xts_daily_base, tb_weekly_base, xts_weekly_base); gc()

tb_data <- tb_daily_base %>% 
  select(Fecha, trafico_web_sesiones_totales,
         contains("sales"), 
         goog_trends, temp_mn, otros_precio_medio, 
         eco_sent, unemp_rate,
         tiendas_numero_de_tiendas, 
         winter_sales, summer_sales,
         w1_winter_sales, w1_summer_sales,
         black_friday_2016, black_friday_2017) %>% 
  rename(ds = Fecha, 
            y = trafico_web_sesiones_totales) 


# QUICK -------------------------------------------------------------------


# Fit the model
m <- prophet(tb_data)

# CV
# tb_data_cv <- cross_validation(m, initial = 53, period = 4, horizon = 52, units = 'weeks')
# tb_data_cv <- cross_validation(m, horizon = 30, units = 'days')
tb_data_cv <- cross_validation(m, initial = 365, horizon = 30, units = 'days')
head(tb_data_cv)

tb_data_cv %>% select(cutoff) %>% unique
tb_data_cv %>% group_by(cutoff) %>% summarise(n = n()) 

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

plot_cross_validation_metric(tb_data_cv, metric = 'mape')

# Make predictions
future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

# Plots
plot(m, forecast)

prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)


# TREND CHANGEPOINTS ------------------------------------------------------
plot(m, forecast) + add_changepoints_to_plot(m)

# MORE flexibility
m <- prophet(tb_data, changepoint.prior.scale = 0.5)

tb_data_cv <- cross_validation(m, initial = 365, horizon = 30, units = 'days')
head(tb_data_cv)

tb_data_cv %>% select(cutoff) %>% unique
tb_data_cv %>% group_by(cutoff) %>% summarise(n = n()) 

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

plot_cross_validation_metric(tb_data_cv, metric = 'mape')

forecast <- predict(m, future)
plot(m, forecast) + add_changepoints_to_plot(m)


# SEASONALITIES -----------------------------------------------------------

# m <- prophet(weekly.seasonality=FALSE)
m <- prophet()
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- fit.prophet(m, tb_data)

tb_data_cv <- cross_validation(m, initial = 365, horizon = 30, units = 'days')
head(tb_data_cv)

tb_data_cv %>% select(cutoff) %>% unique
tb_data_cv %>% group_by(cutoff) %>% summarise(n = n()) 

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

plot_cross_validation_metric(tb_data_cv, metric = 'mape')



forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)


# REGRESSORS --------------------------------------------------------------

m <- prophet()
# m <- add_seasonality(m, name='monthly_1', period=365, fourier.order=10)
# m <- add_seasonality(m, name='monthly_2', period=30.5, fourier.order=10)
m <- add_regressor(m, "goog_trends")
m <- add_regressor(m, "temp_mn")
m <- add_regressor(m, "otros_precio_medio")
m <- add_regressor(m, "unemp_rate")
m <- add_regressor(m, "tiendas_numero_de_tiendas")
m <- add_regressor(m, "w1_winter_sales")
m <- add_regressor(m, "w1_summer_sales")
# m <- add_regressor(m, "winter_sales")
# m <- add_regressor(m, "summer_sales")
m <- add_regressor(m, "black_friday_2016")
m <- add_regressor(m, "black_friday_2017")

m <- fit.prophet(m, tb_data)

tb_data_cv <- cross_validation(m, initial = 365, horizon = 30, units = 'days')
head(tb_data_cv)

tb_data_cv %>% select(cutoff) %>% unique
tb_data_cv %>% group_by(cutoff) %>% summarise(n = n()) 

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

plot_cross_validation_metric(tb_data_cv, metric = 'mape')

forecast <- predict(m, tb_data)
plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)

aux <- forecast %>% select(ds, yhat) %>% 
  left_join(tb_data %>% select(ds, y) %>% 
              mutate(ds = as.POSIXct(ds))) %>% 
  mutate(residuals = y - yhat)

Metrics::mape(aux$y, aux$yhat)
r2 <- (1 - crossprod(aux$residuals) / crossprod(aux$y - mean(aux$y))) %>% 
  as.numeric()
r2


# ALL ---------------------------------------------------------------------

m <- prophet(changepoint.prior.scale = 0.5)
m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
m <- add_regressor(m, "goog_trends")
m <- add_regressor(m, "temp_mn")
m <- add_regressor(m, "otros_precio_medio")
m <- add_regressor(m, "unemp_rate")
m <- add_regressor(m, "tiendas_numero_de_tiendas")
m <- add_regressor(m, "w1_winter_sales")
m <- add_regressor(m, "w1_summer_sales")
m <- add_regressor(m, "black_friday_2016")
m <- add_regressor(m, "black_friday_2017")

m <- fit.prophet(m, tb_data)

tb_data_cv <- cross_validation(m, initial = 365, horizon = 30, units = 'days')
head(tb_data_cv)

tb_data_cv %>% select(cutoff) %>% unique
tb_data_cv %>% group_by(cutoff) %>% summarise(n = n()) 

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

plot_cross_validation_metric(tb_data_cv, metric = 'mape')

forecast <- predict(m, tb_data)
plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)

aux <- forecast %>% select(ds, yhat) %>% 
  left_join(tb_data %>% select(ds, y) %>% 
              mutate(ds = as.POSIXct(ds))) %>% 
  mutate(residuals = y - yhat)

Metrics::mape(aux$y, aux$yhat)
r2 <- (1 - crossprod(aux$residuals) / crossprod(aux$y - mean(aux$y))) %>% 
  as.numeric()
r2


# CONTRIBUTIONS -----------------------------------------------------------

m_aportes <- forecast %>% as.tibble() %>% 
  select(-ends_with("_lower"), -ends_with("_upper"), -ends_with("_terms"), 
         -starts_with("extra_regressors_"), -yhat) 

Conento::aportes_ag(m_aportes)
Conento::dibuja_areas(m_aportes %>% mutate(ds = as.Date(ds)))
