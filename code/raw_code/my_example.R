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


# CORES -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(prophet)
library(readxl)

URL_CORES <- "https://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp.xlsx"

get_cores_data <- function(sheet = "Gasolinas") {
  #
  # Esta función se baja de la web de CORES el archivo excel con los consumos 
  # nacionales mensuales de productos petrolíferos
  #
  # PARAMETERS
  #   - sheet: char("Gasolinas", "Gasoleos") - hoja del libro excel cuyos datos 
  #            se desea obtener
  #
  # RETURN
  #   - Tibble con <fecha> (ajustada al dia fina de mes) y una columna para el 
  #     consumo mensual (en Tn) por producto 
  
  destfile <- paste0(tempfile(), ".xlsx")
  
  curl::curl_download(URL_CORES, destfile)
  
  consumos_pp <- read_excel(destfile, sheet = sheet, skip = 5) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(mes)) %>% 
    filter(mes != "total") %>% 
    mutate(dia = 1) %>% 
    unite(fecha, c("ano", "mes", "dia"), sep = "-") %>% 
    mutate(fecha = ymd(fecha))
  
  unlink(destfile)
  
  day(consumos_pp$fecha)  <-  days_in_month(consumos_pp$fecha)
  
  consumos_pp
}

tb_gna95 <- get_cores_data() %>% 
  select(fecha, gasolina_95) %>% 
  rename(ds = fecha,
         y = gasolina_95)

tb_goa   <- get_cores_data("Gasoleos") %>% 
  select(fecha, gasoleo_a) %>% 
  rename(ds = fecha,
         y = gasoleo_a)

# Fit the model
m <- prophet(tb_goa)

# m <- prophet(daily.seasonality = FALSE,
#              weekly.seasonality = FALSE)
# m <- add_seasonality(m, name='monthly', period=30.5, fourier.order=5)
# m <- fit.prophet(m, tb_goa)

# Make predictions
future <- make_future_dataframe(m, periods = 24, freq = 'month')
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Plots
plot(m, forecast)

prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)



### PERF
tb_data_cv <- cross_validation(m, 
                               initial = 365.25*4, 
                               period = 365.25, 
                               horizon = 365.25*2, 
                               units = 'days')
head(tb_data_cv)
tb_data_cv %>% select(cutoff) %>% unique
tb_data_cv %>% group_by(cutoff) %>% summarise(n = n()) 

tb_data_p <- performance_metrics(tb_data_cv)
head(tb_data_p)

tb_data_p_by_h <- tb_data_p %>% 
  group_by(horizon) %>% 
  summarise_all(mean)

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

