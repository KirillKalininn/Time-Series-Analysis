install.packages("devtools") # позволяет устанавливать и собирать пакеты с GitHub
devtools::install_github("SteffenMoritz/imputeTS")
install.packages("imputeTS")

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(lubridate) # куча плюшек для рядов
library(rio) # импорт данных
library(ggrepel) # симпатичные подписи
library(ggplot2) # графики
library(patchwork) # расположение графиков
library(latex2exp) # красивые формулы
library(imputeTS) # заполнение пропусков в данных

setwd('/Users/polinapogorelova/Desktop/ЭЭП_АВР') # установка рабочей директории

# Задание 1. Заполнение пропусков на примере ежедневных данных о стоимости акций компании Apple
gafa_stock

app_price = filter(gafa_stock, Symbol == "AAPL",
                   year(Date) >= 2017)


app_price$Date = ymd(app_price$Date)

app_price = as_tsibble(app_price, index = Date, key = Symbol)

# Заполнение пропусков
has_gaps(app_price)

app_price %>% scan_gaps()

app_price_gaps = app_price %>%
  fill_gaps()

app_price_na = app_price_gaps %>%
  group_by_key() %>%
  mutate(Close = na_locf(Close))

autoplot(app_price)
autoplot(app_price_na, Close) +
  labs(title = "Динамика цены акций компании Apple", y = "")



# Задание 2.

n_obs = 2400
# MA(q)-процесс
data_ma = tibble(y = arima.sim(n = n_obs, model = list(ma = 0.9)),
                 x = arima.sim(n = n_obs, model = list(ma = c(0.8, -0.2))))
data_ma

data_ma$date = yearmonth(ymd('2000-12-01') + months(1:n_obs))

data_ma = as_tsibble(data_ma, index = date)

data_ma %>% autoplot(y) + labs(title = "Процесс MA(1)")
data_ma %>% autoplot(x) + labs(title = "Процесс MA(2)")

data_ma %>% autoplot(x) + labs(title = "Процесс MA(2)")
gg_tsdisplay(data_ma, x, plot_type = 'partial')

# AR(p)-процесс
set.seed(777)

data_ar = tibble(a = arima.sim(n = n_obs,
                               model = list(ar = 0.7)),
                 b = arima.sim(n = n_obs,
                               model = list(ar = c(0.6, 0.3))))

data_ar$year = 1:n_obs
data_ar = as_tsibble(data_ar, index = year)

gg_tsdisplay(data_ar, a, plot_type = 'partial')
gg_tsdisplay(data_ar, b, plot_type = 'partial')

# ARMA(p,q)-процесс
set.seed(777)

data_arma = tibble(arma11 = arima.sim(n = n_obs,
                               model = list(ar = 0.7, ma = -0.4)),
                   arma21 = arima.sim(n = n_obs,
                               model = list(ar = c(1.3, -0.4), ma = 0.7)))

data_arma$year = 1:n_obs
data_arma = as_tsibble(data_arma, index = year)

gg_tsdisplay(data_arma, arma11, plot_type = 'partial')
gg_tsdisplay(data_arma, arma21, plot_type = 'partial')
