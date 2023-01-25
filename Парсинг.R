# АВР. Семинар 4.

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(lubridate) # куча плюшек для рядов
library(rio) # импорт данных
library(ggrepel) # симпатичные подписи
library(ggplot2) # графики
library(patchwork) # расположение графиков
library(rvest)

setwd('/Users/polinapogorelova/Desktop/ЭЭП_АВР') # установка рабочей директории

# Задание 1. ARMA-модель

# ARMA(p,q)-процесс

# Парсинг данных с сайте Sophist
url = 'http://sophist.hse.ru/hse/1/tables/POPNUM_Y.htm'
xml_tree = read_html(url)

p1 = html_table(xml_tree)
p1 = html_table(xml_tree)[[1]]
colnames(p1) = c('year', 'total')
p2 = tail(p1, -2)
p3 = head(p2, -4)
p4 = mutate(p3, year = as.numeric(year), total = as.numeric(total)/1000)

popnum = as_tsibble(p4, index = 'year')
autoplot(popnum, total)

train = filter(popnum, year < 2019)
test = filter(popnum, year >= 2019)

models = model(train,
               naive = NAIVE(total),
               ar1 = ARIMA(total ~ pdq(1, 0, 0)),
               ma1 = ARIMA(total ~ pdq(0, 0, 1)),
               arma11 = ARIMA(total ~ pdq(1, 0, 1)),
               arma = ARIMA(total ~ 1))
models
report(models$ar1[[1]])

fcst = forecast(models, h = 3)

glance(models) %>%
  arrange(AICc) %>%
  select(.model:BIC)

accuracy(models) %>%
  arrange(RMSE)

augment(models) %>%
  filter(.model == 'arma') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

autoplot(train) +
  autolayer(fcst) +
  autolayer(test) +
  labs(titale = "Прогноз численности населения РФ")


# Задание 2. Данные о числе свадеб в России. Источник: https://fedstat.ru/indicator/33553
h = import('marriages.xls')

h2 = import('marriages.xls', skip = 2)

colnames(h2)[1:3] = c('region', 'unit', 'period')

nchar(unique(h2$period))

h3 = filter(h2, nchar(period) < 13)
unique(h3$period)

month_dict = tibble(period = unique(h3$period),
                    month_no = 1:12)
month_dict

h4 = left_join(h3, month_dict, by = 'period')

h5 = select(h4, -unit, -period)
glimpse(h5)

h6 = pivot_longer(h5, cols = `2006`:`2021`,
                  names_to = 'year',
                  values_to = 'total')

h7 = mutate(h6, date = yearmonth(paste0(year, '-', month_no)))

h8 = separate(h7,
              region,
              into = c('code', 'name'),
              sep = ' ',
              extra = 'merge')

h9 = select(h8, -month_no, -year)

marriages = as_tsibble(h9, index = date, key = c('code', 'name'))

marr_save = mutate(marriages,
                        date = as.Date(date))

export(marr_save, 'marriages.csv')





m = import('marriages.csv')

m2 = mutate(m, year = year(date))

m3 = select(m2, -date)

m_agg = group_by(m3, code, name, year) %>%
        summarise(sum = sum(total))

marr_rf = filter(m_agg, code == 643)

marr_rf = as_tsibble(marr_rf, index = year)

marr_rf %>% autoplot(sum)

gg_tsdisplay(marr_rf, sum, plot_type = 'partial')

train = filter(marr_rf,
               year < 2018)
test = filter(marr_rf,
              year >= 2018)

models = model(train,
               naive = NAIVE(sum),
               ar1 = ARIMA(sum ~ pdq(1, 0, 0)),
               ma1 = ARIMA(sum ~ pdq(0, 0, 1)),
               arma11 = ARIMA(sum ~ pdq(1, 0, 1)),
               arma = ARIMA(sum))

models
report(models$ar1[[1]])

fcst = forecast(models, test)

accuracy(fcst, marr_rf) %>%
  arrange(RMSE)

autoplot(fcst) +
  autolayer(train) +
  autolayer(test)

best_model = models = model(train,
                            naive = NAIVE(sum))

augment(best_model) %>%
  features(.innov, ljung_box, lag = 10, dof = 0)

augment(best_model) %>%
  gg_tsdisplay(.innov)
