# АВР. Семинар 5.

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(lubridate) # куча плюшек для рядов
library(rio) # импорт данных
library(ggrepel) # симпатичные подписи
library(ggplot2) # графики
library(patchwork) # расположение графиков
library(rvest)

setwd('/Users/polinapogorelova/Desktop/ЭЭП_АВР') # установка рабочей директории

# Задание 1. Данные о числе свадеб в России. Источник: https://fedstat.ru/indicator/33553
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

h8 = select(h7, -month_no, -year)

h9 = separate(h8,
              region,
              into = c('code', 'name'),
              sep = ' ',
              extra = 'merge')


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
               ar1 = ARIMA(sum ~ pdq(1, 0, 0) + PDQ(0,0,0)),
               ma2 = ARIMA(sum ~ pdq(0, 0, 2) + PDQ(0,0,0)) ,
               arma11 = ARIMA(sum ~ pdq(1, 0, 1) + PDQ(0,0,0)),
               arma = ARIMA(sum))

models
report(models$arma11[[1]])

fcst = forecast(models, test)
fcst = forecast(models, h = 1)

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
