# АВР. Семинар 2.

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(lubridate) # куча плюшек для рядов
library(rio) # импорт данных
library(ggrepel) # симпатичные подписи
library(ggplot2) # графики
library(patchwork) # расположение графиков
library(latex2exp)


setwd('/Users/polinapogorelova/Desktop/ЭЭП_АВР') # установка рабочей директории

# Задание 1. Генерирование процессов MA(q) и AR(p)

set.seed(777)

plot_height = 15
plot_width = 20

n_obs = 24

# MA(q)-процесс
data_ma = tibble(y = arima.sim(n = n_obs, model = list(ma = 0.9)),
                 x =  arima.sim(n = n_obs, model = list(ma = 0.8, -0.2)))
data_ma
data_ma$date = yearmonth(ymd('2000-12-01') + months(1:n_obs))
data_ma

data_ma = as_tsibble(data_ma, index = date)

data_ma %>% autoplot(y) + labs(title = "Процесс MA(1)")
ggsave("graph_ma.png", dpi = 300, width = plot_width, height = plot_height, unit = 'cm')
gg_tsdisplay(data_ma, y, plot_type = 'season')
gg_tsdisplay(data_ma, y, plot_type = 'partial')

ACF(data_ma, y)
ACF(data_ma, y) %>% autoplot()

PACF(data_ma, y)
PACF(data_ma, y) %>% autoplot()

data_ma %>% autoplot(x) + labs(title = "Процесс MA(2))")
gg_tsdisplay(data_ma, x, plot_type = 'partial')

# AR(p)-процесс
set.seed(777)

data_ar = tibble(a = arima.sim(n = n_obs,
                            model = list(ar = 0.7)),
                 b = arima.sim(n = n_obs,
                            model = list(ar = 0.9)))

data_ar$year = 1:n_obs
data_ar = as_tsibble(data_ar, index = year)

gg_tsdisplay(data_ar, a, plot_type = 'partial')
gg_tsdisplay(data_ar, b, plot_type = 'partial')


# Задание 2. Данные о количестве безработных в России. Источник: https://fedstat.ru/indicator/36250

h = import('unemployments.xls')
h2 = import('unemployments.xls', skip = 2) # удаляем пустые строки в начале таблицы
head(h2)

colnames(h2)[1:3] = c('region', 'unit', 'period')
glimpse(h2)

month_dict = tibble(period = unique(h2$period), month_no = 1:12)
month_dict

h3 = left_join(h2, month_dict, by = 'period')
head(h3)

h4 = select(h3, -unit, -period)
glimpse(h4)

h5 = pivot_longer(h4, cols = `2009`:`2021`,
                  names_to = 'year',
                  values_to = 'total')

h6 = mutate(h5, date = yearmonth(paste0(year, '-', month_no)))

h7 = separate(h6,
              region,
              into = c('code', 'name'),
              sep = ' ',
              extra = 'merge')

h8 = select(h7, -month_no, -year)

unemp = as_tsibble(h8, index = date, key = c('code', 'name'))

unemp_save = mutate(unemp,
                    date = as.Date(date))

export(unemp_save, 'unemployments.csv')

unemp_rf = filter(unemp, (code == '643') & (date < ymd('2021-10-01')))
tail(unemp_rf)

unemp_rf = mutate(unemp_rf, total = total/1000)

p1 = autoplot(unemp_rf) +
  labs(x = "Дата", y = "Число безработных")

p2 <- ACF(unemp_rf, total) %>% autoplot() + labs(y = "АКФ", x = NULL)
p3 <- PACF(unemp_rf, total) %>% autoplot() + labs(y = "АКФ", x = NULL)

(p1 + p2) + plot_annotation(title = "График динамики безработных в России  (тыс. чел.) и АКФ")
(p1 / (p2 + p3)) + plot_annotation(title = "График динамики безработных в России  (тыс. чел.) и АКФ")

gg_tsdisplay(unemp_rf, total, plot_type = 'season')
