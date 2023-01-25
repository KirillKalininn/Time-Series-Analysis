install.packages("tidyverse")
install.packages("fpp3")
install.packages("rio")
install.packages("ggrepel")
install.packages("ggplot2")
install.packages("patchwork")

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(ggrepel) # симпатичные подписи
library(ggplot2) # графики
library(patchwork) # расположение графиков

setwd('/Users/polinapogorelova/Desktop/ЭЭП_АВР') # установка рабочей директории

# Задание 1.
# Создаём ряд и строим графики.

n_obs = 100

set.seed(1000) # для воспроизводимости результатов

y = tsibble(sample = 1:n_obs, wn = rnorm(n_obs), index = sample) # белый шум

y %>%
  autoplot(wn) + labs(title = "Белый шум", y = "")

y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "Белый шум")


data = tsibble(date = yearmonth(ymd('2013-01-01') + months(0:(n_obs - 1))),
               iid = rnorm(n_obs, mean = 10, sd = 4), # модель с независимыми наблюдениями y_t = mu + eps_t
               rwalk = 15 + cumsum(rnorm(n_obs, mean = 0, sd = 2)), # модель случайного блуждания
               index = date)
data

p1 = autoplot(data, iid)
p2 = autoplot(data, rwalk)

p1 + p2 + plot_annotation(title = "Процесс с независимыми наблюдениями и процесс случайного блуждания")

p1 / p2 + plot_annotation(title = "Процесс с независимыми наблюдениями и процесс случайного блуждания")

(p1 + p1) / (p2 + p2) + plot_annotation(title = "Процесс с независимыми наблюдениями и процесс случайного блуждания")
