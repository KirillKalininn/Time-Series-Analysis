# Семинар 15. Причинность по Грэйнджеру для нестационраных рядов: Toda-Yamamoto approach
library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов (tsibble, mutate)
library(rio) # импорт данных
library(lmtest) # тесты (включая тест на причинность)
library(urca) # тест на стационарность, коинтеграцию
library(patchwork)
library(ggplot2)
library(fpp2)
library(vars)
library(ARDL)
library(tsDyn)


data = Coffee_Prices
data
n_obs = nrow(data)
d = as_tibble(data)
d = mutate(d, date =  yearmonth(ymd('1960-01-01') + months(0:(n_obs - 1))))
d = as_tsibble(d, index = date)

d %>% ggplot(aes(x = date)) +
  geom_line(aes(y = Arabica, colour = "Arabica")) +
  geom_line(aes(y = Robusta, colour = "Robusta")) +
  scale_colour_manual(
    values = c(Arabica = "blue", Robusta = "black")
  ) +
  labs(y = "",
       title = "Динамика цен на кофе Арабика и Робуста") +
  guides(colour = guide_legend(title = "Сорт кофе"))

d = filter(d, date >= ymd("1976-01-01"))
d = filter(d, date < ymd("2011-04-01"))

d %>% ggplot(aes(x = date)) +
  geom_line(aes(y = Arabica, colour = "Arabica")) +
  geom_line(aes(y = Robusta, colour = "Robusta")) +
  scale_colour_manual(
    values = c(Arabica = "blue", Robusta = "black")
  ) +
  labs(y = "",
       title = "Динамика цен на кофе Арабика и Робуста") +
  guides(colour = guide_legend(title = "Сорт кофе"))

# Шаг 1. Подбор порядка интегрированности рядов (проверка на стационарность)
summary(ur.df(d$Arabica))
summary(ur.df(d$Robusta))

summary(ur.kpss(d$Arabica))
summary(ur.kpss(d$Robusta))

summary(ur.df(diff(d$Arabica,1)))
summary(ur.df(diff(d$Robusta,1)))

summary(ur.kpss(diff(d$Arabica,1)))
summary(ur.kpss(diff(d$Robusta,1)))

# Оба ряда I(1)

# Шаг 2. m = 1 - выбрали максимальный порядок интегрированности рядов

# Шаги 3-4.Построим VAR в уровнях, не обращая внимания на порядок интегрированности рядов. Выберем оптимальный порядок p для VAR модели,
# используя информационные критерии
VARselect(d[,2:1], lag = 20, type = "both")
var2_model = VAR(d[,1:2], p = 2, type = "both")
var6_model = VAR(d[,1:2], p = 6, type = "both")

# Шаг 5.Проведем диагностику VAR. Убедимся, что нет автокорреляции остатков и модель динамически устойчива
serial.test(var2_model)
serial.test(var6_model)

roots(var6_model)[[1]] # "<1"
roots(var6_model)[[2]] # "<1"

# Шаг 6. Если две или больше рядов имеют одинаковый порядок интеграции, то проверяем наличие коинтегрированности,
# предпочтительно использование методологии Йохансена (для построенного VAR)
jo_test_trace = ca.jo(d[,1:2], type = "trace", ecdet = "trend", K = 6)
summary(jo_test_trace)
jo_test_max = ca.jo(d[,1:2], type = "eigen", ecdet = "trend", K = 6)
summary(jo_test_max)

# r = 1

# Шаг 7.Теперь увеличиваем порядок отобранной VAR модели на m = 1, добавляя лаги каждой переменной в каждом уравнении.
var7_model = VAR(d[, 1:2], p = 7, type = "both")
var7_model$varresult

summary(var7_model)

# Тестирование причинности по Грэйнджеру
# H0: цена на кофе Робуста не является причиной по Грэйнджеру для цены на кофе Арабика
wald.test(b = coef(var7_model$varresult[[2]]), Sigma = vcov(var7_model$varresult[[2]]), Terms = c(1,3,5,7,9,11))

# H0: цена на кофе Арабика не является причиной по Грэйнджеру для цены на кофе Робуста
wald.test(b = coef(var7_model$varresult[[1]]), Sigma = vcov(var7_model$varresult[[1]]), Terms = c(2,4,6,8,10,12))
