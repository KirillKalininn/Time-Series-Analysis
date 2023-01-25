# Семинар 12. VAR(p) модели
library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(lmtest) # тесты (включая тест на причинность)
library(urca) # тест на стационарность, коинтеграцию
library(patchwork)
library(ggplot2)
library(tseries)


# Импорт данных
d = import('/Users/polinapogorelova/Desktop/dataset_var.xlsx')
d = mutate(d, date = yearmonth(dateid01))
d = as_tsibble(d, index = date)

d1 = mutate(d, inlf = difference(lcpi,1)*100)
d2 = tail(d1,-1)
d3 = d2[, c(5,6,3)]

train = filter(d3, date < ymd('2017-01-01'))
test = filter(d3, date >= ymd('2017-01-01'))

# Графики показателей
p1 = train %>% autoplot(inlf)
p2 = train %>% autoplot(int_rate)
p1 / p2

# Тест на стационарность
summary(ur.df(train$inlf, type = "drift", selectlags = "AIC"))
summary(ur.df(train$int_rate, type = "drift", selectlags = "AIC"))

# Оценивание VAR(p) модели
fit = train %>%
  model(
    aicc = VAR(vars(inlf, int_rate), ic = "aicc"),
    bic = VAR(vars(inlf, int_rate), ic = "bic")
  )
fit

glance(fit)

# Диагностика VAR-модели
fit %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

# Прогнозирование с помощью VAR модели
forecast = fit %>%
  select(aicc) %>%
  forecast(h = 12)
forecast

fit %>%
  select(aicc) %>%
  forecast() %>%
  autoplot(d3 %>% filter(year(date) > 2010))

# Причинность по Грейнджеру
# H0: inlf не является причиной для int_rate
grangertest(train$int_rate ~ train$inlf, order = 2, data = d3)

# H0: int_rate не является причиной для inlf
grangertest(train$inlf ~ train$int_rate, order = 2, data = d3)

# Функция отклика импульса
int_rate = ts(d3$int_rate, start = (1999-02-01), frequency = 12)
inlf = ts(d3$inlf, start = (1999-02-01), frequency = 12)

series = subset(train, select = c("inlf", "int_rate"))

var4 = vars::VAR(series, p = 4, type = "const")
summary(var4)


int_rate_irf = vars::irf(var4, impulse = "int_rate", response = "int_rate", n.ahead = 20, boot = TRUE)
plot(int_rate_irf, ylab = "int_rate", main = "Interest rate's shock to Interest rate")

inlf_irf = vars::irf(var4, impulse = "int_rate", response = "inlf", n.ahead = 20, boot = TRUE)
plot(inlf_irf, ylab = "int_rate", main = "Interest rate's shock to Inflation")
