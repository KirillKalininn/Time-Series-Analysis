# Семинар 13. VAR(p) модели
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

d1 = mutate(d, infl = difference(lcpi,1)*100)
d2 = tail(d1,-1)
d3 = d2[, c(5,6,3)]

train = filter(d3, date < ymd('2017-01-01'))
test = filter(d3, date >= ymd('2017-01-01'))

# Графики показателей
p1 = train %>% autoplot(infl)
p2 = train %>% autoplot(int_rate)
p1 / p2

# Тест на стационарность - DF GLS
summary(ur.ers(train$infl, type = "DF-GLS",  model = "const", lag.max = 12))
summary(ur.ers(train$int_rate,  type = "DF-GLS",  model = "const", lag.max = 12))


# Оценивание VAR(p) модели
VARselect(train[,2:3], lag.max = 5, type = "const")[["selection"]]
var_model1 = VAR(train[,2:3], p = 2, type = "const")

summary(var_model1)

serial.test(var_model1, lags.pt = 12, type = "PT.asymptotic")
serial.test(var_model1, lags.bg = 4, type = "BG")

res1 = residuals(var_model1)
res1 = as_tibble(res1)

ggAcf(res1$infl) + ggtitle("ACF of residuals")
ggAcf(res1$int_rate) + ggtitle("ACF of residuals")


# В остатках осталась значимая автокорреляция, следовательно, надо рассмотреть альтернативные модели
train$dummy1 = 0
train$dummy1[35] = 1
train$dummy1[120] = 1
train$dummy1[191] = 1

train$dummy2 = 0
train$dummy2[190:194] = 1

VARselect(train[,2:3], lag.max = 12, type = "const", exogen = train[,4:5], season = 12)[["selection"]]
var_model2 = VAR(train[,2:3], p = 2, type = "const", exogen = train[,4:5])

summary(var_model2)

serial.test(var_model2, lags.pt = 12, type = "PT.asymptotic")
serial.test(var_model2, lags.bg = 12, type = "BG")

res2 = residuals(var_model2)
res2 = as_tibble(res2)

ggAcf(res2$infl) + ggtitle("ACF of residuals")
ggAcf(res2$int_rate) + ggtitle("ACF of residuals")

int_rate_irf = vars::irf(var_model1, impulse = "int_rate", response = "int_rate", n.ahead = 20, boot = TRUE, ortho = FALSE)
plot(int_rate_irf, ylab = "int_rate", main = "Interest rate's shock to Interest rate")

inlf_irf = vars::irf(var_model1, impulse = "int_rate", response = "inlf", n.ahead = 20, boot = TRUE)
plot(inlf_irf, ylab = "int_rate", main = "Interest rate's shock to Inflation")

# Проанализируем долю дисперсии в ошибке прогноза
# Декомпозиция дисперсий представляет собой составляющие дисперсии ошибки прогноза исследуемой эндогенной переменной, обусловленные
# шоком остальных эндогенных переменных, т.е. вклад каждой из этих переменных в дисперсию прогноза исследуемого показателя
var4fevd = fevd(var_model1, n.ahead = 12, ortho = TRUE)
var4fevd$int_rate
plot(var4fevd)


# Процедура Йохансена (на симулированных данных)
set.seed(123)
n_obs = 10000
rw = 15 + cumsum(rnorm(n_obs, mean = 0, sd = 8))
data = tsibble(date = yearmonth(ymd('2013-01-01') + months(0:(n_obs - 1))),
               a = 0.4*rw + rnorm(10000),
               b = 0.7*rw + rnorm(10000),
               c = 0.2*rw + rnorm(10000),
               index = date)
data

p1 = autoplot(data,a)
p2 = autoplot(data,b)
p3 = autoplot(data,c)
p1 / p2 / p3

jo_test = ca.jo(data[,2:4], type = "trace", K = 3, ecdet = "const", spec = "longrun")
summary(jo_test)


s1 = 1.000*data$a + 8.4857606*data$b - 31.6928889*data$c - 0.3310744
plot(s1, type = "l")
# Проверим сгенерированную комбинацию на стационарность с помощью DF-GLS теста
summary(ur.ers(s1))

s2 = 1.00000000*data$a - 0.53527968*data$b - 0.12702552*data$c + 0.02109574
plot(s2, type = "l")
# Проверим сгенерированную комбинацию на стационарность с помощью DF-GLS теста
summary(ur.ers(s2))

s3 = 1.000000*data$a + 3.372623*data$b + 1.258051*data$c - 447.324660
plot(s3, type = "l")
# Проверим сгенерированную комбинацию на стационарность с помощью DF-GLS теста
summary(ur.ers(s3))

# Протестируем ранг коинтеграции для наших данных

jotest = ca.jo(train[,2:3], type = "trace", K = 2, ecdet = "const", spec = "longrun")
summary(jotest)

ts1 = 1.00000000 * train$infl - 0.01707946*train$int_rate - 0.78269272
plot(ts1, type = "l")
summary(ur.ers(ts1, type = "DF-GLS", model = "const", lag.max = 4))

ts2 = 1.00000000 * train$infl - 0.6658181*train$int_rate - 3.2907798
plot(ts2, type = "l")
summary(ur.ers(ts2, type = "DF-GLS", model = "const", lag.max = 5))
