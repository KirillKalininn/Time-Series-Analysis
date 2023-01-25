# Семинар
library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(urca) # тесты
library(lmtest) # тесты (включая тест на причинность)
library(patchwork)
library(rvest) # импорт данных
library(ARDL) # ardl


# Сгенерируем решения
set.seed(1000) # для воспроизводимости результатов

n_obs = 10000
a = -0.4
b = 1.5

# X_t + bY_t = u_t - нестационарный процесс X_t = -bY_t + u_t
# X_t + aY_t = v_t - стационарный процесс X_t = -aY_t + v_t
data = tsibble(date = yearmonth(ymd('2013-01-01') + months(0:(n_obs - 1))),
               u = cumsum(rnorm(n_obs, mean = 0, sd = 1)), # случайное блуждание
               v = arima.sim(list(order = c(1,0,0), ar = .7), n = n_obs), # стационарный AR(1) процесс
               index = date)

data = mutate(data, x = a/(a-b)*u - b/(a-b)*v, # I(1)
              y = -1/(a-b)*u + 1/(a-b)*v) # I(1)

p1 = autoplot(data, x)
p2 = autoplot(data, y)
p1/p2

data = mutate(data, r = x + a*y)

# Что будет, если попробовать оценить a и b с помощью МНК?
model = lm(x ~ 0 + y, data = data) # оценка коэффициента a!
summary(model)

plot.ts(model$residuals)
summary(ur.df(model$residuals, type = "drift")) # стационарный ряд остатков X_t - 0.396*Y_t - коинтегрирующее соотношение

data = mutate(data, dy = difference(y), dx = difference(x), z = x - 0.396*y)
m1 = lm(dx ~ lag(z), data = data)
summary(m1)
m2 = lm(dy ~ lag(z), data = data)
summary(m2)

beta = (-0.229829/ 0.148142)*(-1)
beta


# Потребление и доходы США
d = import('/Users/polinapogorelova/Desktop/usdata.xls')
d = mutate(d, date = yearmonth(date))
d = mutate(d, quarter = yearquarter(date))
d = as_tsibble(d, index = quarter)

# Добавим логарифмированные переменные
d = mutate(d, lcons = log(consumption), linc = log(income))
p1 = autoplot(d, lcons) + labs(title = "Consumption")
p2 = autoplot(d, linc) + labs(title = "Income")
p1/p2

# Проверим стационарность рядов
# H0: ряд нестационарен
# Если ADF<DF_cr, то H1 не отвергается, то есть ряд стационарен
summary(ur.df(d$lcons, type = "drift", selectlags = "AIC"))
summary(ur.df(d$linc, type = "drift", selectlags = "AIC"))


# Оценим коинтегрирующее соотношение
model_coint = lm(lcons ~ linc, data = d)
summary(model_coint)
d = mutate(d, res_coint = model_coint$residuals)
# Проверим остатки коинтегрирующей регрессии на стационарность
autoplot(d, res_coint) +
  labs(y = "", x = "", title = "Остатки коинтегрирующей регрессии")
summary(ur.df(d$res_coint, type = "drift", selectlags = "AIC"))

library("aTSA")
coint.test(d$lcons, d$linc, nlag = 2)

# Оценим ECM-модель
# Шаг 1
d = mutate(d, dlcons = difference(lcons,1), dlinc = difference(linc,1), dlinc2 = difference(linc,2))
mod1 = lm(lcons ~ linc + lag(dlinc) + dlinc + lead(dlinc, 1), data = d) # DOLS
summary(mod1)

# Шаг 2
d1 = tail(d,-2)
d1 = head(d1,-1)
d1 = mutate(d1, error.ecm = mod1$residuals)

model_ecm = lm(dlcons ~ dlinc + lag(error.ecm), data = d1)
summary(model_ecm)
# Проверим остатки на отсутствие автокорреляции с помощью теста Дарбин-Уотсон
# H0: автокорреляции отсутствует
res = model_ecm$residuals
plot.ts(res)
dwtest(model_ecm)

AIC(model_ecm)


# VAR(p) процессы
library(tsDyn)
# 1. Сгенерирум двумерный процесс VAR(2) без константы
# Xt = 0.2Xt-1 - 0.1Yt-1 - 0.2Xt-2 + 0.15Yt-2 + eps1t
# Yt = -0.2Xt-1 - 0.15Yt-1 + 0.1Xt-2 - 0.1Yt-2 + eps2t
A = matrix(c(0.2, -0.1, -0.2, 0.15, -0.2, -0.15, 0.1, -0.1), byrow = TRUE, nrow = 2, ncol = 4)
A
#>      [,1] [,2] [,3] [,4]
#> [1,]  0.2  -0.1 -0.2  0.15
#> [2,] -0.2  -0.15  0.1 -0.1

var2 = VAR.sim(B = A, lag = 2, include = "none", n = 100)
ts.plot(var2, type = "l", col = c(1,2))

# 2. Сгенерирум двумерный процесс VAR(2) c константой
# Xt = 2 + 0.2Xt-1 - 0.1Yt-1 - 0.2Xt-2 + 0.15Yt-2 + eps1t
# Yt = -0.5 - 0.2Xt-1 - 0.15Yt-1 + 0.1Xt-2 - 0.1Yt-2 + eps2t
Ac = matrix(c(2, 0.2, -0.1, -0.2, 0.15, -0.5, -0.2, -0.15, 0.1, -0.1), byrow = TRUE, nrow = 2, ncol = 5)
Ac
#>       [,1] [,2] [,3] [,4]  [,5]
#> [1,]  2    0.2  -0.1 -0.2  0.15
#> [2,] -0.5 -0.2  -0.15  0.1 -0.1

var2с = VAR.sim(B = Ac, lag = 2, include = "const", n = 100)
ts.plot(var2с, type = "l", col = c(1,2))

# 3. Сгенерирум двумерный процесс VAR(2) c трендом
# Xt = 0.4t + 0.2Xt-1 - 0.1Yt-1 - 0.2Xt-2 + 0.15Yt-2 + eps1t
# Yt = 0.7t -0.5 - 0.2Xt-1 - 0.15Yt-1 + 0.1Xt-2 - 0.1Yt-2 + eps2t
At = matrix(c(0.4, 0.2, -0.1, -0.2, 0.15, 0.7, -0.2, -0.15, 0.1, -0.1), byrow = TRUE, nrow = 2, ncol = 5)
At
#>      [,1] [,2] [,3] [,4]  [,5]
#> [1,]  0.4 0.2  -0.1 -0.2  0.15
#> [2,]  0.7 -0.2  -0.15  0.1 -0.1

var2t = VAR.sim(B = At, lag = 2, include = "trend", n = 100)
ts.plot(var2t, type = "l", col = c(1,2))

# 4. Сгенерирум двумерный процесс VAR(2) c константой и трендом
# Xt = 5 - 0.4t + 0.2Xt-1 - 0.1Yt-1 - 0.2Xt-2 + 0.15Yt-2 + eps1t
# Yt = 3 - 0.6t -0.5 - 0.2Xt-1 - 0.15Yt-1 + 0.1Xt-2 - 0.1Yt-2 + eps2t
Act = matrix(c(5, -0.4, 0.2, -0.1, -0.2, 0.15, 3, -0.6, -0.2, -0.15, 0.1, -0.1), byrow = TRUE, nrow = 2, ncol = 6)
Act
#>    [,1] [,2] [,3] [,4]  [,5]   [,6]
#> [1,] 5 -0.4  0.2  -0.1   -0.2  0.15
#> [2,] 3  -0.6 -0.2  -0.15   0.1 -0.1

var2ct = VAR.sim(B = Act, lag = 2, include = "both", n = 100)
ts.plot(var2ct, type = "l", col = c(1,2))
