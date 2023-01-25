# Семинар 16. GARCH модели
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
library(rugarch) # GARCH
library(FinTS) # ARCH test


d = import("/Users/polinapogorelova/Desktop/ftse.xlsx")
d = mutate(d, trading_day = row_number())
d = as_tsibble(d, index = trading_day)

gg_tsdisplay(d, ftse, plot_type = "partial") # ACF и PACF
PACF(d, ftse) # PACF(1) близка к единице, следовательно, ряд может быть описан процессом случайного блуждания

d = d[,3:2]

# Рассчитаем лог-доходности
ret_ftse = diff(log(d$ftse),1)
d = mutate(d[2:5094,], ret_ftse = ret_ftse)

gg_tsdisplay(d, ret_ftse, plot_type = "partial") # график временного ряда лог-доходностей FTSE показывает существование периодов с низкой
# и высокой волатильностью

# Одним из способов моделирования такой структуры является использование модели с условной гетероскедастичностью (AutoRegressive Contional Heteroscedastisity,
# ARCH): модели, которая основана на введении предположения о зависимости дисперсии остатков от предыстории

# Проверим наличие ARCH-эффекта в остатках (вручную)
# H0: условная гетероскедастичность остатков отсутствует
model_const = lm(ret_ftse ~ 1, data = d)
summary(model_const)

d = mutate(d, ehat_sq = (resid(model_const)^2)) # добавим переменную "квадрат остатков"

test_arch1 = lm(ehat_sq ~ lag(ehat_sq, 1), data = d)
summary(test_arch1)

# Рассчитаем наблюдаемое значение статистики теста и критическое
T1 = nobs(model_const)
q1 = length(coef(test_arch1))-1
Rsq1 = glance(test_arch1)[[1]]
LM1 = (T1-q1)*Rsq1
alpha = 0.05
Chicr1 = qchisq(1-alpha, q1)
LM1
Chicr1

# Попробуем увеличить число лагов в правой части регрессии для квадрата остатков до 12
test_arch12 = lm(ehat_sq ~ lag(ehat_sq, 1) +  lag(ehat_sq, 2) + lag(ehat_sq, 3) +  lag(ehat_sq, 4) +
                  lag(ehat_sq, 5) + lag(ehat_sq, 6) + lag(ehat_sq, 7) + lag(ehat_sq, 8) + lag(ehat_sq, 9) +
                  lag(ehat_sq, 10) + lag(ehat_sq, 11) + lag(ehat_sq, 12), data = d)

# Рассчитаем наблюдаемое значение статистики теста и критическое
T12 = nobs(model_const)
q12 = length(coef(test_arch12))-1
Rsq12 = glance(test_arch12)[[1]]
LM12 = (T12-q12)*Rsq12
Chicr12 = qchisq(1-alpha, q12)
LM12
Chicr12

# Есть ARCH эффект!


# Проделаем то же самое, используя автоматическую процедуру
archtest1 = ArchTest(d$ret_ftse, lags = 1, demean = TRUE)
archtest1

archtest12 = ArchTest(d$ret_ftse, lags = 12, demean = TRUE)
archtest12



# Оценим ARCH(1), используя библиотеку "rugarch"
# Оценивание производится методом максимального правдоподобия
arch_spec = ugarchspec(
  variance.model = list(garchOrder = c(1,0)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE))

arch_fit = ugarchfit(spec = arch_spec, data = d$ret_ftse)
arch_fit

# Посмотрим на условную дисперсию
arch_var = ts(arch_fit@fit$var)
plot.ts(arch_var)


# Оценим GARCH модель, используя библиотеку "rugarch"
garch_spec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE))

garch_fit = ugarchfit(spec = garch_spec, data = d$ret_ftse)
garch_fit
coef(garch_fit)


# Сравним ARCH(1) и GARCH(1,1) модели с помощью информационных критериев
infocriteria(arch_fit)
infocriteria(garch_fit)

# Посмотрим на оцененную доходность
rhat = garch_fit@fit$fitted.values
plot.ts(rhat)
# и на условную дисперсию
garch_var = garch_fit@fit$var # сохраним условную дисперсию
res_sq = (garch_fit@fit$residuals)^2
plot.ts(res_sq, type = "l")
lines(garch_var, col = "green")

# Построим прогноз
fcst = ugarchforecast(garch_fit, n.ahead = 10)
fcst
# Прогноз для условной дисперсии:
fcst_var = fcst@forecast$sigmaFor
fcst_var
plot(fcst_var, type = "l")

