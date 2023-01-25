# Временные ряды. Семинар 8.
# Тест Дикки-Фуллера

library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(urca) # тесты
library(patchwork)

setwd('/Users/polinapogorelova/Desktop/ЭЭП_АВР') # установка рабочей директории

set.seed(111)

n_obs = 200
eps = rnorm(n_obs)
t = 1:n_obs

data = tsibble(date = t,
               dt = 2 + 0.5*t + eps, # процесс стационарный вокруг тренда
               rw = cumsum(eps), # случайное блуждание (нестац)
               rw.tr = 1 + 0.25*t + cumsum(eps), # случайное блуждание с трендом (нестац)
               ar1 = arima.sim(model = list(ar = 0.75), n = n_obs), # AR(1) - стац
               index = date)

p1 = data %>% autoplot(rw)
p2 = data %>% autoplot(rw.tr)
p3 = data %>% autoplot(dt)
p4 = data %>% autoplot(ar1)

(p1 + p2) / (p3 + p4)

# ADF-тест
# H0: ряд нестационарен (DS)
# H1: ряд стационарный (TS)

# ADF без константы
# H0: y = y0 + ARIMA(p, 1, q) (нестационарный ряд)
# Ha: y = 0 + ARIMA(p+1, 0, q) (стационарный ряд с нулевым мат. ожиданием)
df_none = ur.df(data$rw, type = "none", selectlags = c("AIC"))
summary(df_none) # H0 не отвергается

# ADF с константой
# H0: y = ARIMA(p, 1, q) + trend (нестационарный ряд)
# Ha: y = ARIMA(p+1, 0, q) + const (стационарный ряд c ненулевым мат. ожиданием)
df_drift = ur.df(data$rw, type = "drift", selectlags = c("AIC"))
summary(df_drift)

# ADF с трендом
# H0: y = ARIMA(p, 1, q) + quadratic_trend (нестационарный ряд)
# Ha: y = ARIMA(p+1, 0, q) + trend (стационарный ряд)
df_trend =  ur.df(data$rw, type = "trend", selectlags = c("AIC"))
summary(df_trend)






# Пример данных о свадьбах в РФ
m = import('marriages.csv')

m2 = mutate(m, date = yearmonth(date))

marriages = as_tsibble(m2, index = date,
                       key = c('code', 'name'))


marr_rf = filter(marriages, (code == 643) & (date < ymd('2021-11-01')))

gg_tsdisplay(marr_rf, total, plot_type = 'partial')


# ADF без константы
# H0: y = y0 + ARIMA(p, 1, q) (нестационарный ряд)
# Ha: y = 0 + ARIMA(p+1, 0, q) (стационарный ряд с нулевым мат. ожиданием)

res_adf = ur.df(marr_rf$total, type = 'none',
                selectlags = 'AIC')
summary(res_adf) # H0 отвергается


# ADF с константой
# H0: y = ARIMA(p, 1, q) + trend (нестационарный ряд)
# Ha: y = ARIMA(p+1, 0, q) + const (стационарный ряд c ненулевым мат. ожиданием)

res_adf = ur.df(marr_rf$total, type = 'drift',
                selectlags = 'AIC')
summary(res_adf) # H0 отвергается


# ADF с трендом
# H0: y = ARIMA(p, 1, q) + quadratic_trend (нестационарный ряд)
# Ha: y = ARIMA(p+1, 0, q) + trend (стационарный ряд)

res_adf = ur.df(marr_rf$total, type = 'trend',
                selectlags = 'AIC')
summary(res_adf) # H0 отвергается

