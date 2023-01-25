# Семинар
library(tidyverse) # обработка данных
library(fpp3) # куча плюшек для рядов
library(rio) # импорт данных
library(urca) # тесты
library(lmtest) # тесты (включая тест на причинность)
library(patchwork)
library(rvest) # импорт данных
library(ARDL) # ardl

# Часть 1. ARDL модели
denmark
d = as_tibble(denmark)
d = mutate(d,quarter = yearquarter(time(denmark)))
d
glimpse(d)
d = as_tsibble(d, index = quarter)

p1 = autoplot(d, LRM)
p2 = autoplot(d, LRY)
p1 / p2

# Протестируем ряды на стационарность
# H0: ряд нестационарен
# H1: ряд стационарен
# Если ADF < DF_cr, то H0 отвергается
summary(ur.df(d$LRM, type = "drift"))
summary(ur.df(d$LRY, type = "drift"))

d = mutate(d, dLRM = difference(LRM,1), dLRY = difference(LRY,1), dIBO = difference(IBO,1), dIDE = difference(IDE,1))
d = tail(d,-1)
summary(ur.df(d$dLRM, type = "drift"))
summary(ur.df(d$dLRY, type = "drift"))



# ARDL-модели
ardl_11 = ardl(dLRM ~ dLRY, data = d, order = c(1,1))
summary(ardl_11)

mult_ardl_sr = multipliers(ardl_11, type = "sr") # действительно ли это мгновенный эффект изменения?
mult_ardl_sr

mult_ardl_lr = multipliers(ardl_11, type = "lr")
mult_ardl_lr

# Представим модель ARDL(1,1) в виде ECM
uecm_11 = uecm(ardl_11)
summary(uecm_11)

mult_uecm_sr = multipliers(uecm_11, type = "sr")
mult_uecm_sr

mult_uecm_lr = multipliers(uecm_11, type = "lr")
mult_uecm_lr

all.equal(mult_ardl_sr, mult_uecm_sr)
all.equal(mult_ardl_lr, mult_uecm_lr)



# Автоматический подбор параметров ARDL-модели
auto = auto_ardl(dLRM ~ dLRY + dIBO + dIDE, data = d, selection = "AIC", max_order = c(3,3,3,3))
summary(auto$best_model)
auto$top_orders

# ARDL(2,0,0,1)
ardl_2001 = ardl(dLRM ~ dLRY + dIBO + dIDE, data = d, order = c(2,0,0,1))
summary(ardl_2001)

res = ardl_2001$residuals
plot.ts(res)
bgtest(ardl_2001, order = 5)
mult_ardl = multipliers(ardl_2001, type = "lr") # dLRY: (0.561197)/(1-(-0.182716)-0.443575) = 0.7592
mult_ardl

# Представим модель ARDL(2,0,0,1) в виде ECM
uecm_2001 = uecm(ardl_2001)
summary(uecm_2001)
mult_uecm = multipliers(uecm_2001, type = "lr")
mult_uecm
all.equal(mult_ardl, mult_uecm)



# Часть 2. Коинтеграция
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
