# VAR/VECM
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


# Импорт данных
d = denmark
d
d = as_tibble(d)
d = mutate(d, quarter = yearquarter(time(denmark)))
d
d = as_tsibble(d, index = quarter)
d = d[, -3]
gg_tsdisplay(d, LRM)

# Проверим ряды на стационарность
train = filter(d, quarter < ymd('1987-01-01'))
test = filter(d, quarter >= ymd('1987-01-01'))

# Графики показателей
p1 = autoplot(train, LRY)
p2 = autoplot(train, IBO)
p3 = autoplot(train, LRM)
p4 = autoplot(train, IDE)
(p1 + p2) / (p3 + p4)

# Тест на стационарность
# ADF
summary(ur.df(train$LRY, type = "trend", selectlags = "AIC"))
summary(ur.df(train$LRM, type = "drift", selectlags = "AIC"))
summary(ur.df(train$IBO, type = "drift", selectlags = "AIC"))
summary(ur.df(train$IDE, type = "drift", selectlags = "AIC"))

# DF-GLS
summary(ur.ers(train$LRY, type = "DF-GLS", lag.max = 4))
summary(ur.ers(train$LRM, type = "DF-GLS", lag.max = 4))
summary(ur.ers(train$IBO, type = "DF-GLS", lag.max = 4))
summary(ur.ers(train$IDE, type = "DF-GLS", lag.max = 4)) # I(0)?


# Тест на стационарность для первых разностей
# DF-GLS
summary(ur.ers(diff(train$LRY,1), type = "DF-GLS", lag.max = 4))
summary(ur.ers(diff(train$LRM,1), type = "DF-GLS", lag.max = 4))
summary(ur.ers(diff(train$IBO,1), type = "DF-GLS", lag.max = 4)) # Дргуое число лагов? (3)
summary(ur.ers(diff(train$IDE,1), type = "DF-GLS", lag.max = 4))


# Все ряды I(1)!
# Протестируем наличие коинтегрирующих соотношений с помощью теста Йохансена
jotest = ca.jo(train[,1:4], type = "eigen", K = 2, ecdet = "const", spec = "longrun", season = 4)
summary(jotest) # r=1 одно коинтегрирующее соотношение

coint = 1*train$LRM -1.037571*train$LRY + 5.236812*train$IBO - 4.260811*train$IDE - 6.033561
coint = as.ts(coint)

plot(coint, type = "l")
summary(ur.ers(coint, type = "DF-GLS", model = "constant", lag.max = 4)) # стационарный ряд

# Так как ряды коинтегрированы, то можно оценить VECM
vecm_model = VECM(train[,1:4], lag = 2, r = 1, include = "const", beta = NULL, estim = "ML", LRinclude = "const")
vecm_model

# Переход от VEC к VAR при r = 1
var_model = vec2var(jotest, r = 1)

# Obtain IRF
irf_lrm_ibo = irf(var_model, n.ahead = 100, impulse = "LRM", response = "IBO", ortho = TRUE)

#  Построим график IRF
plot(irf_lrm_ibo) # irf не обязательно затухает, так как ряды нестационарны


# Прогнозирование с помощью VAR модели
fcst1 = predict(var_model, h = 3)
fcst1
plot(fcst1)

f = as.data.frame(fcst1$fcst)
f
f = f[1:3,]
test = mutate(test, pred_LRM = f$LRM.fcst[1:3], pred_LRY = f$LRY.fcst[1:3], pred_IBO = f$IBO.fcst[1:3], pred_IDE = f$IDE.fcst[1:3])


p5 = d %>%
  autoplot(LRM) +
  autolayer(test, pred_LRM, colour = "red")

p6 = d %>%
  autoplot(LRY) +
  autolayer(test, pred_LRY, colour = "red")


p7 = d %>%
  autoplot(IBO) +
  autolayer(test, pred_IBO, colour = "red")

p8 = d %>%
  autoplot(IDE) +
  autolayer(test, pred_IDE, colour = "red")

(p5 + p6) / (p7 + p8)


# Bounds Test
# 1. Убедимся, что оба ряда не I(2) - уже сделали
ardl = auto_ardl(LRM ~ LRY + IBO + IDE, data = train, max_order = 3)
ardl$best_order
ardl2200 = ardl(LRM ~ LRY + IBO + IDE, data = train, order = c(2, 2, 0, 0))
res_ardl = ardl2200$residuals
plot(res_ardl)
dwtest(ardl2200) # lm-test H0: нет автокорреляции в остатках

bounds_f_test(ardl2200, case = 3)
bft = bounds_f_test(ardl2200, case = 3, alpha = 0.05)
bft
bft$tab # Ряды коинтегрированы!

# Оценим ECM-модель для LRM
# Шаг 1
# Оценим коинтегрирующее соотношение (DOLS)
train = mutate(train, dLRM = difference(LRM,1), dLRY = difference(LRY,1), dIBO = difference(IBO,1), dIDE = difference(IDE,1))

model_coint1 = lm(LRM ~ LRY + IBO + IDE, data = train)
summary(model_coint1)

model_coint2 = lm(LRM ~ LRY + IBO + IDE + lag(dLRY) + dLRY + lead(dLRY, 1) + lag(dIBO) + dIBO + lead(dIBO, 1) +
                    lag(dIDE) + dIDE + lead(dIDE, 1), data = train) # DOLS
summary(model_coint2)
resid = model_coint2$residuals
plot(resid, type = 'l')
summary(ur.ers(resid, type = "DF-GLS", lag.max = 4))
# Шаг 2
train2 = tail(train,-2)
train2 = head(train2,-1)
train2 = mutate(train2, error.ecm = model_coint2$residuals)

model_ecm = lm(dLRM ~ dLRY + dIBO + dIDE + error.ecm, data = train2)
summary(model_ecm)

dwtest(model_ecm)
plot(model_ecm$residuals, type = "l")
summary(ur.df(model_ecm$residuals, type = "drift", selectlags = "AIC"))
