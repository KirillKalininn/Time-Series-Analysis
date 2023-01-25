# Семинар 17. Многомерные GARCH модели
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
library(rmgarch) # многомерные GARCH
library(MTS) # BEKK

d = import("/Users/polinapogorelova/Desktop/mgarch.xlsx")

# Добавим лог-доходности РТС и Brent
ret_rtsi = diff(log(d$rtsi))
ret_brent = diff(log(d$brent))

d = mutate(d[2:5467,], ret_rtsi = ret_rtsi, ret_brent = ret_brent)
d = mutate(d, trading_day = row_number())
d = as_tsibble(d, index = trading_day)

# Построим графики исходных рядов
p1 = autoplot(d, rtsi)
p2 = autoplot(d, brent)
p1/p2

# Протестируем ряды лог-доходностей на стационарность
gg_tsdisplay(d, ret_rtsi, plot_type = "partial") # ACF и PACF для РТС
summary(ur.df(d$ret_rtsi, type = "drift"))

gg_tsdisplay(d, ret_brent, plot_type = "partial") # ACF и PACF для Brent
summary(ur.df(d$ret_brent, type = "drift"))

# Протестируем наличие ARCH-эффекта для РТС
archtest1 = ArchTest(d$ret_rtsi, lags = 1)
archtest1
archtest12 = ArchTest(d$ret_rtsi, lags = 12)
archtest12

# Протестируем наличие ARCH-эффекта для Brent
archtest1 = ArchTest(d$ret_brent, lags = 1)
archtest1
archtest12 = ArchTest(d$ret_rtsi, lags = 12)
archtest12

# Оценим BEKK модель
model1 = BEKK11(d[,4:5])

# Оцененные значения дисперсии, ковариации и корреляции остатков
Sigma.t = model1$Sigma.t

# Построим графики
par(mfrow = c(2,2))
plot(d$date, Sigma.t[,1], xlab = 'Date', ylab = "", main = "Var RTS", type = 'l')
plot(d$date, Sigma.t[,4], xlab = 'Date', ylab = "", main = "Var Brent", type = 'l')
plot(d$date, Sigma.t[,2], xlab = 'Date', ylab = "", main = "Covariance Brent RTS", type = 'l')
plot(d$date, Sigma.t[,2]/(sqrt(Sigma.t[,1])*sqrt(Sigma.t[,4])), xlab = 'Date', ylab = "", main = "Correlation Brent RTS", type = 'l')

# Рассчитаем условную корреляцию между РТС и Brent
corr_rtsi_brent_bekk = Sigma.t[,2]/(sqrt(Sigma.t[,1])*sqrt(Sigma.t[,4]))
plot(corr_rtsi_brent_bekk, type = "l")

# Оценим DCC-модель
# Зададим для каждого ряда GARCH(1,1) модель
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                          variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

# DCC модель - GARCH(1,1) для условной коорреляции
# Зададим спецификацию DCC модели
dcc.garch11.spec = dccspec(uspec = multispec(replicate(2, garch11.spec)), dccOrder = c(1,1), distribution = "mvnorm")
dcc.garch11.spec

# Оценим DCC модель
dcc.fit = dccfit(dcc.garch11.spec, data = d[,4:5])
dcc.fit
infocriteria(dcc.fit)

plot(dcc.fit)
dcc.fcst = dccforecast(dcc.fit, n.ahead = 30)
plot(dcc.fcst)

# Рассчитаем условную корреляцию между РТС и Brent
H = dcc.fit@mfit$H # матрица условных ковариаций
corr_rtsi_brent_dcc = H[1,2,]/sqrt(H[1,1,]*H[2,2,])
plot(corr_rtsi_brent_dcc, type = "l")

# Сравним графики условных корреляций по двум моделям
matplot(cbind(corr_rtsi_brent_bekk, corr_rtsi_brent_dcc), type = "l", lty = 1,
        main = "Условные корреляции между РТС и Brent")
legend("bottomright", legend = c("BEKK", "DCC"), lty = 1, col = 1:2)

# Оценим корреляционную матрицу для модельных значений условных корреляций
cor(cbind(corr_rtsi_brent_bekk, corr_rtsi_brent_dcc))

# Построим диаграмму рассеяния
plot(corr_rtsi_brent_bekk, corr_rtsi_brent_dcc, main = "Диаграмма рассеяния")

