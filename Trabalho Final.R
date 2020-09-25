library(readr)
library(readxl)
library(ts)
library(fpp2)
library(tidyverse)
library(gridExtra)
library(BETS)
library(fpp)
library(fpp2)
library(readxl)
library(tseries)
library(writexl)

# Importando a base de dados
setwd("C:/Users/Pichau/Documents/MBA/Analise Series Temporais")
fertilizantes1 <- read_xlsx("Demanda.xlsx")
fertilizantes1

# transformando a base de dados em serie temporal
fertilizantes2 <- ts(fertilizantes1, frequency =  12, start=c(1998,1), end = c(2020,04))
fertilizantes2

# Criando as bases de treino e teste
treino <- ts(fertilizantes2, frequency = 12, start = c(2007,1), end = c(2018,12))
teste <- ts(fertilizantes2, frequency = 12, start = c(2019,1), end = c(2020,4))

# visualizando a base inteira
autoplot(fertilizantes2)+
  xlab("Trimestres")+
  ylab("")+
  ggtitle("")

# Efetuando uma breve analise exploratoria
ggseasonplot(fertilizantes2, year.labels = TRUE,
             year.label.left = TRUE) +
  ylab("")+ 
  xlab("Trimestre")+
  ggtitle("")

boxplot(fertilizantes2~cycle(fertilizantes2),
        xlab="Trimestres",
        ylab = "" ,
        col="orange", 
        main ="",
        par(bg = '#E0E0E0'))

# Decompondo a serie
decomp <- decompose(fertilizantes2)
plot(decomp)

# criando modelo holt winters com sazonalidade aditiva
hw.a <- hw(treino, seasonal = "additive", h= 12, level = 95)
summary(hw.a)
checkresiduals(hw.a, col ="blue")

fit.a <- hw.a$fitted # Ajuste do modelo aos dados observados

# plotando a serie real, a previsão e o modelo
autoplot(treino, series = "Serie_Real", lwd= 1.1)+ #serie_original
  autolayer(fit.a, series = "modelo HW- Aditivo", lwd = 1.1)+ # Ajuste do modelo
  autolayer(hw.a, series = "Previsao", showgap = FALSE) # previsÃ£o h= n perÃ­odos 

# obtendo a acuracia
accuracy(hw.a)

# criando modelo holt winters com sazonalidade multiplicativa
hw.m <- hw(treino, seasonal = "multiplicative", h= 12, level = 95)
summary(hw.m)
checkresiduals(hw.m, col ="blue")

fit.m <- hw.m$fitted # Ajuste do modelo aos dados observados

autoplot(treino, series = "Serie_Real", lwd= 1.1)+ #serie_original
  autolayer(fit.m, series = "modelo HW- Multiplicativo", lwd = 1.1)+ # Ajuste do modelo
  autolayer(hw.m, series = "Previsao", showgap = FALSE) # previsÃ£o h= n perÃ­odos 

# SARIMA
arima.fert <- auto.arima(treino, seasonal = TRUE, stepwise = TRUE, lambda="auto")

# teste de ausência de autocorrelação serial Ho=iid H1=nao iid
#p(4)+q(2)+P(2)+Q(0) = 8
Box.test(x = arima.fert$residuals, lag=24, type = "Ljung-Box", fitdf = 8) # pvalor=0.1078 a serie aparente ser iid
checkresiduals(arima.fert, col ="blue")

# teste de heterocedasticidade H0 os residuos ao quadrado sao homocedasticos (ruido branco) H1 a serie e heterocedastica
library(FinTS)

ArchTest(arima.fert$residuals, lags = 36) # a serie é homocedastica
?ArchTest

# teste de normalidade Ho= normal h1= nao normal
library(normtest)

jb.norm.test(arima.fert$residuals, nrepl = 2000) # pvalor < 0.05 nao e normal
shapiro.test(arima.fert$residuals)
hist(arima.fert$residuals,
     main = 'Histograma dos resíduos',
     xlab = 'resíduos',
     ylab = 'Frequências')

round(summary(arima.fert$residuals), digits = 4)

# Comparando modelos
accuracy(arima.fert)
accuracy(hw.m)
accuracy(hw.a)

AIC(arima.fert)
AIC(hw.m$model)
AIC(hw.a$model)


# Correlograma

ggAcf(fertilizantes2)
acf(fertilizantes2)

ggPacf(fertilizantes2)
pacf(fertilizantes2)
ggPacf(treino)
ggAcf(treino)

# Testes de Estacionariedade

#Revisitando a serie

autoplot(fertilizantes2)

#### Teste ADF ----

adf.test(fertilizantes2) #passou

#### Teste KPSS ----

kpss.test(fertilizantes2)

#### Teste de Phillips-Perron ----

pp.test(fertilizantes2)

# Analisando os parametros do modelo
t_test(model = arima.fert)

# Efetuando as previsoes

fit <- forecast(arima.fert, h = 36)

autoplot(fertilizantes2, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "SÃ©rie Predita")+
  autolayer(fit, series = "ProjeÃ§Ã£o h =12")

checkresiduals(fertilizantes2)

