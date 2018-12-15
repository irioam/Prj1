# instalação de pacotes necessários
install.packages("forecast")
install.packages("ggplot2")
install.packages("urca")
install.packages("lmtest")
install.packages("seasonal")
install.packages("seasonalview")
install.packages("plotly")

library(ggplot2)
library(forecast)
library(plotly)
library(urca)

#criação de um TS
myts = rnorm(60)
myts = ts(myts, start = c(2012,1), end = c(2016,12), frequency = 12)
myts

class(myts)
plot(myts)


# utilizaremos a TS sunspots (mancha solares que já vem com o R)
# TS = Time Séries ou Série Temporal
# sunspots -> TS que já vem com o R

print(sunspots)  # imprime a série temporal. 
class(sunspots) # exibe a classe da série temporal.
help("sunspots") # exibe um texto a respeito da TS. 

max(sunspots) # valor máximo da TS
min(sunspots) # valor mínimo da TS
mean(sunspots) # média
median(sunspots) # mediana
summary(sunspots) # resumo: menor valor, 1º quartil, mediana, média, 3º quartil e o maior valor. 

length(sunspots) # saber a quantidade de dados da TS

start(sunspots) #exibe quando a série inicia
end(sunspots)   #exibe quando a série termina
frequency(sunspots)

#window -> cria um subconjunto dos dados de outro conjunto
# sun2 é o novo conjunto ou melhor a nova TS
sun2 = window(sunspots, start=c(1749,1), end=c(1763,12) )
print(sun2)

plot(sunspots)
hist(sunspots)
boxplot(sunspots)

autoplot(sunspots)
autoplot(AirPassengers)

sun2 = window(sunspots, start=c(1749,1), end=c(1763,12) )
plot(sun2)
print(sun2)
hist(sun2)
autoplot(sun2)
boxplot(sun2)

plot(aggregate(AirPassengers, FUN = mean))

# importar um arquivo csv
tempts = read.csv(file.choose(), sep=",", header = F)

# mostrar os dados da série
print(tempts)

# transformar a série de dados em série temporal
# indicado com a função "ts" e passar o início e fim e a frequencia.
tempts = ts(tempts[2], start = c(1884), end = c(1939), frequency = 1)

# checar se é uma série temporal
class(tempts)

#plotar os dados em um gráfico padrão
plot(tempts)

# curso de Linguagem R na udemy
citation()
demo(persp)
demo(graphics)
prod(3,3,3)
factorial(5)

# avaliar Residuos no R
library(ggplot2)
library(forecast)
library(plotly)

# ts -> série temporal presidents é uma série que mostra a popularidade
# presidentes

#mostra um gráficos simples
autoplot(presidents)

# criando um modelo preditivo utilizando o auto.arima
# auto.arima é um dos modelos preditivos mais poderosos
modelo_prev = auto.arima(presidents)

# exibe os dados do modelo
print(modelo_prev$residuals)

# mostra um gráfico do modelo
autoplot(modelo_prev$residuals)

# mostra um histograma
hist(modelo_prev$residuals)

# mostra a variância
var(modelo_prev$residuals, na.rm = T)

# média como vetor dos resíduos
mean(as.vector(modelo_prev$residuals), na.rm = T)

# diagrama de correlação desses residuais
acf(modelo_prev$residuals, na.action = na.pass)

#analisar os residuais
checkresiduals(modelo_prev)

# pré-requisitos para requisitos devem ser normalmente distribuídos.
# testar se o modelo está normalmente distribuído
shapiro.test(modelo_prev$residuals)

# será utilizado o pacote urca que tem o teste de estacionaridade
# fazer o teste de estacionaridade

# a TS AirPassenger que é uma série que tem Tendência e Sazonalidade e
# por isso provavelmente tem Estacionaridade
x = ur.kpss(AirPassengers)

# exibir o resultado
print(x)

# como o resultado está muito acima de 0,05 esta série não é estacionária
# diff irá realizar a diferenciação e armazena em z
z = diff(AirPassengers)


# testar se é estacionária

x = ur.kpss(z)
print(x)

#agora o resultado ficou com 0,01 portanto há indícios de que a
# TS ficou estacionária
plot(x)

# para saber quantas diferenciações são necessárias para transformar 
# uma série não estacionária em estacionária

ndiffs(AirPassengers)

split.screen(figs = c(2,1))
screen(1)
plot(AirPassengers)
screen(2)
plot(z)

#decomposição -> é necessário para separar a tendência da sazonalidade
# do restante da série e para realizar previsão.

# Transformações no R

# utiliza os pacotes forecas e ggplot2

# Primeira transformação t1

t1 = BoxCox(AirPassengers, lambda = 0)
autoplot(t1)

t2 = BoxCox(AirPassengers, lambda = 0.1)
autoplot(t2)

lbd = BoxCox.lambda(AirPassengers)
print(lbd)
t3 = BoxCox(AirPassengers, lambda = lbd)
print(t3)
autoplot(t3)

t4 = diff(AirPassengers)
autoplot(t4)

t5 = log10(AirPassengers)
autoplot(t5)

split.screen(figs = c(2,2))
screen(1)
plot(t1)
screen(2)
plot(t2)
screen(3)
plot(t3)
screen(4)
plot(t5)
close.screen(all = T)

# Médias Móveis no R
# pode gerada como simples e exponêncial

autoplot(fdeaths)

fdeaths2 = ma(fdeaths, order=5)
autoplot(fdeaths2)

fdeaths3 = ma(fdeaths, order=12)
autoplot(fdeaths3)

fdeaths4 = tsclean(fdeaths)
autoplot(fdeaths4)

plot(fdeaths)
lines(fdeaths2, col="red")
lines(fdeaths3, col="blue")
lines(fdeaths4, col="green")

legend("topright", legend = c("Orig.", "Ma5", "Ma12", "Tsc"), col = c("black","red","blue","green"), lty = 1:2, cex = 0.8)

# Previsão
# Em uma Previsão não está descartado um erro.
# Porque erros?

# Técnicas de Previsão estudadas: 
# Naive - técnica simples.
# Naive Sazonal

# Naive com R

library(forecast)
library(ggplot2)

set.seed(4312)
x = cumsum(sample(c(-1,1),100, T))
print(x)
serie = ts(x, start = c(1900), end = c(2000), frequency = 1)
print(serie)
autoplot(serie)

prev = naive(serie,h=5)
class(prev)
print(prev)
print(prev$fitted)
print(prev$residuals)
autoplot(prev)

prev2 = naive(serie, h=5, level = c(95,99))
print(prev2)
autoplot(prev2)
split.screen(figs = c(2,1))
screen(1)
plot(prev)
screen(2)
plot(prev2)
close.screen(all=T)
