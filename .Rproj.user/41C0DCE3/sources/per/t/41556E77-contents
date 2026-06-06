library(readxl)
#install.packages(pacman)
install.packages("pacman")

p_load(readxl, tseries, urca, dplyr)

cointegracion <- read_excel("cointegracion.xlsx")

ingreso <- cointegracion$Ingreso
consumo <- cointegracion$Consumo

set.seed(123) #semilla para que los datos siempre sean los mismos

ingreso <- cointegracion$Ingreso + rnorm(n = 20, mean = 0, sd = 15)
consumo <- cointegracion$Consumo + rnorm(n = 20, mean = 0, sd = 10)

head(consumo)

print('adf ingreso')
adf.test(ingreso)
print("adf consumo")
adf.test(consumo)

modelo_eg <- lm(y ~ x)
summary(modelo_eg)

#residuos del modelo 
residuos <- modelo_eg$residuals

#adf a los residuos

cat('si el p-value es < 0.05 los residuos son estacionarios y hay cointegracion)')
adf.test(residuos)
