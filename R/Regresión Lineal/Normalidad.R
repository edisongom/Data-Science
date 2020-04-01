###### Muestra pequeña
#H0: Los residuos son normales
shapiro.test(residuals(modelo_lineal))
hist(residuals(modelo_lineal))
qqnorm(modelo_lineal$residuals)
qqline(modelo_lineal$residuals)

#Muestra grande
library(nortest)
#H0: Los residuos son normales

lillie.test(residuals(modelo_lineal))
hist(residuals(modelo_lineal))
qqnorm(modelo_lineal$residuals)
qqline(modelo_lineal$residuals)
