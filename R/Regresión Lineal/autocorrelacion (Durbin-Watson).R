########## Autocorrelación (durbin-Watson)


dwtest(modelo_lineal, alternative= "two.sided")
acf(modelo_lineal$residuals)
#Si el p-valor es pequeño rechazaríamos la hipótesis de independencia.