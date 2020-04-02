#
Disponemos de la informacion de 2 clasificadores, se utilizo k-FOld=10

metricas <- data.frame(C50 = c(0.9073259, 0.6822547, 0.6925111, 0.2719983, 
                               0.9073259, 0.0925111, 0.0925111, 0.8572170,
                               0.9106929, 0.0925111),
                       Rf = c(0.6541556, 0.7521948, 0.8056707, 0.2816065, 
                              0.9874889, -0.1236222, 0.3208222, 0.7521948, 
                              0.8056707, 0.1236222))

metricas

############ t-Student
# Requiere normalidad
shapiro.test(metricas$C50-metricas$Rf)
# con una significancia del 5%, sigue una distribución normal.

# Homocedasticidad
metricas_stack <- stack(list(C50 = metricas$C50, Rf = metricas$Rf))
head(metricas_stack)
# Test de Bartlett
bartlett.test(values ~ ind, data = metricas_stack)
# se cumple la homogeneidad

# t-test
t.test(metricas$C50, metricas$Rf, paired = TRUE)

#
#No podemos rechazar, con un 95% de confianza, que la diferencia entre las medias
#de las métricas entre los modelos Rf y C50 son distintas a 0. Es decir, el 
#rendimiento del modelo Rf no es significativamente distinto al C50. De haber sido
#significativa, el signo del mean of the differences nos indicaría cual de los 
#dos sería el mejor (Rf).
