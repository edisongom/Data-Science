#conjunto de métricas (índice Kappa) obtenidas de la calibración de dos modelos
#clasificadores (C5.0 y Random Forest)

metricas <- data.frame(C50 = c(0.9333333, 0.8750000, 0.8125000, 0.8125000,
                               0.9333333, 0.8000000, 0.6000000, 0.9375000,
                               0.7500000, 0.8000000),
                       Rf = c(0.9333333, 0.8750000, 0.8750000, 0.8750000,
                              0.9333333, 0.8000000, 0.5333333, 0.9375000,
                              0.9375000, 0.7333333))

metricas


######### t-test

# Test Shapiro-Wilk
shapiro.test(metricas$C50 - metricas$Rf)

# Se rechaza la normalidad, por lo que no es conveniente realizar un t-test

########## McNemar (test de la suma de rangos con signo de Wilcoxon)
# necesitamos una matriz de confusión para realizar el test

matriz_conf <- data.frame(si = c(29, 19), no = c(10, 137), 
                          row.names = c("si", "no"))

matriz_conf

# Test de McNemar
mcnemar.test(as.matrix(matriz_conf))

# no existen diferencias significativas entre ambas metricas,con una confianza
# del 5%.

