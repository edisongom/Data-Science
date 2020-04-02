metricas <- data.frame(nnet = c(0.3669627, 0.8487879, 0.2656273, 0.8660868,
                                0.7509778, 0.2667577, 0.4762539, 0.8290359,
                                0.3857618, 0.3987424),
                       lda = c(0.4325550, 0.8287879, 0.3012339, 0.8553988,
                               0.7186083, 0.2873961, 0.5731759, 0.8234315,
                               0.3661388, 0.5699495),
                       svm = c(0.3498855, 0.8587879, 0.3365727, 0.8917666,
                               0.7527696, 0.2972005, 0.4869577, 0.8400313,
                               0.3945373, 0.6073158),
                       row.names = c("bladder", "breast", "colon", "kidney",
                                     "lung", "prostate", "thyroid", "oral",
                                     "uterine", "pancreatic"))
metricas

# concatenamos

metricas_stack <- stack(metricas)
metricas_stack$dataset <- as.factor(rep(row.names(metricas), times = 3))
names(metricas_stack) <- c("Accuracy","Model", "Dataset")
head(metricas_stack)

# Test Saphiro Wilk
tapply(metricas_stack$Accuracy, metricas_stack$Model, shapiro.test)

# En este caso conviene ANOVA, pero vamos a utilizar el método no parámetricos

# Test de Friedman
friedman.test(Accuracy ~ Model | Dataset, data = metricas_stack)

#Esta vez el test ha encontrado evidencias significativas, con un 95% de confianza,
#de que existen diferencias entre la precisión de los tres modelos con los 10 
#conjuntos de datos utilizados. Para saber entre qué modelos existen estas
#diferencias, aplicaremos el test post hoc de Nemenyi:


######### Test Nemenyi
library(PMCMRplus)
# Test Nemenyi
posthoc.friedman.nemenyi.test(Accuracy ~ Model | Dataset, data = metricas_stack)
  