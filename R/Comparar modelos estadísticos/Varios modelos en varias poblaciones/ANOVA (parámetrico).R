#métricas de precisión de 3 modelos sobre los 10 conjuntos de datos de distintas fuentes:

metricas <- data.frame(nnet = c(0.95343, 0.95428, 0.95637, 0.95334, 0.95795,
                                0.95935, 0.95421, 0.95617, 0.95462, 0.95640),
                       lda = c(0.85374, 0.82696, 0.78597, 0.83563, 0.84223,
                               0.75414, 0.85768, 0.84611, 0.80011, 0.83464),
                       svm = c(0.93707, 0.95418, 0.93346, 0.94629, 0.95895,
                               0.90717, 0.95892, 0.96263, 0.97544, 0.96700),
                       row.names = c("bladder", "breast", "colon", "kidney",
                                     "lung", "prostate", "thyroid", "oral",
                                     "uterine", "pancreatic"))

metricas

#concatenar
metricas_stack <- stack(metricas)
metricas_stack$dataset <- as.factor(rep(row.names(metricas), times = 3))
names(metricas_stack) <- c("Accuracy","Model", "Dataset")
head(metricas_stack)


### ANOVA
## Normalidad
# Test Saphiro Wilk (comprobar normalidad)

tapply(metricas_stack$Accuracy, metricas_stack$Model, shapiro.test)

# Todos los casos muestran una distribución normal


library(car)

# Test de Mauchly (comprobar esfericidad). Se utiliza la funcion Anova del 
# paquete "car", que devuelve dicho test: no es indispensable

modelos <- factor(c("nnet", "lda", "svm"))

options(contrasts = c("contr.sum", "contr.poly"))

# Anova
summary(Anova(lm(as.matrix(metricas) ~ 1), # modelo lineal
              idata = data.frame(modelos), 
              idesign = ~ modelos),
        multivariate = FALSE)

# No existen evidencias significativas para rechazar la esfericidad de la matriz de covarianzas

#la ANOVA indica la existencia de diferencias significativas entre los tres modelos


############## test de post hoc de Tukey

library(nlme)
# Modelo lineal generalizado que compensa el hecho de que no puede existir 
# variabilidad entre los grupos
modelo_lme <- lme(Accuracy ~ Model, random = ~1 | Dataset, 
                  data = metricas_stack)

library(multcomp)
# Test Tukey, aplicado al modelo lineal generalizado
tukey_lme <- glht(modelo_lme, linfct=mcp(Model = "Tukey"))
summary(tukey_lme)
# El test muestra diferencias altamente significativas entre los modelos LDA-NNET
#siendo NNET mejor que LDA ya que el estimador de la media de las diferencias es negativo)
# el modelo LDA es el peor de los tres.



