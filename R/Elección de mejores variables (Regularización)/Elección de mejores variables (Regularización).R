library(ISLR)
datos=College
head(datos)
summary(datos)
glimpse(datos)

sum(is.na(datos)) #Sin valores missing

##### Separamos los datos en train y test

set.seed(123)
train=sample(x = 1:nrow(datos), size = round(nrow(datos) * (2/3)))
datos.train <- datos[train, ]
datos.test <- datos[-train, ]

##### K-Fold cross validation

# Se asigna cada observaci�n a un grupo
k <- 10
set.seed(11)
folds <- sample(x = 1:k, nrow(datos.train), replace = TRUE)
head(folds)

#Distribuci�n de las observaciones en cada Fold
table(folds)

######### For loop##
#Para el j-�simo fold o grupo, los elementos en el objeto fold que coincidan con
#j actuar�n como set de validaci�n, y el resto como set de entrenamiento. 
#Se utilizan los datos de entrenamiento para identificar el mejor modelo para
#cada posible tama�o en base al RSS. A continuaci�n, se lleva a cabo la 
#predicci�n para cada modelo usando el grupo de test j, calculando el test 
#error de cada grupo correspondiente y guardando el resultado en la posici�n
#correspondiente de la matriz cv_error.


pred_best_subset <- function (object ,newdata ,id ,...){
  # extracci�n de la formula del modelo
  form=as.formula (object$call [[2]])
  # matriz modelo con la f�rmula del modelo y los datos de test 
  mat=model.matrix (form ,newdata )
  # extracci�n de los coeficientes del modelo
  coefi =coef(object ,id=id)
  # se almacena el nombre de los predictores
  xvars =names (coefi )
  # obtenci�n de predicciones mediante product matricial entre los coeficientes del modelo y el valor de los predictores en el test set
  mat[,xvars ]%*% coefi
}

# Matriz donde se almacenar� el error de validaci�n
cv_error <- matrix(data = NA, nrow = 10, ncol = 17, 
                   dimnames = list(NULL, c(1:17)))
# Cada fila corresponde a un fold, y cada columna a un tama�o de modelo
head(cv_error, 3)}



library(leaps)
for (k in 1:10) {
  train <- datos.train[folds != k, ]
  best_subset <- regsubsets(Apps ~ ., data = datos.train, nvmax = 17)
  # para cada mejor modelo escogido en el paso anterior...
  for (i in 1:17) {
    test <- datos.train[folds == k, ]
    # uso de la funci�n definida anteriormente para extraer las predicciones del modelo i almacenado en el objeto best_subset
    pred <- pred_best_subset(object = best_subset, newdata = test, id = i)
    # se calcula y almacena el test error (MSE) para cada modelo i
    cv_error[k, i] <- mean((test$Apps - pred)^2)
  }
}

#media del test error de los 10 grupos por cada tama�o de modelo

cv_error_medio <- apply(X = cv_error, MARGIN = 2, FUN = mean)
cv_error_medio

which.min(cv_error_medio)

id=data.frame(ID=1:17)
cv_er=cbind(cv_error_medio,id)

cv_er %>%
  ggplot( aes(x=ID, y=cv_error_medio)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  ggtitle("Evolution of bitcoin price")

# el punto m�nimo es 17, pero a partir de 7 la disminuci�n es m�nima.

test.MSE.subset <- cv_error_medio[7] 
test.MSE.subset

#Como �ltimo paso, se identifican los coeficientes de los predictores escogidos 
#en el mejor modelo, esta vez con el set de datos completo (para obtener unos 
#coeficientes m�s precisos), y se calcula su correspondiente test error.

modelo.bestsubset <- regsubsets(Apps ~ ., data = datos.train, nvmax = 17)

coef(modelo.bestsubset, 7)




############# Ridge regression y Lasso ####################

# La funci�n a utilizar en ambos casos es la misma, glmnet(), con la diferencia
#de que para aplicar ridge regression ha de usarse el argumento alpha = 0, y 
#alpha = 1 si se quiere aplicar lasso. No deben haber valores missing

#El valor de lambda ?? ser� elegido mediante validaci�n cruzada, en lugar de 
#elegirlo arbitrariamente, y ridge regression/lasso se llevar�n a cabo sobre los
#datos de entrenamiento. Para crear la matriz de valores num�ricos que requiere 
#la funci�n glmnet(), aplicamos la funci�n model.matrix() a los datos de 
#entrenamiento y validaci�n.

library(glmnet)

# Conversi�n a matriz modelo de los datos de train y test
datos.train.mat <- model.matrix(Apps ~ ., data = datos.train)
datos.test.mat <- model.matrix(Apps ~ ., data = datos.test)

# Conjunto de valores de lambda 
lambda = 10 ^ seq(from = 4, to = -2, length = 100)
# 10-fold cross validation para obtener el mejor lambda
set.seed(12)
cv.ridge <- cv.glmnet(x = datos.train.mat, y = datos.train$Apps, alpha = 0, 
                      lambda = lambda, thresh = 1e-12, type.measure="mse")

plot(cv.ridge)

# Mejor lambda (menor error de validaci�n)
cv.ridge$lambda.min

# Modelo RIDGE con los datos de entrenamiento. Se asocia un vector de coeficientes para cada valor de lambda
modelo.ridge.train <- glmnet(x = datos.train.mat, y = datos.train$Apps, 
                             alpha = 0, lambda = lambda, thresh = 1e-12)
# 18 variables + intercept, y 100 vectores de coeficientes (uno para cada valor de lambda
dim(coef(modelo.ridge.train))

plot(modelo.ridge.train, xvar = "lambda", label = TRUE)

# Predicciones del modelo con los datos de test y el mejor lambda
pred.modelo.ridge <- predict(modelo.ridge.train, s = 43.28761, newx = datos.test.mat)
# Test error (MSE)
test.MSE.ridge <- mean((pred.modelo.ridge - datos.test$Apps)^2)
test.MSE.ridge


# Se excluye la primera columna con los nombres de las universidades
modelo.ridge <- glmnet(x = model.matrix(Apps ~ ., data = datos)[,-1], 
                       y = datos$Apps, alpha = 0)
# Coeficientes del modelo
predict(modelo.ridge, type = "coefficients", s = 43.28761)


########## LASSO

set.seed(11)

cv.lasso <- cv.glmnet(x = datos.train.mat, y = datos.train$Apps, alpha = 1, 
                      lambda = lambda, thresh = 1e-12, type.measure="mse")

plot(cv.lasso)
mejor.lambda <- cv.lasso$lambda.min
mejor.lambda


# Modelo LASSO con los datos de entrenamiento. Se asocia un vector de coeficientes para cada valor de lambda
modelo.lasso.train <- glmnet(x = datos.train.mat, y = datos.train$Apps, 
                             alpha = 1, lambda = lambda, thresh = 1e-12)

plot(modelo.lasso.train, xvar = "lambda", label = TRUE)


# Predicciones del modelo con los datos de test y el mejor lambda
pred.modelo.lasso <- predict(modelo.lasso.train, s =4.641589, 
                             newx = datos.test.mat)
# Test error (MSE)
test.MSE.lasso <- mean((pred.modelo.lasso - datos.test$Apps)^2)
test.MSE.lasso


modelo.lasso <- glmnet(x = model.matrix(Apps ~ ., data = datos)[,-1],
                       y = datos$Apps, alpha = 1)
# Coeficientes del modelo
predict(modelo.lasso, type = "coefficients", s = 49.77024)





############### PCR y PLS


# Al ajustar un modelo PCR es importante estandarizar los predictores.
#Aplicaremos adem�s 10-fold cross validation para calcular el error asociado
#a cada posible valor de M (n�mero de componentes principales), y escoger 
#el mejor.
install.packages("pls")
library(pls)

# Modelo PCR
modelo.pcr.train <- pcr(Apps ~ ., data = datos.train, scale = TRUE, 
                        validation = "CV")
# Resultado del ajuste del modelo
summary(modelo.pcr.train)

#En el summary del modelo se muestra el error de validaci�n (CV y adjCV, que 
#corresponden al RMSEP o root mean squared error) correspondiente al uso de
#distinto n�mero de componentes. Tambi�n se muestra el porcentaje de varianza 
#explicada en los predictores y la variable respuesta.

validationplot(modelo.pcr.train, val.type = "MSEP")

# Test MSE
pred.modelo.pcr <- predict(modelo.pcr.train, datos.test, ncomp = 5)
test.MSE.pcr <- mean((pred.modelo.pcr - datos.test$Apps)^2)

test.MSE.pcr

modelo.pcr <- pcr(Apps ~ ., data = datos, scale = TRUE, ncomp = 5)
summary(modelo.pcr)



########### PSL

set.seed(1)
# Modelo PLS
modelo.pls.train <- plsr(Apps ~ ., data = datos.train, scale = TRUE, 
                         validation = "CV")
# Resumen del ajuste del modelo
summary(modelo.pls.train)

validationplot(modelo.pls.train, val.type = "MSEP")

pred.modelo.pls <- predict(modelo.pls.train, datos.test, ncomp = 3)
test.MSE.pls <- mean((pred.modelo.pls - datos.test$Apps)^2)
test.MSE.pls

modelo.pls <- plsr(Apps ~ ., data = datos, scale = TRUE, ncomp = 3)
summary(modelo.pls)


modelo <- c("Best subset", "Ridge regression", "Lasso", "PCR", "PLS")
test.MSE <- c(test.MSE.subset, test.MSE.ridge, test.MSE.lasso, test.MSE.pcr, test.MSE.pls)
comparacion <- data.frame(modelo, test.MSE)
comparacion


library(ggplot2)
ggplot(data = comparacion, aes(x = reorder(x = modelo, X = test.MSE), 
                               y = test.MSE)) +
  geom_bar(stat = "identity", aes(fill = modelo)) +
  labs(x = "Modelo regresi�n", y = "Test error(MSE)") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none")
