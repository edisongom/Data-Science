
devtools::install_github("easystats/report")
library(report)

#Reporte en forma de texto
model <- t.test(Sepal.Length ~ Species, data = iris[1:100, ])
r <- report(model)
r
summary(r)

#Reporte en forma de tablas
as.data.frame(r)
summary(as.data.frame(r))
as.list(r)


## correlacion
library(dplyr)
cor.test(iris$Sepal.Length, iris$Petal.Length) %>% 
  report()


# Table reporte para una regresión lineal
tt=lm(Sepal.Length ~ Petal.Length + Species, data=iris) %>% 
  report() %>% 
  table_short()

tt

#EJEMPLOS

#t-test y correlacion
t.test(mtcars$mpg ~ mtcars$am) %>% 
  report()
