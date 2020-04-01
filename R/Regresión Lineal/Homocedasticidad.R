library(lmtest)
#Si el p-valor es grande aceptaríamos que hay igualdad de varianzas.
#H0: Homocedasicidad (ésto es, la varianza de los residuos es constante)
bptest(ml, studentize = FALSE)
ggplot(data = iris, aes(ml$fitted.values, ml$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()
