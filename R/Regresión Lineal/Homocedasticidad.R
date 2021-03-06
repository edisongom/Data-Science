library(lmtest)
#Si el p-valor es grande aceptar�amos que hay igualdad de varianzas.
#H0: Homocedasicidad (�sto es, la varianza de los residuos es constante)
bptest(ml, studentize = FALSE)
ggplot(data = iris, aes(ml$fitted.values, ml$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()
