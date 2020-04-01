library(dplyr)
head(trees)

glimpse(trees) #bien clasificadas
summary(trees)
# Volume es la variable de interés
### Correlaciones
corr=round(cor(trees),2)

library(reshape2)
# Separamos la variable corr para poder graficarla
corr1 <- melt(corr)
head(corr1)


library(heatmaply)

ggheatmap <- ggplot(corr1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(-0.2, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


library(psych)
pairs.panels(trees, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#alta correlación entre VOlume y Girth

########### Modelo Lineal

modelo_lineal=lm(Volume~Girth,data=trees)
summary(modelo_lineal)

#Girth es muy significativa, se posee un R2 muy alto (93.53)

# Intervalos de confianza
confint(modelo_lineal)

#Representación gráfica

ggplot(data = trees, mapping = aes(x = Girth, y = Volume)) +
  geom_point(color = "firebrick", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Volumen ~ Diámetro", x = "Diámetro", y = "Volumen") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 

###############Supuestos del modelo


#### Linealidad de los predictores

library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = trees, aes(Girth, modelo_lineal$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1)



########### COntrastes ################

########### Normalidad ############


###### Muestra pequeña
#H0: Los residuos son normales
dim(trees)
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

# Los residuos son normales

########### Homocedasticidad (Breush-pagan)

library(lmtest)
#Si el p-valor es grande aceptaríamos que hay igualdad de varianzas.
# H0: Homocedasicidad (ésto es, la varianza de los residuos es constante)

bptest(modelo_lineal, studentize = FALSE)
ggplot(data = trees, aes(modelo_lineal$fitted.values, modelo_lineal$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()



########## Autocorrelación (durbin-Watson)


dwtest(modelo_lineal, alternative= "two.sided")
acf(modelo_lineal$residuals)
#Si el p-valor es pequeño rechazaríamos la hipótesis de independencia.



########### valores influyentes

# Residuos estudentizados
studentized_residual <- rstudent(modelo_lineal)
which(abs(studentized_residual) > 3)


library(car)
summary(influence.measures(model = modelo_lineal))

influencePlot(model = modelo_lineal)

#Las observaciones 20 y 31 son influyentes 
# Se ajusto el modelo sin estas observaciones, el modelo no cambio.
