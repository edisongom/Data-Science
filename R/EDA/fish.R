


setwd("C:/Users/ediso/Downloads/Notebooks/R/Regresión Lineal")
fish=read.csv("fish.csv")
head(fish)
# La primera columna esta mal escrita, la renombramos
colnames(fish)[1]="Species"
attach(fish)
glimpse(fish)
summary(fish)
table(is.na(fish))

# No hay valores missing

names(fish)

## Variación de Weigth vs Specie

id=data.frame(ID=1:nrow(fish))
fish=cbind(fish,id)

p1<-ggplot(fish, aes(x=ID, y=Weight, color=Species)) + 
  geom_point() + geom_smooth()
p1

### Variación length vs specie

p2<-ggplot(fish, aes(x=ID, y=Length1, color=Species)) + 
  geom_point() + geom_smooth()
p3<-ggplot(fish, aes(x=ID, y= Length2, color=Species)) +
  geom_point() + geom_smooth()
p4<-ggplot(fish, aes(x=ID, y= Length3, color=Species)) +
  geom_point() + geom_smooth()
grid.arrange(p2, p3,p4, nrow=3)


## Variación Height
p5<-ggplot(fish, aes(x=ID, y=Height, color=Species)) + 
  geom_point() + geom_smooth()
p5


# En todos los casos se ve que un dato de la Specie "perch" se escapa de los demás
#valores



######## Correlaciones
datanum=fish[,2:7]
corr=round(cor(datanum),2)

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

