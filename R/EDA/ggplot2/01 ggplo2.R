# Creando un gráfico con ggplot2
library(ggplot2)
library(car) #base de salarios
str(Salaries)

###### DATA SALARIES

p=ggplot(data=Salaries, aes(y=yrs.service,x=salary))
p+geom_point() #Agregar nube de puntos

## Agregar filtro de sexo
p=ggplot((data=Salaries),aes(x=yrs.service,y=salary,fill=sex)) #asignar variable al color
p+geom_point(shape=21) #Agregar nube de puntos

# Agregar línea de regresión
p+geom_point(shape=21)+
  geom_smooth() # dos lineas, una para cada sexo

# Separar graficos por factor (sexo)
p+geom_point(shape=21)+
  geom_smooth()+
  facet_grid(~sex)



