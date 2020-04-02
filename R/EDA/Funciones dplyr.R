# Seleccionamos los 5 primeros lirios de cada una de las tres
#especies.
lirios=iris[c(1:5,51:55,101:105),]



#############Seleccionar filas: filter()##################

#seleccionar las observaciones (filas) que cumplen las condiciones
#que nos interesan.
library(dplyr)
#attach(lirios)
filter(lirios, Species=='setosa')

filter(lirios,Species=="setosa" | Species=="virginica")

filter(lirios,Species=="setosa", Sepal.Length<5)

# "|" es el simil de "or" y "," de "and"

############ Seleccionar columnas: select() ################

#Esta acción consiste en elegir un subconjunto de las 
#variables (columnas) del fichero. Por ejemplo,

select(lirios,Sepal.Length,Petal.Length)

#seleccionar un rango de columnas
select(lirios, Petal.Length:Sepal.Length)

#Excluir columna
select(lirios, -Species)

#seleccionar las variables cuyo nombre contenga ciertos términos:
select(lirios, contains('Petal'))

#En lugar de contains, se puede hacer un uso similar con las 
#siguientes expresiones: starts_with, ends_with o matches.



################Ordenar: arrange()##################

#Ordena las filas de menor a mayor valor de la variable elegida. Si escribimos un signo menos, ordena de mayor a menor. Por ejemplo para ordenar de acuerdo con la longitud del sépalo
#Si escribimos un signo menos, ordena de mayor a menor

arrange(lirios,Sepal.Length)

arrange(lirios,-Sepal.Length)

#Es posible ordenar respecto de una variable y resolver los 
#empates de acuerdo con una segunda variable

arrange(lirios, Species,Sepal.Length)



################# Sintaxis en cadena ###################

# Permite escribir las acciones en un orden natural,
#en oposición a la forma anidada en la que lo haríamos
#normalmente. Primero se escribe el nombre del fichero y luego 
#las acciones en el orden en que se realizan separadas por el
#operador %>%(que podríamos leer como entonces). Por ejemplo, 
#si queremos seleccionar las variables que contienen las medidas
#del pétalo, seleccionar los lirios para los que la longitud del
#pétalo es mayor que 4 mm y ordenarlos de menor a mayor longitud
#del pétalo


lirios %>%
  select(contains("Petal")) %>%
  filter(Petal.Length>4) %>%
  arrange(Petal.Length)

# lo anterior es igual al anidado siguiente
arrange(filter(select(lirios, contains('Petal')), Petal.Length > 4), Petal.Length)


################# Añadir nuevas variables: mutate() ###########


#Veamos como crear nuevas variables que son función de las ya
#existentes. En este ejemplo creamos una variable que corresponde 
#al cociente entre la anchura y la longitud del pétalo (que
#podría corresponder a algún aspecto de la forma del pétalo)


lir2=lirios %>%
  mutate(forma=Petal.Width/Petal.Length)




 
########     Resumir (subconjuntos de) variables: ############
##########          group_by() + summarise()   ###############


#Usamos summarise() para aplicar comandos a variables. 
#Normalmente se usa en combinación con group_by() de manera
#que se calculen estadísticos para subgrupos de observaciones.
#En el siguiente ejemplo se calcula la media de la longitud del
#pétalo para los lirios de cada una de las especies


lirios %>%
  group_by(Species) %>%
  summarise(mean(Sepal.Width))


#Una variación viene dada por la acción summarise_each() en la
#que se consideran varias variables a la vez. Un ejemplo es el
#siguiente, en el que se calculan las medias de cada una de las
#medidas del pétalo y para cada una de las tres especies

lirios %>%
  group_by(Species) %>%
  summarise_each(funs(mean), contains("Petal"))


###### Para extraer aleatoriamente algunas observaciones #####
# SIN REEMPLAZO
lirios %>%
  sample_n(3)

#CON REEMPLAZO, EXTRAER UN 25% DE LA MUESTRA

lirios %>% sample_frac(0.25,rep=T)


# Ver estructura de la base

glimpse(lirios)

