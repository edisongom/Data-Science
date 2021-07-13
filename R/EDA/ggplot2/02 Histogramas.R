################ Histograma
library(car)
library(ggplot2)

####### Univariable
ggplot(Salaries,aes(x=salary,y=..density..))+
  geom_histogram(colour="grey60",fill="cornsilk")+
  geom_density(colour=NA,fill="blue",alpha=0.2)+
  geom_line(stat="density",colour="red")+
  xlim(45000,250000)

######## COmparar niveles

ggplot((Salaries),aes(x=salary,fill=rank))+
  geom_density(alpha=0.4)
  
###### Separar niveles
ggplot((Salaries),aes(x=salary,fill=rank))+
  geom_density(alpha=0.4)+
  facet_grid(~rank)
