########## Scatterplot

library(ggplot2)
library(car)
library(ggthemes)


ggplot(Salaries,aes(x=yrs.since.phd,y=salary,fill=sex))+
  geom_smooth(method = lm,formula = y~poly(x,2))+
  geom_point(shape=21)+
  facet_grid(~sex)+
  theme_few()+
  scale_color_few()
str(Salaries)
