######## Balloon Plot.
install.packages("gcookbook")
require(gcookbook)
require(ggplot2)
ggplot(subset(countries,Year==2009 & healthexp>2000),
       aes(x = healthexp, y = infmortality, size = GDP))+
  geom_point(shape = 21, color = "black", fill = "cornsilk") + 
  xlim(0, 11000)+ scale_size_area(max_size = 15) +
  geom_text(aes(label = Name), size = 4,  hjust = -0.3)
