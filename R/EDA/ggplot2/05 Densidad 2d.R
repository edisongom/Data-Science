########## 2D density plot

require(MASS)
require(ggplot2)

ggplot(geyser,aes(x=waiting,y=duration))+
  stat_density2d(aes(alpha=..density..),
                 geom="raster",contour=F)+ #transparencia del mapa
  geom_point()+
  expand_limits(x=35,yend=6)
