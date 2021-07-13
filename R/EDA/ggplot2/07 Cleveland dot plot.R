require(gcookbook)
require(ggplot2)     

# 
tophit=tophitters2001[1:25,]                        
nameorder=tophit$name[order(tophit$lg,tophit$avg)]  
tophit$name1=factor(tophit$name,levels=nameorder)   

#
ggplot(tophit,aes(x=avg,y=name1,colour=lg))+            
  geom_segment(aes(yend=name),xend=0,size=0.5)+      # Agregar segmentos    
  geom_point()+                                      # Agregar puntos
  facet_grid(lg ~ .,scales='free_y',space='free_y')+ # Separar
  theme_bw(base_family='Helvetica')+                 # aplicar tema
  theme(legend.position='none')+                      # Eliminar leyendas