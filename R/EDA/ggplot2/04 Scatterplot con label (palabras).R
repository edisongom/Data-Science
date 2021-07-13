####### Scatterplot con labels
mtcars$cyl1=factor(mtcars$cyl)
ggplot(data=mtcars,aes(x=disp,y=mpg,colour=cyl1))+
  geom_text(aes(label=rownames(mtcars),x=disp+5),hjust=0,check_overlap = T)+
  geom_point()+
  expand_limits(xend=600)
