library(ggplot2)
library(viridis)


se_map<-se_map %>% mutate(Longitude=long,Latitude=lat)
predict_infection_clipped<-data.frame()
groups<-unique(se_map$group)

for(g in 1:length(groups))
{
  temp<-predict_infection[with(predict_infection %>% dplyr::select("Longitude","Latitude"), 
                               inSide(se_map %>% filter(group==groups[g]) %>% dplyr::select("Longitude","Latitude"),Longitude,Latitude)),]
  predict_infection_clipped<-rbind(predict_infection_clipped,temp)
  
}

ggplot(aes(Longitude, Latitude,  fill= model_fit),
       data=predict_infection_clipped %>% filter(Year.Sampled==2011))+
  geom_tile()+
  #facet_wrap(~Year.Sampled,nrow=2)+
  scale_fill_viridis("Infection Probability")+
  geom_polygon(data=se_map,aes(x=long,y=lat,group=group),color="black",inherit.aes = FALSE,alpha=0)+
  theme_classic()


