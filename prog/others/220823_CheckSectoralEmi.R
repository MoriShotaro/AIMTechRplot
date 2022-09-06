library(tidyverse)
library(gdxrrw)

df_Emi <- df_all %>% 
  filter(Variable%in%c('Emi_CO2_Ene_Sup','Emi_CO2_Ene_Dem_Ind','Emi_CO2_Ene_Dem_Res_and_Com',
                       'Emi_CO2_Ene_Dem_Tra','Emi_CO2_Ene_Sup_Ele')) %>%
  pivot_wider(names_from=Variable,values_from=Value) %>% 
  mutate(Emi_CO2_Ene_Sup_notEle=Emi_CO2_Ene_Sup-Emi_CO2_Ene_Sup_Ele) %>% 
  pivot_longer(cols=-c(1:3),names_to="Variable",values_to="Value") %>% 
  mutate(Variable=recode(Variable,"Emi_CO2_Ene_Sup"="Energy Supply","Emi_CO2_Ene_Sup_Ele"="Power generation","Emi_CO2_Ene_Sup_notEle"="Energy Supply (w/o Ele)","Emi_CO2_Ene_Dem_Ind"="Industry",
                         "Emi_CO2_Ene_Dem_Res_and_Com"="Buildings","Emi_CO2_Ene_Dem_Tra"="Transport")) %>%
  mutate(Variable=factor(Variable,levels=c("Energy Supply","Power generation","Energy Supply (w/o Ele)","Industry","Buildings","Transport"))) %>%
  mutate(Year=as.numeric(as.character(Year))) %>% 
  filter(Scenario!="historical")

g <- df_Emi %>% 
  ggplot() +
  geom_line(aes(x=Year,y=Value,group=Scenario,color=Scenario)) +
  facet_wrap(vars(Variable))
plot(g)

if(!dir.exists(paste(odir,"others",sep="/"))){dir.create(paste(odir,"others",sep="/"),recursive=T)}
ggsave(paste0("output/model_expansion/",date,"/others/Emi_Sectoral.png"),g,width=10,height=6)
