library(gdxrrw)
library(tidyverse)

df_all <- rgdx.param("data/model_expansion/2208231844/gdx_primary/Baseline.gdx","vx_l")
region_all <- c("JPN","CHN","IND","IDN","KOR","THA","MYS","VNM","XSE","XSA","XEA","XCS","XME",
                "AUS","NZL","XOC","CAN","USA","XE15","XE10","XE2","TUR","XEWI","XEEI","XENI","RUS","MEX","ARG",
                "BRA","XLM","ZAF","XAF","XASIA")

df <- df_all %>% 
  rename("Region"=1,"Sector"=2,"Variable"=3,"Year"=4,"Value"=5) %>% 
  mutate(Year=as.numeric(as.character(Year))) %>% 
  filter(Sector=="OIL",Region%in%region_all) %>%
  select(-Sector) %>% 
  group_by(Variable,Year) %>% 
  summarise(Value=sum(Value)) %>% 
  filter(Variable%in%c("OIL_CTL","OIL_GTL")) %>% 
  filter(Year>=2020) %>% 
  mutate(flag=5*floor(Year/5-404)+2020) %>%
  group_by(Variable,flag) %>% 
  summarise(Value=mean(Value)) %>% 
  rename("Year"="flag")

g <- df %>% 
  ggplot() +
  geom_line(aes(x=Year,y=Value,group=Variable,color=Variable))
plot(g)

if(!dir.exists(paste(odir,"others",sep="/"))){dir.create(paste(odir,"others",sep="/"),recursive=T)}
ggsave(paste0("output/model_expansion/",date,"/others/CTLGTLvx.png"),g,width=10,height=6)
       
