library(tidyverse)
library(gdxrrw)


df_oil <- df_all %>% 
  filter(Variable=="Prm_Ene_Oil") %>% 
  select(Year,Value,Scenario) %>%
  mutate(Year=as.numeric(as.character(Year))) %>% 
  filter(Year>=2020) %>%
  rename("totvar"=Value)

df_vx1 <- rgdx.param(paste(cdir,"data",project,date,"gdx_primary","Baseline.gdx",sep="/"),"vx_l") %>%
  filter(L%in%c("OIL_CTL","OIL_CTLX","OIL_GTL","OIL_GTLX","OIL_WOL_OILM1","OIL_WOLX_OILM1")) %>% 
  mutate(Scenario="Baseline")
# df_vx2 <- rgdx.param(paste(cdir,"data",project,date,"gdx_primary","SR15_HOS.gdx",sep="/"),"vx_l") %>% 
#   filter(L%in%c("OIL_CTL","OIL_CTLX","OIL_GTL","OIL_GTLX","OIL_WOL_OILM1","OIL_WOLX_OILM1")) %>% 
#   mutate(Scenario="SR15_HOS")
# df_vx3 <- rgdx.param(paste(cdir,"data",project,date,"gdx_primary","SR15_LOS.gdx",sep="/"),"vx_l") %>% 
#   filter(L%in%c("OIL_CTL","OIL_CTLX","OIL_GTL","OIL_GTLX","OIL_WOL_OILM1","OIL_WOLX_OILM1")) %>% 
#   mutate(Scenario="SR15_LOS")

vx_l <- df_vx1
# vx_l <- bind_rows(df_vx1,df_vx2,df_vx3)

df_a1 <- rgdx.param(paste(cdir,"data",project,date,"gdx_primary","Baseline.gdx",sep="/"),"a_t") %>% 
  filter(L%in%c("OIL_CTL","OIL_CTLX","OIL_GTL","OIL_GTLX","OIL_WOL_OILM1","OIL_WOLX_OILM1")) %>% 
  mutate(Scenario="Baseline")
# df_a2 <- rgdx.param(paste(cdir,"data",project,date,"gdx_primary","SR15_HOS.gdx",sep="/"),"a_t") %>% 
#   filter(L%in%c("OIL_CTL","OIL_CTLX","OIL_GTL","OIL_GTLX","OIL_WOL_OILM1","OIL_WOLX_OILM1")) %>% 
#   mutate(Scenario="SR15_HOS")
# df_a3 <- rgdx.param(paste(cdir,"data",project,date,"gdx_primary","SR15_LOS.gdx",sep="/"),"a_t") %>% 
#   filter(L%in%c("OIL_CTL","OIL_CTLX","OIL_GTL","OIL_GTLX","OIL_WOL_OILM1","OIL_WOLX_OILM1")) %>% 
#   mutate(Scenario="SR15_LOS")

a_t <- df_a1
# a_t <- bind_rows(df_a1,df_a2,df_a3)

df_prm <- full_join(vx_l,a_t) %>% 
  select(R,L,H,J,vx_l,a_t,Scenario) %>% 
  mutate(Value=vx_l*a_t) %>%
  replace_na(list(Value=0))
  

df <- df_prm %>% 
  select(R,H,L,Value,Scenario) %>% 
  group_by(H,L,Scenario) %>% 
  summarise(Value=sum(Value)) %>%
  group_by(H,Scenario) %>% 
  mutate(totvar=sum(Value)) %>% 
  pivot_wider(names_from=L,values_from=Value) %>% 
  mutate(across(c("OIL_CTL","OIL_CTLX","OIL_GTL","OIL_GTLX","OIL_WOL_OILM1"),~.x/totvar)) %>% 
  select(-totvar) %>%
  pivot_longer(cols=-c(H,Scenario),names_to="Variable",values_to="Value") %>% 
  rename("Year"=H) %>% 
  mutate(Year=as.numeric(as.character(Year))) %>% 
  filter(Year>=2020) %>% 
  mutate(flag=5*floor(Year/5-404)+2020) %>% 
  group_by(Variable,flag,Scenario) %>% 
  summarise(Value=mean(Value)) %>% 
  rename("Year"="flag") %>% 
  pivot_wider(names_from=Variable,values_from=Value) %>% 
  rename("Oil(CTL) w/o CCS"="OIL_CTL","Oil(CTL) w/ CCS"="OIL_CTLX","Oil(GTL) w/o CCS"="OIL_GTL","Oil(GTL) w/ CCS"="OIL_GTLX",
         "Oil"="OIL_WOL_OILM1") %>% 
  ungroup()

col <- c("Oil(CTL) w/o CCS"="grey50","Oil(CTL) w/ CCS"="grey30","Oil(GTL) w/o CCS"="lightgoldenrod","Oil(GTL) w/ CCS"="lightgoldenrod3",
         "Oil"="sandybrown")

df2 <- full_join(df,df_oil) %>% 
  mutate(across(-c(Year,Scenario),~.x*totvar)) %>% 
  select(-totvar) %>% 
  pivot_longer(cols=-c(Year,Scenario),names_to="Variable",values_to="Value")

df3 <- df %>% 
  pivot_longer(cols=-c(Year,Scenario),names_to="Variable",values_to="Value")

g <- df2 %>% 
  ggplot() +
  geom_area(aes(x=Year,y=Value,fill=Variable)) +
  scale_fill_manual(values=col) +
  facet_wrap(vars(Scenario))
plot(g)
ggsave(paste0(odir,"/others/CTLGTLarea.png"),g,width=10,height=6)

g <- df3 %>% 
  ggplot() +
  geom_bar(aes(x=Year,y=Value,fill=Variable),position="fill",stat="identity") +
  scale_fill_manual(values=col) +
  facet_wrap(vars(Scenario))
plot(g)
ggsave(paste0(odir,"/others/CTLGTLbar.png"),g,width=10,height=6)
