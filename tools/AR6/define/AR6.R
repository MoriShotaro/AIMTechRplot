#library--------------------------------------------------------------------

library(gdxrrw)
library(dplyr)
library(tidyr)
library(stringr)
library(cowplot)
library(fs)
library(readxl)
library(data.table)

# igdx('C:/GAMS/win64/26.1')
igdx('F:/smori/GAMS/38')
source('./Category.R')

# input data --------------------------------------------------------------

#Input IPCC AR6 Scenario Database
df <- fread('../data/AR6_Scenario_Database.csv',stringsAsFactors=FALSE,header=TRUE) %>% 
  pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit),names_to='Year',values_to='Value') %>%
  filter(Year%in%seq(2010,2100,5),Variable%in%Varlist) %>%
  select(1,2,4,6,7) %>% 
  inner_join(Variables,by="Variable") %>%
  mutate(across(where(is.numeric),~replace_na(.x,0))) %>%
  unite(col=Name,Model,Scenario,sep="|",remove=FALSE) %>% 
  select(1:4,7,5,6) %>%
  inner_join(scen_category,by="Name")
df$Year <- as.numeric(df$Year)

fwrite(df,"../output/ReadAR6.csv")

# dfAR6 <- read.csv('../data/AR6_Scenario_Database.csv')
#   mutate(across(where(is.numeric),~replace_na(.x,-1))) %>% 
#   pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit),names_to='Year',values_to='Value') %>% 
#   select(-5) %>%
#   drop_na(Variable) %>%
#   pivot_wider(names_from=Variable,values_from=Value, values_fill=0) %>% 
#   pivot_longer(cols=-c(Model,Scenario,Region,Year),names_to='Variable',values_to='Value') %>%
#   unite(col=SCENARIO,Model,Scenario,sep="/") %>% 
#   mutate(Variable=str_replace_all(Variable," ","")) %>% 
#   separate(col=Variable,into=c("x1","x2","x3"),sep="\\|") %>% 
#   mutate(across(4:6,~str_sub(.,start=1,end=3))) %>% 
#   unite(col=Variable,x1,x2,x3,sep="_") %>% 
#   mutate(across(Variable,~gsub("_NA","",.x))) %>% 
#   filter(Year%in%seq(2020,2100,5)) %>% 
#   mutate(Variable=str_replace_all(Variable,"Pri","Pri_Ene")) %>%
#   mutate(Variable=str_replace_all(Variable,"Sec","Sec_Ene")) %>%
#   mutate(Variable=str_replace_all(Variable,"Fin","Fin_Ene")) %>% 
#   mutate(Variable=str_replace_all(Variable,"Res","Res_and_Com")) %>% 
#   mutate(Region=str_replace_all(Region,"World","WOR"))
# dfAR6$Year <- as.numeric(dfAR6$Year)