#library--------------------------------------------------------------------

library(gdxrrw)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(cowplot)
library(fs)
library(readxl)
library(data.table)

igdx('C:/GAMS/win64/26.1')


# input data --------------------------------------------------------------

# Variable list
Variables <- read.csv('../data/variable.csv',header=F) %>% 
  rename("Variable2"=V1,"Variable"=V2) %>% 
  filter(!str_detect(Variable2,"^Lan")) %>% 
  filter(!str_detect(Variable2,"^Agr")) %>% 
  filter(!str_detect(Variable2,"^AGMIP")) %>% 
  filter(!str_detect(Variable2,"^Wat")) %>% 
  filter(!str_detect(Variable2,"^Red_Cos")) %>% 
  filter(!str_detect(Variable2,"^Trd")) %>% 
  filter(!str_detect(Variable2,"^Tec")) %>% 
  filter(!str_detect(Variable2,"^Cap")) %>% 
  filter(!str_detect(Variable2,"^Cum")) %>% 
  filter(!str_detect(Variable2,"^Car_Its")) %>% 
  filter(!str_detect(Variable2,"^Emi")) %>% 
  filter(!str_detect(Variable2,"^LCO")) %>% 
  filter(!str_detect(Variable2,"^Lif")) %>% 
  filter(!str_detect(Variable2,"^OM")) %>% 
  filter(!str_detect(Variable2,"^Eff")) %>% 
  filter(!str_detect(Variable2,"^Res")) %>% 
  filter(!str_detect(Variable2,"^Frc")) %>% 
  filter(!str_detect(Variable2,"^Ful"))
Varlist <- Variables$Variable
Varlist2 <- Variables$Variable2

df1 <- df %>%
  filter(Variable%in%Varlist) %>% 
  inner_join(df,Variables,by="Variable") %>% 
  mutate(across(where(is.numeric),~replace_na(.x,0))) %>%
  unite(col=Name,Model,Scenario,sep="|",remove=FALSE) %>%
  select(1:3,8,4,5:7)

#Input IPCC AR6 Scenario Database
df <- fread('../data/AR6_Scenario_Database.csv',stringsAsFactors=FALSE,header=TRUE) %>% 
  pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit),names_to='Year',values_to='Value') %>%
  filter(Year%in%seq(2010,2100,5),Variable%in%Varlist) %>%
  select(1,2,4,6,7) %>%

