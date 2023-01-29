# Option ------------------------------------------------------------------

general <- FALSE
index <- FALSE
bind_baseline <- TRUE

# library -----------------------------------------------------------------

library(tidyverse)
library(gdxrrw)
library(stringr)
library(patchwork)
library(readr)
library(scales)
library(data.table)
library(purrr)
library(readxl)
library(countrycode)

# Setting -----------------------------------------------------------------

# General information
date <- "m1"
project <- "221003_Industry"
cdir <- getwd()
odir <- paste(cdir,"output",date,project,sep="/")
ddir <- paste(cdir,"data",date,project,sep="/")
pdir <- paste(cdir,"prog",sep="/")
pdir2 <- paste(pdir,date,project,sep="/")
mdir <- paste(pdir,"inc_prog",sep="/")
tdir <- paste(cdir,"tools",sep="/")
othdir <- paste(pdir,"others",sep="/")
gdir <- "C:/GAMS/win64/26.1"

# Make input/output directory
if(!dir.exists(odir)){dir.create(odir,recursive=T)}
if(!dir.exists(ddir)){dir.create(ddir,recursive=T)}

reg_in <- "World"
year_all <- seq(2005,2100,by=5)

igdx(gdir)


# Input data --------------------------------------------------------------

df_BaU <- rgdx.param(paste0(ddir,"/20221110/Baseline.gdx"),"vx_l") %>% mutate(Scenario="Baseline")
df_500C <- rgdx.param(paste0(ddir,"/20221110/500C.gdx"),"vx_l") %>% mutate(Scenario="500C")


# yield data --------------------------------------------------------------



# data arrangement --------------------------------------------------------

df <- bind_rows(df_BaU,df_500C) %>% 
  filter(I=="NEN") %>% 
  group_by(L,H,Scenario) %>% 
  summarise(Value=sum(vx_l)) %>% 
  filter(!str_detect(L,"NEN")) %>% 
  mutate(H=as.numeric(as.character(H))) %>%
  pivot_wider(names_from=L,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(H,Scenario),names_to="L",values_to="Value") %>% 
  mutate(flag=5*floor(H/5-404)+2020) %>%
  group_by(L,flag,Scenario) %>% 
  summarise(Value=mean(Value)) %>% 
  rename("H"="flag") %>% 
  mutate(Product=case_when(
    str_detect(L,"NH3") ~ "NH3",
    str_detect(L,"MOH") ~ "MOH",
    str_detect(L,"HVC") ~ "HVC"
  )) %>% 
  mutate(Feedstock=case_when(
    str_detect(L,"BMS")  ~ "Biomass",
    str_detect(L,"NGSX") ~ "Gas w/ CCS",
    str_detect(L,"GAS")  ~ "Gas w/o CCS",
    str_detect(L,"NGS")  ~ "Gas w/o CCS",
    str_detect(L,"ELE")  ~ "Electrolysis"
  ))


# plot --------------------------------------------------------------------

g <- df %>%
  # filter(H%in%c(2010,2030,2050)) %>% 
  ggplot() +
  geom_bar(aes(x=H,y=Value,fill=Feedstock),stat="identity") +
  scale_fill_brewer(palette="YlGnBu") +
  facet_grid(rows=vars(Scenario),cols=vars(Product))
plot(g)

ggsave(paste0(odir,"/20221110.png"),width=8,height=4)
