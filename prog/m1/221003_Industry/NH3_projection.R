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



# input data --------------------------------------------------------------

# Japan(COUNTRY) to JAPAN(IEA) mapping
iea_country <- read.delim(paste0(ddir,"/ieacountry.set"),header=FALSE,sep="\t") %>%
  rename(IEA=1,COUNTRY=2)

# JAPAN(IEA) to JPN(R32) mapping
iea_R32 <- read.delim(paste0(ddir,"/iea32.map"),header=FALSE,sep=".") %>% 
  rename(IEA=1,R32=2) %>% 
  mutate(IEA=str_replace_all(IEA," ","")) %>%
  mutate(IEA=str_replace_all(IEA,"\t",""))

# Japan(COUNTRY) to JPN(R32) mapping
country_R32 <- full_join(iea_country,iea_R32) %>%
  select(COUNTRY,R32) %>% 
  mutate(COUNTRY=recode(COUNTRY,"China (P.R. of China and Hong Kong  China)"="China")) %>%
  rename(Country=COUNTRY) %>% 
  drop_na() %>% filter(R32!="INTL")

# historical GDP
ind_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"ind_t") %>% 
  rename("R33"=1,"Variable"=2,"Y"=3,"Value"=4) %>% 
  filter(Variable!="GDP_PPP") %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y)) %>%
  pivot_wider(names_from=Variable,values_from=Value) %>% 
  rename(R32=R33)

# Chemical product demand in Mt product
serv_Chem <- df_ToAIMTech_Chem %>%
  filter(Y<=2015) %>% 
  transmute(R33,Y,Product,Value) %>% 
  filter(Product=="Ammonia") %>% 
  select(R33,Y,Value) %>% 
  rename(R32=R33,Prod=Value)

# unit thousand tonnes nutrients
df_NH3 <- readLines(paste0(ddir,"/IFA_NH3.csv")) %>%
  str_replace_all(",",".") %>% 
  paste(collapse='\n') %>%
  fread(sep=";") %>% 
  select(-24,-25) %>%
  pivot_longer(cols=-c(Country,Product),names_to="Y",values_to="Value") %>%
  filter(Product=="Grand Total N",Y>=2005,Y<=2015) %>% 
  left_join(country_R32) %>% 
  replace_na(list(Value=0)) %>% 
  drop_na(R32) %>%
  group_by(R32,Y) %>% 
  summarise(Value=sum(Value)) %>%
  mutate(Y=as.numeric(Y)) %>% 
  left_join(ind_t) %>% left_join(serv_Chem)

df_NH3 <- readLines(paste0(ddir,"/IFA_95_20.csv")) %>%
  str_replace_all(",",".") %>% 
  paste(collapse='\n') %>%
  fread(sep=";") %>% 
  select(-29,-30) %>%
  pivot_longer(cols=-c(Country,Product),names_to="Y",values_to="Value") %>%
  filter(Product=="Grand Total N",Y>=2005,Y<=2015) %>% 
  left_join(country_R32) %>% 
  replace_na(list(Value=0)) %>% 
  drop_na(R32) %>%
  group_by(R32,Y) %>% 
  summarise(Value=sum(Value)) %>%
  mutate(Y=as.numeric(Y)) %>% 
  left_join(ind_t) %>% left_join(serv_Chem)

hoge <- df_NH3 %>% 
  group_by(Y) %>% 
  summarise(Value=sum(Value),POP=sum(POP))

plot(hoge$POP,hoge$Value)

g <- df_NH3 %>% 
  ggplot() +
  geom_point(aes(x=GDP_MER/POP,y=Value/POP,color=R32))
plot(g)

g <- df_NH3 %>% 
  ggplot() +
  geom_point(aes(x=POP,y=Value,color=R32))
plot(g)

g <- df_NH3 %>% 
  ggplot() +
  geom_point(aes(x=GDP_MER,y=Value/POP,color=R32))
plot(g)

g <- df_NH3 %>% 
  ggplot() +
  geom_point(aes(x=Value,y=Prod,color=R32))
plot(g)

hoge <- ind_t %>%
  group_by(Y) %>% 
  summarise(POP=sum(POP))
plot(hoge$Y,hoge$POP)

