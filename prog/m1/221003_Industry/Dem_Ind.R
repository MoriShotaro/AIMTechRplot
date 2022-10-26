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


# Import ------------------------------------------------------------------

dev_oin <- rgdx.set(paste0(ddir,"/serv_global_SSP2.gdx"),"DEV_OIN")
dev_oin_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"dev_oin_t")
ind_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"ind_t")
serv_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"serv_t")

df_oin <- dev_oin_t %>%
  group_by(DEV_OIN,t) %>% 
  summarise(Value=sum(dev_oin_t)) %>% 
  mutate(t=as.numeric(as.character(t)))

df_serv <- serv_t %>%
  filter(I=="OIN") %>%
  select(-I) %>% 
  group_by(J,t) %>% 
  summarise(Value=sum(serv_t)) %>% 
  mutate(t=as.numeric(as.character(t)))

var_serv <- df_serv %>% 
  distinct(J)

g <- df_oin %>% 
  ggplot() +
  geom_path(aes(x=t,y=Value,group=DEV_OIN,color=DEV_OIN))
plot(g)

g <- df_serv %>% 
  ggplot() +
  geom_path(aes(x=t,y=Value,group=J,color=J))
plot(g)

df_svsh <-df_serv %>%
  pivot_wider(names_from=J,values_from=Value) %>% 
  mutate(OI_TOT=OI_BOL+OI_HET+OI_MAC+OI_ELE+OI_OTH) %>%
  mutate(across(c(OI_BOL,OI_HET,OI_MAC,OI_ELE,OI_OTH,OI_TOT),~./OI_TOT)) %>% 
  pivot_longer(cols=c(2:7),names_to="Variable",values_to="Value")

g <- df_svsh %>% 
  ggplot() +
  geom_path(aes(x=t,y=Value,group=Variable,color=Variable))
plot(g)


# hogehoge ----------------------------------------------------------------

test1 <- df_oin %>% 
  group_by(t) %>% 
  summarise(Value1=sum(Value))

test2 <- df_serv %>%
  group_by(t) %>%
  summarise(Value2=sum(Value))

test <- full_join(test1,test2) %>% 
  mutate(Value=Value1-Value2)
max(test$Value)

test3 <- df_oin %>% 
  rename(Value1=Value) %>% 
  rename(Variable=DEV_OIN)

test4 <- df_serv %>%
  rename(Value2=Value) %>% 
  mutate(J=recode(J,OI_BOL="BL",OI_HET="FR",OI_MAC="MT",OI_ELE="EL",OI_OTH="OT")) %>% 
  rename(Variable=J)

test5 <- full_join(test3,test4) %>% 
  mutate(Value=Value1-Value2)

