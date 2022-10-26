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


# Input data --------------------------------------------------------------

# historical GDP
ind_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"ind_t") %>% 
  rename("R33"=1,"Variable"=2,"Y"=3,"Value"=4) %>% 
  filter(Variable!="GDP_PPP") %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y)) %>%
  pivot_wider(names_from=Variable,values_from=Value) %>%
  transmute(R33,Y,GDP_CAP=GDP_MER/POP)

# historical non energy use
serv_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"serv_t") %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(t=as.numeric(t)) %>%
  filter(I=="NEN") %>% 
  select(-I,-J) %>% 
  rename("R33"=R,"Y"=t)

serv_GDP <- full_join(ind_t,serv_t) %>% 
  unite(col=R33_Y,R33,Y,sep="-",remove=FALSE) %>% 
  filter(Y<=2015)

serv_glo <- serv_t %>% 
  group_by(Y) %>% 
  summarize(serv_t=sum(serv_t))

ind_glo <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"ind_t") %>% 
  rename("R33"=1,"Variable"=2,"Y"=3,"Value"=4) %>% 
  filter(Variable!="GDP_PPP") %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Y=as.numeric(Y)) %>%
  pivot_wider(names_from=Variable,values_from=Value) %>%
  group_by(Y) %>% 
  summarise(POP=sum(POP),GDP=sum(GDP_MER))

serv_GDP_glo <- full_join(serv_glo,ind_glo) %>% 
  mutate(GDP_CAP=GDP/POP)

serv_shr <- serv_t %>% 
  filter(Y<=2015) %>% 
  group_by(Y) %>% 
  mutate(serv_shr=serv_t/sum(serv_t))

g <- ind_glo %>% 
  ggplot()+
  geom_point(aes(x=Y,y=GDP))
plot(g)

g <- serv_GDP_glo %>%
  ggplot() +
  geom_point(aes(x=GDP,y=serv_t,color=Y))
plot(g)

g <- serv_shr %>% 
  ggplot() +
  geom_path(aes(x=Y,y=serv_shr,color=R33))
plot(g)


# Regression --------------------------------------------------------------

serv_GDP_glo_2015 <- serv_GDP_glo %>% 
  filter(Y<=2015)

y <- serv_GDP_glo_2015$serv_t
x <- serv_GDP_glo_2015$GDP
plot(x,y)

# logisticModel <-  glm(y ~ x, binomial)
# summary(logisticModel)
# 
# a <- logisticModel$coefficients[1]
# b <- logisticModel$coefficients[2]
# 
# lineLogistic = function(x) 10000 * exp (a+b*x)/(1+exp(a+b*x))
# plot (lineLogistic,0,10000000000)
# 
# g <- serv_GDP %>%
#   ggplot() +
#   geom_point(aes(x=GDP_CAP,y=serv_t,color=Y))+
#   stat_function(fun=lineLogistic)
# plot(g)

# high demand
y <- serv_GDP_glo_2015$serv_t
x <- serv_GDP_glo_2015$GDP

plot(x,y)

fit <- lm(formula=y~x)
summary(fit)

a <- fit$coefficients[2]
b <- fit$coefficients[1]
line_fit_high <- function(x) a*x + b

plot(line_fit_high,0,100000)

# middle demand
y <- serv_GDP_glo_2015$serv_t/1e+06
x <- serv_GDP_glo_2015$GDP
fit_m <-  glm(y ~ x, binomial)


a2 <- fit_m$coefficients[1]
b2 <- fit_m$coefficients[2]
line_fit_middle <-  function(x) 100000 * exp (a2+b2*x)/(1+exp(a2+b2*x))
plot(line_fit_middle,0,1000000000)

serv_GDP_glo_2100 <- serv_GDP_glo %>% 
  select(Y,serv_t,GDP) %>% 
  mutate(serv_t_high=if_else(Y>2015,line_fit_high(GDP),serv_t)) %>% 
  mutate(serv_t_midddle=if_else(Y>2015,line_fit_middle(GDP),serv_t)) %>% 
  pivot_longer(cols=-c(Y,GDP),names_to="Scenario",values_to="serv_t")

g <- serv_GDP_glo_2100 %>% 
  ggplot()+
  geom_point(aes(x=Y,y=serv_t,color=Scenario))+
  scale_y_continuous(limits=c(0,NA))
plot(g)
  
