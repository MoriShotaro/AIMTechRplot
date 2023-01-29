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
  pivot_wider(names_from=Variable,values_from=Value)

# historical non energy use
serv_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"serv_t") %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(t=as.numeric(t)) %>%
  filter(I=="NEN") %>% 
  select(-I,-J) %>% 
  rename("R33"=R,"Y"=t)

serv_GDP <- full_join(ind_t,serv_t) %>% 
  unite(col=R33_Y,R33,Y,sep="-",remove=FALSE) %>% 
  filter(Y<=2015) %>% 
  mutate(GDP_CAP=GDP_MER/POP,serv_CAP=serv_t/POP)


# Chemical product demand in Mt product
serv_Chem <- df_ToAIMTech_Chem %>%
  full_join(ind_t) %>%
  filter(Y<=2015) %>% 
  transmute(R33,Y,Product,GDP_CAP=GDP_MER*10^3/POP,Value=Value*10^6/POP) %>% 
  group_by(Product) %>% 
  nest()

# Mapping R33 to R17
R33_17 <- read.delim(paste0(ddir,"/region17_enduse.map"),header=F,sep='\t') %>% 
  select(-V2) %>% rename(R33=1,R17=2)
R33_17[30,1] <- "XE3"; R33_17[33,1] <- "XNF"

serv_Chem17 <-  df_ToAIMTech_Chem %>%
  full_join(ind_t) %>%
  filter(Y<=2015) %>%
  left_join(R33_17) %>% 
  group_by(R17,Y,Product) %>% 
  summarise(Value=sum(Value),POP=sum(POP),GDP=sum(GDP_MER)) %>% 
  transmute(R17,Y,Product,GDP_CAP=GDP*10^3/POP,Value=Value*10^6/POP) %>% # USD/cap, kg/cap
  group_by(Product) %>% 
  nest()

g <- serv_Chem17 %>%
  unnest(data) %>%
  group_by(Product) %>% 
  mutate(Max=max(Value)) %>% 
  ggplot() +
  geom_point(aes(x=GDP_CAP,y=Value,color=R17)) +
  facet_wrap(vars(Product),scales="free")
plot(g)

ggsave(paste0(odir,"/Chemical_demand.png"),g,width=10,height=6)


# Regression --------------------------------------------------------------

df <- serv_Chem17 %>%
  mutate(
    alpha=map(data,~{summary(nls(Value~a*exp(-b/GDP_CAP),start=c(a=100,b=10000),data=.))$coefficients[1]}),
    beta=map(data,~{summary(nls(Value~a*exp(-b/GDP_CAP),start=c(a=100,b=10000),data=.))$coefficients[2]}))%>%
  mutate(plot=pmap(list(data,alpha,beta,Product),~{
    hoge <- data.frame(y=..1$Value,ypred=..2*exp(-..3/..1$GDP_CAP))
    ggplot(..1) +
      geom_path(aes(x=GDP_CAP,y=Value,color=R17)) +
      stat_function(fun=function(x) ..2*exp(-..3/x)) +
      annotate('text',x=Inf,y=-Inf,label=paste0('Rsquared = ',summary(lm(hoge$ypred~hoge$y))$r.squared),hjust=1,vjust=-1) +
      xlim(0,10^5) +
      labs(title=paste(..4),y='Chemical Production') +
      theme(legend.position = "none")
  }))
map2(df$Product,df$plot,~ggsave(paste0(odir,"/Chem_GDP","/logistic_",..1,"_R17.png"),..2,width=4,height=4))


df <- serv_Chem %>%
  mutate(
    alpha=map(data,~{summary(nls(Value~a*exp(-b/GDP_CAP),start=c(a=100,b=10000),data=.))$coefficients[1]}),
    beta=map(data,~{summary(nls(Value~a*exp(-b/GDP_CAP),start=c(a=100,b=10000),data=.))$coefficients[2]}))%>%
  mutate(plot=pmap(list(data,alpha,beta),~{
    hoge <- data.frame(y=..1$Value,ypred=..2*exp(-..3/..1$GDP_CAP))
    ggplot(..1) +
      geom_path(aes(x=GDP_CAP,y=Value,color=R33)) +
      stat_function(fun=function(x) ..2*exp(-..3/x)) +
      annotate('text',x=Inf,y=-Inf,label=paste0('Rsquared = ',summary(lm(hoge$ypred~hoge$y))$r.squared),hjust=1,vjust=-1) +
      xlim(0,10^5)
  }))
map2(df$Product,df$plot,~ggsave(paste0(odir,"/Chem_GDP","/logistic_",..1,"_R33.png"),..2,width=10,height=6))

# Future extension --------------------------------------------------------

param_Chem <- df %>% 
  select(Product,alpha,beta) %>% 
  mutate(alpha=unlist(alpha),beta=unlist(beta))

serv_future <- df_ToAIMTech_Chem %>% 
  left_join(ind_t) %>% left_join(param_Chem) %>%
  mutate(GDP_CAP=GDP_MER*10^3/POP) %>% select(-GDP_MER) %>% 
  mutate(Value_e=alpha*exp(-beta/GDP_CAP)*POP/10^6) %>% 
  select(R33,Y,Product,Value,Value_e) %>% 
  mutate(Value_e2=ifelse(Y>=2015,Value_e,Value))
  
g <- serv_future %>% 
  group_by(Y,Product) %>% 
  summarise(Value=sum(Value),Value_e=sum(Value_e),Value_e2=sum(Value_e2)) %>%
  pivot_longer(cols=c(Value,Value_e,Value_e2),names_to="Variable",values_to="Value") %>% 
  ggplot() +
  geom_path(aes(x=Y,y=Value,color=Variable)) +
  labs(x="Year",y="Production (Mt)") +
  facet_wrap(vars(Product))
ggsave(paste0(odir,"/future_extension_global.png"),g,width=10,height=6)

g <- serv_future %>% 
  pivot_longer(cols=c(Value,Value_e,Value_e2),names_to="Variable",values_to="Value") %>%
  filter(Variable=="Value_e2") %>% 
  ggplot() +
  geom_path(aes(x=Y,y=Value,color=R33)) +
  labs(x="Year",y="Production (Mt)") +
  facet_wrap(vars(Product)) +
  xlim(c(2005,2030))
ggsave(paste0(odir,"/future_extension_R33.png"),g,width=10,height=6)

# Others ------------------------------------------------------------------
# 
# demp <- c(1.0,1.25,1.5)
# 
# df_serv <- tibble(Sc=demp,dt=rep(list(serv_Chem17),length(demp))) %>% unnest(dt)
# 
# df <- df_serv %>%
#   mutate(
#     alpha=map2(data,Sc,~{glm(..1$Value/max(..1$Value)/..2~..1$GDP_CAP,binomial())$coefficients[2]}),
#     beta=map2(data,Sc,~{glm(..1$Value/max(..1$Value)/..2~..1$GDP_CAP,binomial())$coefficients[1]})) %>% 
#   mutate(plot=pmap(list(data,alpha,beta,Sc),~{
#     ggplot(..1) +
#       geom_point(aes(x=GDP_CAP,y=Value,color=R17)) +
#       stat_function(fun=function(x) max(..1$Value)*..4/(1+exp(-(..2*x+..3)))) +
#       xlim(0,5*10^5)
#   }))
