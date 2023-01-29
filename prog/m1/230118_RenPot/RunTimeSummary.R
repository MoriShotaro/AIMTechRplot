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
library(gdxdt)

# Setting -----------------------------------------------------------------

# General information
date <- "m1"
project <- "230118_RenPot"
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
year_all <- seq(2020,2050,by=5)

igdx(gdir)


# Input data --------------------------------------------------------------

df <- expand.grid(name=c('cluster','server'),size=seq(1,10)*10,ncore=c('T','F')) %>%
  mutate(name=as.character(name),ncore=as.character(ncore)) %>%
  mutate(time=pmap(list(.$name,.$size,.$ncore),~read.table(paste0(ddir,'/',..1,'/runtime_',..2,..3,'.txt'))[1,1])) %>%
  mutate(time=as.numeric(time)) %>% 
  unite(col=name,name,ncore,remove=FALSE) %>%
  mutate(name=recode(name,cluster_T='cluster60',cluster_F='cluster30',server_T='server32',server_F='server16')) %>%
  group_by(name) %>% 
  mutate(relative_time=time/min(time),relative_size=as.factor(size/min(size))) %>% 
  mutate(recode=size/100*146000) %>% 
  mutate(time2=time/60)

hoge <- lm(time2~recode, data = df %>% filter(name%in%c('cluster60')))
a <- ceiling(hoge$coefficients[2]*10)/10
b <- ceiling(hoge$coefficients[1]*10)/10

g <- df %>%
  ggplot() +
  geom_point(aes(x=recode,y=time2,color=name)) +
  geom_path(aes(x=recode,y=time2,color=name)) +
  geom_smooth(data = df %>% filter(name%in%c('cluster60','cluster30')),method = "lm", formula=y~x, aes(x=recode,y=time2),se=FALSE,size=0.5,color='grey30') +
  annotate('text',label=paste0('y=',a,'x',b),x=100000,y=40,size=4)+
  scale_x_continuous(limits=c(0,NA)) +
  labs(x='Number of record',y='run time (minutes)') +
  theme(legend.title=element_blank(),
        legend.position = 'bottom')
ggsave(paste0(odir,'/runtimeSummary.png'),g,width=5.5,height=6)
plot(g)

g <- df %>%
  ggplot(aes(x=relative_size,y=relative_time)) +
  geom_point(aes(group=name,color=name)) +
  geom_path(aes(group=name,color=name)) +
  geom_smooth(method = "lm", formula=y~x)

ggsave(paste0(odir,'/runtimeSummary2.png'),g,width=6,height=5)


g <- df %>%
  ggplot(aes(x=relative_size,y=relative_time)) +
  geom_point(aes(group=name,color=name)) +
  geom_path(aes(group=name,color=name)) +
  theme(legend.title=element_blank(),
        legend.position = 'bottom')
ggsave(paste0(odir,'/runtimeSummary2.png'),g,width=6,height=5)
plot(g)
  
