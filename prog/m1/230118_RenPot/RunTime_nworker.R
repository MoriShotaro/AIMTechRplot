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

df <- expand.grid(name=c('server'),size=c(20,50),nworker=c(4,8,10,12,14,16,18,20,22)) %>%
  bind_rows(expand.grid(name=c('server'),size=c(20),nworker=c(24,26,28))) %>% 
  mutate(name=as.character(name),ncore=as.character(nworker)) %>%
  mutate(time=pmap(list(.$name,.$size,.$ncore),~read.table(paste0(ddir,'/nworker/runtime_',..2,'_',..3,'.txt'))[1,1])) %>%
  mutate(nworker=as.numeric(nworker)) %>% 
  mutate(time=as.numeric(time)) %>% 
  unite(col=name,name,size,remove=FALSE)

g <- df %>% 
  ggplot() +
  geom_path(aes(x=nworker,y=time/60,color=name)) +
  geom_point(aes(x=nworker,y=time/60,color=name)) +
  scale_x_continuous(limits=c(0,NA)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(x='Number of workers',y='run time (minutes)')
plot(g)
ggsave(paste0(odir,'/nworker.png'),width=6,height=4)
