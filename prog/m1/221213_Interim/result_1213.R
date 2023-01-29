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
project <- "221213_Interim"
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

MyThemeLine <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=10, colour = "black", angle = 270,face="bold"),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    legend.text = element_text(size = 10),
    axis.ticks.length=unit(0.15,"cm")
  )

# Input data --------------------------------------------------------------

primdir <- paste0(ddir,'/20221213/gams_output/gdx_primary/')

df_BaU <- rgdx.param(paste0(primdir,"Baseline.gdx"),"vx_l") %>% mutate(Sc="Baseline")
df_lowOS <- rgdx.param(paste0(primdir,"SR15_lowOS.gdx"),"vx_l") %>% mutate(Sc="15C_lowOS")
df_lowOS_DAC <- rgdx.param(paste0(primdir,"SR15_lowOS_DAC.gdx"),"vx_l") %>% mutate(Sc="15C_lowOS_DAC")
df_lowOS_limBio <- rgdx.param(paste0(primdir,"SR15_lowOS_limBio.gdx"),"vx_l") %>% mutate(Sc="15C_lowOS_limBio")
df_qmax <- rgdx.param(paste0(primdir,"SR15_lowOS.gdx"),"qmax_t") %>% mutate(Sc="15C_lowOS")


df <- bind_rows(df_BaU,df_lowOS) %>% 
  filter(I=="NEN") %>%
  group_by(Sc,R,L,H) %>% 
  summarise(Value=sum(vx_l)) %>% 
  filter(!str_detect(L,"NEN")) %>% 
  filter(!str_detect(L,"STM")) %>% 
  mutate(H=as.numeric(as.character(H))) %>%
  pivot_wider(names_from=L,values_from=Value,values_fill=0) %>% 
  pivot_longer(cols=-c(Sc,R,H),names_to="L",values_to="Value") %>% 
  mutate(Y5=5*floor(H/5-404)+2020) %>%
  group_by(R,L,Y5,Sc) %>% 
  summarise(Value=mean(Value)) %>%
  select(Sc,R,L,Y5,Value) %>% 
  mutate(J=case_when(
    str_detect(L,"NH3") ~ "Ammonia",
    str_detect(L,"MOH") ~ "Methanol",
    str_detect(L,"HVC") ~ "HVC"
  )) %>% 
  mutate(K=case_when(
    str_detect(L,"BMS")  ~ "Biomass",
    str_detect(L,"NGSX") ~ "Gas w/ CCS",
    str_detect(L,"GAS")  ~ "Gas w/o CCS",
    str_detect(L,"NGS")  ~ "Gas w/o CCS",
    str_detect(L,"OIL")  ~ "Oil w/o CCS",
    str_detect(L,"COLX") ~ "Coal w/ CCS",
    str_detect(L,"COL")  ~ "Coal w/o CCS",
    str_detect(L,"ELE")  ~ "Electrolysis"
  )) %>% 
  mutate(Sc=factor(Sc,levels=c('Baseline','15C_lowOS')),
         J=factor(J,levels=c('HVC','Ammonia','Methanol')),
         K=factor(K,levels=c('Electrolysis','Biomass','Gas w/ CCS','Gas w/o CCS','Oil w/o CCS','Coal w/ CCS','Coal w/o CCS'))) %>%
  mutate(NCV=case_when(J=="HVC"~47,
                       J=="Ammonia"~18.6,
                       J=="Methanol"~20)) %>% 
  mutate(Yield=case_when(J=="HVC"&K=="Biomass"~0.96,
                         J=="HVC"&K=="Oil w/o CCS"~0.62,
                         J=="HVC"&K=="Gas w/o CCS"~0.80,
                         J=="Ammonia"&K=="Gas w/o CCS"~0.49,
                         J=="Ammonia"&K=="Gas w/ CCS"~0.49,
                         J=="Ammonia"&K=="Coal w/o CCS"~0.48,
                         J=="Ammonia"&K=="Electrolysis"~0.53,
                         J=="Methanol"&K=="Gas w/o CCS"~0.61,
                         J=="Methanol"&K=="Coal w/o CCS"~0.45,
                         J=="Methanol"&K=="Coal w/ CCS"~0.38,
                         J=="Methanol"&K=="Gas w/ CCS"~0.61)) %>%
  mutate(Value2=Value*NCV/Yield)
  

g <- df %>%
  group_by(Sc,Y5,K,J) %>% 
  summarise(Value=sum(Value)) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=Value,fill=K),stat="identity") +
  facet_grid(rows=vars(J),cols=vars(Sc),scales='free') +
  scale_fill_manual(values=c('lightsteelblue','#A9D65D','lightgoldenrod3','lightgoldenrod','sandybrown','grey30','grey50')) +
  labs(fill='',x='Year',y='Chemical Production (Mt/yr)') +
  MyThemeLine
plot(g)
ggsave(paste0(odir,"/20221213_All.png"),g,width=6,height=8)

g <- df %>%
  group_by(Sc,R,Y5,K) %>% 
  summarise(Value2=sum(Value2)/1000) %>%
  ggplot() +
  geom_bar(aes(x=Y5,y=Value2,fill=K),stat="identity") +
  facet_wrap(vars(Sc)) +
  scale_fill_manual(values=c('lightsteelblue','#A9D65D','lightgoldenrod3','lightgoldenrod','sandybrown','grey30','grey50')) +
  labs(fill='',x='Year',y='Feedstock Requirement (EJ/yr)') +
  MyThemeLine
plot(g)
ggsave(paste0(odir,"/20221213_Feedstock.png"),g,width=8,height=4)

g <- df_qmax %>% 
  filter(qmax_t!=Inf) %>%
  select(H,qmax_t) %>%
  mutate(H=as.numeric(as.character(H))) %>% 
  filter(H<=2050) %>% 
  ggplot() +
  geom_path(aes(x=H,y=qmax_t/10^6),color="red") +
  labs(x='Year',y=expression(paste(CO[2],' emissions (Gt-',CO[2],'/yr)'))) +
  scale_y_continuous(limits=c(0,NA)) +
  MyThemeLine
plot(g)
ggsave(paste0(odir,"/20221213_emissionpath.png"),g,width=3,height=4)
