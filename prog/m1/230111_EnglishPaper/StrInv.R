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
project <- "230111_EnglishPaper"
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

I <- rgdx.set(paste0(cdir,'/data/m1/230111_EnglishPaper/other/M_S.gdx'),'I')
SE0 <- rgdx.set(paste0(cdir,'/data/m1/230111_EnglishPaper/other/M_S.gdx'),'SE0')
M_S <- rgdx.set(paste0(cdir,'/data/m1/230111_EnglishPaper/other/M_S.gdx'),'M_S')
check <- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_merged/merged.gdx'),'iamcrgdx')
df_sector <- read_csv(paste0(cdir,'/data/m1/230111_EnglishPaper/other/sector_class.csv'))

for (i in scenario_list$Sc){
  bn_t1 <- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_primary/',i,'.gdx'),'bn_t1') 
  tn <- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_primary/',i,'.gdx'),'tn')
  res_occ<- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_primary/',i,'.gdx'),'res_occ_l')
  FL_IL<- rgdx.set(paste0(ddir,'/2201281605/gams_output/gdx_primary/',i,'.gdx'),'FL_IL')
  df <- bn_t1 %>%
    left_join(tn) %>% 
    left_join(res_occ) %>%
    full_join(FL_IL) %>%
    full_join(M_S) %>% 
    drop_na() %>% 
    mutate(cn2=bn_t1*0.05*exp(tn*log(1+0.05))/(exp(tn*log(1+0.05))-1)) %>%
    mutate(str_inv=res_occ_l*cn2/1000) %>% 
    mutate(Sc=rep(i))
  assign(paste0('df_',i),df)
}

# df_all <- bind_rows(df_Baseline,
#                 df_INDCi2030_500f,
#                 df_INDCi2030_500f_Biofueloff,
#                 df_INDCi2030_500f_Biofueloff_Synfueloff) %>%
#   select(-bn_t1,-tn,-res_occ_l,-cn2) %>%
#   group_by(H,I,Sc) %>% 
#   summarise(Value=sum(str_inv)) %>%
#   pivot_wider(names_from=Sc,values_from=Value,values_fill=0) %>% 
#   # mutate(across(starts_with('INDC'),~.-Baseline)) %>% 
#   # select(-Baseline) %>% 
#   pivot_longer(cols=starts_with(c('INDC','Baseline')),names_to='Sc',values_to='Value') %>%
#   filter(Value!=0)
# 
# df <- df_all %>%
#   mutate(H=as.numeric(as.character(H))) %>%
#   mutate(flag=ceiling(H/5)*5) %>%
#   # mutate(flag=5*floor(H/5-404)+2020) %>%
#   group_by(I,flag,Sc) %>%
#   summarise(Value=mean(Value)) %>%
#   rename('Y5'=flag) %>% 
#   mutate(Sc=recode(Sc,INDCi2030_500f='1.5C Conv',
#                    INDCi2030_500f_Biofueloff='1.5C w/ Synfuel',
#                    INDCi2030_500f_Biofueloff_Synfueloff='1.5C w/o Synfuel')) %>% 
#   full_join(df_sector) %>%
#   mutate(I=factor(I,levels=df_sector$I)) %>% 
#   drop_na()
  
df_all2 <- bind_rows(df_Baseline,
                 df_INDCi2030_500f,
                 df_INDCi2030_500f_Biofueloff,
                 df_INDCi2030_500f_Biofueloff_Synfueloff) %>%
  select(-bn_t1,-tn,-res_occ_l,-cn2) %>%
  group_by(H,I,Sc) %>% 
  summarise(Value=sum(str_inv)) %>%
  pivot_wider(names_from=Sc,values_from=Value,values_fill=0) %>% 
  mutate(across(starts_with('INDC'),~.-Baseline)) %>%
  select(-Baseline) %>%
  pivot_longer(cols=starts_with(c('INDC','Baseline')),names_to='Sc',values_to='Value') %>%
  filter(Value!=0)

df2 <- df_all2 %>%
  mutate(H=as.numeric(as.character(H))) %>%
  mutate(flag=ceiling(H/5)*5) %>%
  # mutate(flag=5*floor(H/5-404)+2020) %>%
  group_by(I,flag,Sc) %>%
  summarise(Value=mean(Value)) %>%
  rename('Y5'=flag) %>% 
  mutate(Sc=recode(Sc,INDCi2030_500f='1.5C Conv',
                   INDCi2030_500f_Biofueloff='1.5C w/ Synfuel',
                   INDCi2030_500f_Biofueloff_Synfueloff='1.5C w/o Synfuel')) %>% 
  full_join(df_sector) %>%
  mutate(I=factor(I,levels=df_sector$I)) %>% 
  drop_na()

# Analysis ----------------------------------------------------------------

# g <- df %>%
#   group_by(Y5,Sc,I2) %>% 
#   summarise(Value=sum(Value)) %>%
#   filter(Y5>=2020,Sc%in%c('1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
#   ggplot() +
#   geom_path(aes(x=Y5,y=Value,color=Sc)) +
#   facet_wrap(vars(I2),ncol=4)
# plot(g)
# ggsave(paste0(odir,'/Sectoral_StrInv_path.png'),g,width=10,height=5)
# 
# g <- df %>%
#   filter(Y5>=2020,Sc%in%c('1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
#   ggplot() +
#   geom_bar(aes(x=Y5,y=Value,fill=I),stat='Identity') +
#   facet_grid(rows=vars(Sc),cols=vars(I2))
# plot(g)
# ggsave(paste0(odir,'/SubSectoral_StrInv_bar.png'),g,width=10,height=10)
# 
# g <- df %>%
#   filter(Y5>=2020,Sc%in%c('1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
#   pivot_wider(names_from=Sc,values_from=Value,values_fill=0) %>%
#   mutate(diff=`1.5C w/o Synfuel`-`1.5C w/ Synfuel`) %>%
#   pivot_longer(cols=starts_with(c('1.5C','diff')),names_to='Sc',values_to='Value') %>%
#   filter(Sc=='diff') %>% 
#   ggplot() +
#   geom_bar(aes(x=Y5,y=Value,fill=I),stat='Identity') +
#   facet_wrap(vars(I2),ncol=4)
# plot(g)
# ggsave(paste0(odir,'/Sectoral_StrInv_Synfuel.png'),g,width=10,height=5)
# 
# g <- df %>%
#   filter(Y5>=2020,Sc%in%c('1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
#   pivot_wider(names_from=Sc,values_from=Value,values_fill=0) %>%
#   mutate(diff=`1.5C w/o Synfuel`-`1.5C w/ Synfuel`) %>%
#   pivot_longer(cols=starts_with(c('1.5C','diff')),names_to='Sc',values_to='Value') %>%
#   filter(Sc=='diff') %>% 
#   ggplot() +
#   geom_bar(aes(x=Y5,y=Value,fill=I),stat='Identity')
# plot(g)
# ggsave(paste0(odir,'/SubSectoral_StrInv_Synfuel.png'),g,width=5,height=5)


# Analysis2 ---------------------------------------------------------------

g <- df2 %>%
  group_by(Y5,Sc,I2) %>% 
  summarise(Value=sum(Value)) %>%
  filter(Y5>=2020,Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
  mutate(I2=factor(I2,levels=c('IND','BLD','TRP','SUP'))) %>% 
  mutate(I2=recode(I2,IND='Industry',BLD='Buildings',TRP='Transport',SUP='Energy supply')) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=Value,color=Sc),size=0.4) +
  geom_point(aes(x=Y5,y=Value,color=Sc,shape=Sc),stroke=0.7) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.4) +
  scale_color_manual(values=c('darkgoldenrod2','indianred2','deepskyblue3')) +
  scale_shape_manual(values=c(0,1,2)) +
  labs(y='Share of energy carriers in final energy (%)') +
  facet_wrap(vars(I2),nrow=1) +
  MyTheme +
  theme(legend.position='bottom')
plot(g)
ggsave(paste0(odir,'/Sectoral_StrInv_path_BaselineDiff.png'),g,width=10,height=4)

# g <- df2 %>%
#   filter(Y5>=2020,Sc%in%c('1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
#   ggplot() +
#   geom_bar(aes(x=Y5,y=Value,fill=I),stat='Identity') +
#   facet_grid(rows=vars(Sc),cols=vars(I2))
# plot(g)
# ggsave(paste0(odir,'/SubSectoral_StrInv_bar_BaselineDiff.png'),g,width=10,height=10)


# Analysis3 ---------------------------------------------------------------

# for (i in scenario_list$Sc){
#   vs_l <- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_primary/',i,'.gdx'),'vs_l')
#   res_occ<- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_primary/',i,'.gdx'),'res_occ_l')
#   df <- res_occ %>%
#     left_join(vs_l) %>% 
#     mutate(Sc=rep(i))
#   assign(paste0('vs_',i),df)
# }
# 
# vs_all <- bind_rows(vs_Baseline,
#                     vs_INDCi2030_500f,
#                     vs_INDCi2030_500f_Biofueloff,
#                     vs_INDCi2030_500f_Biofueloff_Synfueloff) %>%
#   mutate(stock=bn_t1*vs_l) %>% 
#   select(-bn_t1,-vs_l) %>%
#   group_by(H,I,Sc) %>% 
#   summarise(Value=sum(stock)) %>% 
#   mutate(H=as.numeric(as.character(H))) %>%
#   mutate(flag=ceiling(H/5)*5) %>%
#   # mutate(flag=5*floor(H/5-404)+2020) %>%
#   group_by(I,flag,Sc) %>%
#   summarise(Value=mean(Value)) %>%
#   rename('Y5'=flag) %>% 
#   mutate(Sc=recode(Sc,INDCi2030_500f='1.5C Conv',
#                    INDCi2030_500f_Biofueloff='1.5C w/ Synfuel',
#                    INDCi2030_500f_Biofueloff_Synfueloff='1.5C w/o Synfuel')) %>% 
#   full_join(df_sector) %>%
#   mutate(I=factor(I,levels=df_sector$I)) %>% 
#   drop_na()
# 
# df3 <- df %>%
#   left_join()
  
