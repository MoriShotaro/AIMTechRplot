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
library(lemon)
library(cowplot)

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

df <- rgdx.param(paste0(ddir,'/2201281605/main/merged_output.gdx'),'iamc_gdx') %>%
  filter(Sr=='World',Sc!='historical') %>%
  mutate(Y5=as.numeric(as.character(Y5)),Sv=as.character(Sv),Sc=as.character(Sc),Sr=as.character(Sr)) %>%
  complete(Sc,Sr,Sv,Y5=full_seq(Y5,5),fill=list(iamc_gdx=0)) %>%
  rename(value=iamc_gdx) %>% 
  mutate(Sc=recode(Sc,INDCi2030_500f='1.5C Conv',
                   INDCi2030_500f_Biofueloff='1.5C w/ Synfuel',
                   INDCi2030_500f_Biofueloff_Synfueloff='1.5C w/o Synfuel')) %>% 
  mutate(Sc=factor(Sc,levels=c('Baseline','1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel')))

scenario_list <- rgdx.param(paste0(ddir,'/2201281605/main/merged_output.gdx'),'iamc_gdx') %>% 
  distinct(Sc) %>% filter(Sc!='historical')

source(paste0(pdir2,'/dataframe.R'))

MyTheme <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=10, colour = "black", angle = 270,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.ticks.length=unit(0.15,"cm")
  )

MyTheme2 <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.ticks.length=unit(0.15,"cm")
  )


set_plot <- function(var){
  plt <- list()
  plt$Color <- as.character(var$Color); names(plt$Color) <- as.character(var$Sv)
  plt$Legend <- as.character(var$Legend); names(plt$Legend) <- as.character(var$Sv)
  return(plt)
}

# Primary energy ----------------------------------------------------------

df_prmene <- df %>%
  filter(Sv %in% prmene$Sv, Y5 %in% year_all) %>% 
  mutate(Sv=factor(Sv,levels=rev(prmene$Sv)))

plt <- set_plot(prmene)

g_prmene <- df_prmene %>%
  filter(Sc!='Baseline') %>% 
  ggplot() +
  geom_area(aes(x=Y5,y=value,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='Variable') +
  facet_wrap(vars(Sc),nrow=1) +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste('Primary energy (EJ ',{yr^-1},')'))) +
  MyTheme
  
plot(g_prmene)
ggsave(paste0(odir,'/Fig_prmene.png'),width=10,height=4)


# Final energy ------------------------------------------------------------

df_finene <- df %>% 
  filter(Sv %in% finene$Sv, Y5%in%year_all) %>% 
  mutate(Sb=case_when(
    str_detect(Sv,'Bio') ~ 'Biomass',
    TRUE ~ Sv
  )) %>% 
  group_by(Sc,Sb,Y5) %>% 
  summarise(value=sum(value)) %>% 
  rename(Sv=Sb) %>% 
  mutate(Sv=factor(Sv,levels=rev(c('Fin_Ene_Liq_Oil',
                               'Fin_Ene_SolidsCoa',
                               'Fin_Ene_Gas',
                               'Biomass',
                               'Fin_Ene_Solar',
                               'Fin_Ene_Ele',
                               'Fin_Ene_Heat',
                               'Fin_Ene_Hyd',
                               'Fin_Ene_Liq_Hyd_syn'))))

df_sector <- list(name1=c('Ind','Res_and_Com','Tra'),name2=list(finind,finbui,fintra))


df_finsec <- map2(df_sector$name1,df_sector$name2,~{
  df %>% filter(Sv%in%..2$Sv) %>% 
    mutate(Sb=case_when(
      str_detect(Sv,'Oil') ~ 'Fin_Ene_Liq_Oil',
      str_detect(Sv,'Coa') ~ 'Fin_Ene_SolidsCoa',
      str_detect(Sv,'Gas') ~ 'Fin_Ene_Gas',
      str_detect(Sv,'Bio') ~ 'Biomass',
      str_detect(Sv,'Solar') ~ 'Fin_Ene_Solar',
      str_detect(Sv,'Ele') ~ 'Fin_Ene_Ele',
      str_detect(Sv,'Heat') ~ 'Fin_Ene_Heat',
      str_detect(Sv,'Hyd')&!str_detect(Sv,'syn') ~ 'Fin_Ene_Hyd',
      str_detect(Sv,'syn') ~ 'Fin_Ene_Liq_Hyd_syn'
    )) %>% 
    group_by(Sc,Sb,Y5) %>% 
    summarise(value=sum(value)) %>% 
    rename(Sv=Sb) %>%
    mutate(Sv=factor(Sv,levels=rev(c('Fin_Ene_Liq_Oil',
                                     'Fin_Ene_SolidsCoa',
                                     'Fin_Ene_Gas',
                                     'Biomass',
                                     'Fin_Ene_Solar',
                                     'Fin_Ene_Ele',
                                     'Fin_Ene_Heat',
                                     'Fin_Ene_Hyd',
                                     'Fin_Ene_Liq_Hyd_syn')))) %>% 
    mutate(Se=..1)
  # pivot_wider(names_from=Sv,values_from=value) %>%
  # mutate(across(starts_with('Fin_Ene'),~.*100/eval(parse(text=paste0('Fin_Ene_',..1))))) %>%
  # pivot_longer(cols=starts_with('Fin_Ene'),names_to='Sv',values_to='value')
}) %>% bind_rows() %>% 
  mutate(Se=case_when(
    Se=='Ind'~'Industry',
    Se=='Tra'~'Transport',
    TRUE~'Buildings'
  )) %>%
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport')))


df_finene2 <- df_finsec %>%
  bind_rows(df %>% filter(Sv%in%c('Fin_Ene_Ind','Fin_Ene_Res_and_Com','Fin_Ene_Tra')) %>% select(-Sr) %>% 
              mutate(Se=case_when(
                str_detect(Sv,'Ind')~'Industry',
                str_detect(Sv,'Res_and_Com')~'Buildings',
                str_detect(Sv,'Tra')~'Transport'
              ))) %>%
  mutate(Sv=case_when(
    Sv%in%c('Fin_Ene_Ind','Fin_Ene_Res_and_Com','Fin_Ene_Tra')~'Fin_Ene',
    TRUE~Sv
  )) %>% 
  filter(Sv%in%c('Fin_Ene','Fin_Ene_Liq_Hyd_syn','Biomass','Fin_Ene_Hyd','Fin_Ene_Ele'),Y5==2050) %>%
  pivot_wider(names_from=Sv,values_from=value,values_fill=0) %>% 
  mutate(across(starts_with('Fin_Ene'),~.*100/Fin_Ene)) %>% select(-Fin_Ene) %>% 
  pivot_longer(cols=starts_with('Fin_Ene')|'Biomass',names_to='Sv',values_to='value') %>% 
  mutate(Sv=case_when(
    str_detect(Sv,'syn') ~ 'Synfuel',
    str_detect(Sv,'Bio') ~ 'Biomass',
    str_detect(Sv,'Hyd') ~ 'Hydrogen',
    str_detect(Sv,'Ele') ~ 'Electricity'
  )) %>% 
  group_by(Sc,Sv,Se,Y5) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Sv=factor(Sv,levels=rev(c('Biomass',
                               'Electricity',
                               'Hydrogen',
                               'Synfuel')))) %>% 
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport'))) %>% 
  filter(Y5==2050,Sc!='Baseline')

plt <- set_plot(finene %>% 
                  filter(!str_detect(Sv,'Bio')) %>%
                  bind_rows(data.frame(Sv='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Sv=as.character(Sv)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                         'Coal',
                                         'Gas',
                                         'Biomass',
                                         'Solar',
                                         'Electricity',
                                         'Heat',
                                         'Hydrogen',
                                         'Synfuel'))) %>% 
                  arrange(Legend))

g_finarea <- df_finene %>%
  filter(Sc!='Baseline') %>% 
  ggplot() +
  geom_area(aes(x=Y5,y=value,fill=Sv)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='left axis') +
  facet_wrap(vars(Sc),nrow=1) +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(legend.title = element_text(),
        legend.position = 'none')
plot(g_finarea)

g_finbar <- df_finsec %>%
  filter(Y5==2050,Sc!='Baseline') %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='left axis') +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  facet_wrap(vars(Se)) +
  MyTheme +
  theme(#axis.line.y = element_blank(),
        # axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        legend.title = element_text(),
        legend.position = 'none')
plot(g_finbar)

g_finbarpoint <- df_finsec %>%
  filter(Y5==2050,Sc!='Baseline') %>%
  ggplot() +
  geom_bar(aes(x=Sc,y=value,fill=Sv),stat='identity') +
  geom_point(data=df_finene2,aes(x=Sc,y=value/100*200,color=Sv,shape=Sv),size=1,stroke=1,fill='white') +
  scale_y_continuous(sec.axis = sec_axis(~./200*100,name='Share in final energy (%)',breaks=seq(0,100,20))) +
  scale_color_manual(values=c('indianred2','darkgoldenrod2','palegreen3','deepskyblue3'),name='right axis') +
  scale_shape_manual(values=c(21,22,23,24),name='right axis') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='left axis') +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  facet_wrap(vars(Se)) +
  MyTheme +
  theme(#axis.line.y = element_blank(),
    # axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    legend.title = element_text(),
    legend.position = 'none')
plot(g_finbarpoint)


# g_finbar <- df_finene %>%
#   filter(Y5==2050) %>%
#   ggplot() +
#   geom_bar(aes(x=Sc,y=value,fill=Sv),stat='identity') +
#   scale_fill_manual(values=plt$Color,labels=plt$Legend,name='Variable') +
#   guides(fill=guide_legend(reverse=TRUE)) +
#   labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
#   facet_wrap(vars(Y5)) +
#   MyTheme +
#   theme(axis.line.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# plot(g_finbar)

l_finene <- g_legend(g_finbarpoint+theme(legend.position='right'))

g_fin <- g_finarea + g_finbarpoint + plot_spacer() + l_finene + plot_layout(nrow=1, width=c(5,3,0.1,1))
plot(g_fin)
ggsave(paste0(odir,'/Fig_finene.png'),width=10,height=4)


# Secondary energy and CCUS -----------------------------------------------

# electricity
secele2 <- secele %>% 
  mutate(Legend=case_when(
    str_detect(Legend,'Oil|Gas|Coal') ~ 'Fossil',
    str_detect(Legend,'Biomass') ~ 'Biomass',
    str_detect(Legend,'Hydro|Geothermal|Hydrogen') ~ 'Other\nrenewables',
    TRUE ~ Legend
  )) %>% 
  mutate(Color=case_when(
  Legend=='Fossil' ~ 'grey50',
  Legend=='Biomass' ~ 'darkolivegreen2',
  Legend=='Nuclear ' ~'moccasin',
  Legend=='Other\nrenewables' ~ 'lightsteelblue',
  TRUE ~ Color
))

secele3 <- secele2 %>%
  distinct(Legend,.keep_all=TRUE) %>% 
  mutate(Sv=Legend)

df_ele <- df %>% 
  filter(Sv %in% secele$Sv, Y5 %in% year_all) %>% 
  left_join(secele2) %>%
  group_by(Sc,Y5,Legend) %>%
  summarise(value=sum(value)) %>% 
  mutate(Legend=factor(Legend,levels=rev(secele3$Legend))) %>%
  filter(Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
  filter(Y5%in%c(2050))

plt <- set_plot(secele3)

g_ele <- df_ele %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value,fill=Legend),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='Variable') +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste('Electricity generation (EJ ',{yr^-1},')'))) +
  MyTheme2

plot(g_ele)


# hydrogen
df_hyd <- df %>% 
  filter(Sv %in% sechyd$Sv, Y5 %in% year_all) %>% 
  mutate(Sv2=case_when(
    str_detect(Sv,'Oil|Gas|Coa') ~ 'Fossil',
    str_detect(Sv,'Bio') ~ 'Biomass',
    str_detect(Sv,'Ele') ~ 'Electricity',
  )) %>%
  mutate(Sv2=factor(Sv2,levels=c('Electricity','Biomass','Fossil'))) %>% 
  group_by(Sc,Y5,Sv2) %>% 
  summarise(value=sum(value)) %>%
  filter(Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
  filter(Y5%in%c(2050))

g_hyd <- df_hyd %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value,fill=Sv2),stat='identity') +
  scale_fill_manual(values=c('lightsteelblue','darkolivegreen2','grey50'),name='Variable') +
  labs(y=expression(paste('Hydrogen generation (EJ ',{yr^-1},')'))) +
  MyTheme2

plot(g_hyd)


# carbon capture
df_carcap <- df %>%
  filter(Sv %in% carcap$Sv, Y5 %in% year_all) %>%
  mutate(Sv2=case_when(
    str_detect(Sv,'Fos') ~ 'Fossil',
    str_detect(Sv,'Bio') ~ 'Biomass',
    str_detect(Sv,'Ind_Pro') ~ 'Industrial\nprocess',
    TRUE ~ 'DAC'
  )) %>% 
  mutate(Sv2=factor(Sv2,levels=c('DAC','Biomass','Fossil','Industrial\nprocess'))) %>% 
  group_by(Sc,Y5,Sv2) %>% 
  summarise(value=sum(value)) %>%
  filter(Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
  filter(Y5%in%c(2050)) %>% 
  mutate(Flag='Capture')

g_carcap <- df_carcap %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value,fill=Sv2),stat='identity') +
  scale_fill_manual(values=c('lightsteelblue','darkolivegreen2','grey50','sandybrown'),name='Variable') +
  MyTheme2

plot(g_carcap)


# CCUS
df_ccus <- df %>% 
  filter(Sv %in% carseq$Sv, Y5 %in% year_all) %>% 
  mutate(Sv2=case_when(
    str_detect(Sv,'Fos') ~ 'Fossil',
    str_detect(Sv,'Bio') ~ 'Biomass',
    str_detect(Sv,'Oth') ~ 'Industrial\nprocess',
    TRUE ~ 'DAC'
  )) %>% 
  mutate(Sv2=factor(Sv2,levels=c('DAC','Biomass','Fossil','Industrial\nprocess'))) %>% 
  group_by(Sc,Y5,Sv2) %>% 
  summarise(value=sum(value)) %>%
  filter(Y5%in%c(2050)) %>% 
  mutate(Flag='Sequestration') %>% 
  bind_rows(df_carcap) %>% 
  filter(Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel')) %>%
  pivot_wider(names_from=Flag,values_from=value) %>% 
  mutate(Utilization=Capture-Sequestration) %>% 
  pivot_longer(cols=c(Capture,Sequestration,Utilization),names_to='Flag',values_to='value')

g_ccus <- df_ccus %>%
  filter(Flag!='Total') %>%
  pivot_wider(names_from=Sv2,values_from=value) %>% 
  mutate(Fossil=Fossil+`Industrial\nprocess`) %>% select(-`Industrial\nprocess`) %>% 
  pivot_longer(cols=c('DAC','Biomass','Fossil'),names_to='Sv2',values_to='value') %>%
  mutate(Sv2=factor(Sv2,levels=c('DAC','Biomass','Fossil'))) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/1000,fill=Sv2),stat='identity') +
  scale_fill_manual(values=c('thistle2','darkolivegreen2','grey50'),name='Variable') +
  facet_wrap(vars(Flag)) +
  labs(y=expression(paste('CCUS (Gt',{CO[2]},' ',{yr^-1},')'))) +
  MyTheme2 +
  theme(strip.text.x = element_text(size=9, colour = "black", angle = 0,face="bold"))

plot(g_ccus)

g <- g_ele + g_hyd + g_ccus + plot_layout(nrow=1,width=c(1,1,3))
plot(g)
ggsave(paste0(odir,'/Fig_secene.png'),width=10.5,height=4)


# Emission ----------------------------------------------------------------

df_emi <- df %>% 
  filter(Sv %in% emisec$Sv)

plt <- set_plot(emisec %>% filter(Sv!='Emi_CO2_Ene_Dem_AFO'))

# g_total <- df %>%
#   filter(Sv=='Emi_CO2_Ene_and_Ind_Pro_inc_Dir_Air',Sc%in%c('Baseline','1.5C w/ Synfuel')) %>%
#   mutate(Sc=recode(Sc,`1.5C w/ Synfuel`='1.5C')) %>% 
#   group_by(Sc,Y5) %>%
#   summarise(value=sum(value)) %>% 
#   ggplot() +
#   geom_path(aes(x=Y5,y=value/1000,color=Sc)) +
#   labs(y=expression(paste({CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
#   scale_y_continuous(limits=c(-15,60)) +
#   geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.5) +
#   scale_color_manual(values=c('indianred2','deepskyblue3')) +
#   MyTheme +
#   theme(legend.position = c(0.3,0.4))
# plot(g_total)

g_total <- df %>%
  filter(Sv=='Emi_CO2_Ene_and_Ind_Pro_inc_Dir_Air',Sc%in%c('Baseline','1.5C w/ Synfuel')) %>%
  mutate(Sc=recode(Sc,`1.5C w/ Synfuel`='1.5C')) %>% 
  group_by(Sc,Y5) %>%
  summarise(value=sum(value)) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value/1000,color=Sc)) +
  labs(y=expression(paste({CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
  scale_y_continuous(limits=c(-15,60)) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.5) +
  scale_color_manual(values=c('indianred2','deepskyblue3')) +
  MyTheme +
  theme(legend.position = 'right')
plot(g_total)

# g_sec <- df_emi %>%
#     filter(Y5==2050, Sc!='Baseline') %>%
#   mutate(Sv=recode(Sv,Emi_CO2_Ene_Dem_AFO='Emi_CO2_Ene_Dem_Oth_Sec')) %>% 
#   group_by(Sc,Sv,Y5) %>% 
#   summarise(value=sum(value)) %>% 
#   mutate(Sv=factor(Sv,levels=emisec$Sv)) %>% 
#   ggplot() +
#   geom_bar(aes(x=Sc,y=value/1000,fill=Sv),stat='identity') +
#   scale_fill_manual(values=plt$Color,labels=plt$Legend,name='Variable') +
#   guides(fill=guide_legend(reverse=TRUE)) +
#   labs(y=expression(paste({CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
#   scale_y_continuous(limits=c(-15,60)) +
#   geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.5) +
#   MyTheme +
#   theme(
#     axis.line.y = element_blank(),
#     axis.text.y= element_blank(),
#     axis.title.y= element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.position = c(0.4,0.7)
#   )

g_sec <- df_emi %>%
    filter(Y5==2050, Sc!='Baseline') %>%
  mutate(Sv=recode(Sv,Emi_CO2_Ene_Dem_AFO='Emi_CO2_Ene_Dem_Oth_Sec')) %>% 
  group_by(Sc,Sv,Y5) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Sv=factor(Sv,levels=emisec$Sv)) %>% 
  ggplot() +
  geom_bar(aes(x=Sc,y=value/1000,fill=Sv),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='Variable') +
  guides(fill=guide_legend(reverse=TRUE)) +
  labs(y=expression(paste({CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
  # scale_y_continuous(limits=c(-15,60)) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.5) +
  MyTheme +
  theme(
    # axis.line.y = element_blank(),
    # axis.text.y= element_blank(),
    # axis.title.y= element_blank(),
    # axis.ticks.y = element_blank(),
    # legend.position = c(0.4,0.7)
  )

g_rsd <- df_emi %>%
  filter(Y5==2050, Sc!='Baseline',Sv%in%c('Emi_CO2_Ene_Dem_Ind','Emi_CO2_Ene_Dem_Tra','Emi_CO2_Ene_Dem_Res_and_Com')) %>%
  mutate(Sv=recode(Sv,'Emi_CO2_Ene_Dem_Ind'='Industry','Emi_CO2_Ene_Dem_Tra'='Transport','Emi_CO2_Ene_Dem_Res_and_Com'='Buildings')) %>%
  mutate(Sv=factor(Sv,levels=c('Industry','Buildings','Transport'))) %>% 
  ggplot() +
  geom_point(aes(x=Sv,y=value/1000,color=Sv,shape=Sc),size=2,stroke=1) +
  # scale_x_discrete(expand=c(10,1)) +
  scale_shape_manual(values=c(0,1,2)) +
  scale_color_manual(values=c('indianred2','deepskyblue3','mediumseagreen')) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(y=expression(paste('Residual ',{CO[2]},' emissions (Gt',{CO[2]},' ',{yr^-1},')'))) +
  MyTheme +
  theme(
    # axis.text.x= element_blank(),
    # axis.ticks.x= element_blank()
  )
plot(g_rsd)

# g_emi <- g_total + plot_spacer() + g_sec + plot_spacer() +g_rsd + plot_layout(widths=c(4.5,0.5,2.5,0.5,3))
# plot(g_emi)
# ggsave(paste0(odir,'/Fig_emission.png'),width = 9, height = 4.5)

g_emi <- plot_spacer() + {g_total+g_rsd+plot_layout(ncol=1)} + g_sec + plot_layout(width=c(0,1,1))
plot(g_emi)
ggsave(paste0(odir,'/Fig_emission.png'),width = 8, height = 6)

# Sectoral energy carrier share -------------------------------------------

df_sector <- list(name1=c('Ind','Res_and_Com','Tra'),name2=list(finind,finbui,fintra))

df_enshr <- map2(df_sector$name1,df_sector$name2,~{
  df %>% filter(Sv%in%..2$Sv|Sv==paste0('Fin_Ene_',..1)) %>%
    pivot_wider(names_from=Sv,values_from=value) %>%
    mutate(across(starts_with('Fin_Ene'),~.*100/eval(parse(text=paste0('Fin_Ene_',..1))))) %>%
    pivot_longer(cols=starts_with('Fin_Ene'),names_to='Sv',values_to='value')
  }) %>% bind_rows()

df_eleshr <- df_enshr %>% 
  filter(str_detect(Sv,'Ele'),Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel'),Y5 %in% year_all) %>%
  mutate(Sv=case_when(
    str_detect(Sv,'Ind')~'Industry',
    str_detect(Sv,'Res_and_Com')~'Buildings',
    str_detect(Sv,'Tra')~'Transport'
  )) %>%
  mutate(Sv=factor(Sv,levels=c('Industry','Buildings','Transport'))) %>%
  mutate(Sn=rep('Electricity'))
#   ggplot() +
#   geom_path(aes(x=Y5,y=value,color=Sc)) +
#   scale_color_manual(values=c('indianred2','deepskyblue3')) +
#   facet_wrap(vars(Sv)) +
#   MyTheme +
#   theme(legend.position='none')
# plot(g_eleshr)
 
df_hydshr <- df_enshr %>% 
  filter(str_detect(Sv,'Hyd'),!str_detect(Sv,'Liq'),Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel'),Y5 %in% year_all) %>%
  mutate(Sv=case_when(
    str_detect(Sv,'Ind')~'Industry',
    str_detect(Sv,'Res_and_Com')~'Buildings',
    str_detect(Sv,'Tra')~'Transport'
  )) %>%
  mutate(Sv=factor(Sv,levels=c('Industry','Buildings','Transport'))) %>%
  mutate(Sn=rep('Hydrogen'))
#   ggplot() +
#   geom_path(aes(x=Y5,y=value,color=Sc)) +
#   scale_color_manual(values=c('indianred2','deepskyblue3')) +
#   facet_wrap(vars(Sv)) +
#   MyTheme +
#   theme(legend.position='none')
# plot(g_hydshr)
 
df_hycshr <- df_enshr %>% 
  filter(str_detect(Sv,'Solid|Liq|Gas'),Sc%in%c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel'),Y5 %in% year_all) %>%
  mutate(Se=case_when(
    str_detect(Sv,'Ind')~'Industry',
    str_detect(Sv,'Res_and_Com')~'Buildings',
    str_detect(Sv,'Tra')~'Transport'
  )) %>%
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport'))) %>%
  group_by(Sc,Sr,Se,Y5) %>% 
  summarise(value=sum(value)) %>%
  rename(Sv=Se) %>% 
  mutate(Sn=rep('Hydro Carbon'))
#   ggplot() +
#   geom_path(aes(x=Y5,y=value,color=Sc)) +
#   scale_color_manual(values=c('indianred2','deepskyblue3')) +
#   facet_wrap(vars(Se)) +
#   MyTheme +
#   theme(legend.position='none')
# plot(g_hycshr)

g_enshr <- bind_rows(df_eleshr,df_hydshr,df_hycshr) %>%
  filter(Y5>=2030) %>% 
  pivot_wider(names_from=Sv,values_from=value,values_fill=0) %>%
  pivot_longer(cols=c(Industry,Buildings,Transport),names_to='Sv',values_to='value') %>% 
  mutate(Sn=factor(Sn,levels=c('Electricity','Hydrogen','Hydro Carbon'))) %>%
  mutate(Sv=factor(Sv,levels=c('Industry','Buildings','Transport'))) %>%
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Sc),size=0.4) +
  geom_point(aes(x=Y5,y=value,color=Sc,shape=Sc),stroke=0.7) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.4) +
  scale_color_manual(values=c('darkgoldenrod2','indianred2','deepskyblue3')) +
  scale_shape_manual(values=c(0,1,2)) +
  labs(y='Share of energy carriers in final energy (%)') +
  facet_grid(cols=vars(Sv),rows=vars(Sn)) +
  MyTheme +
  theme(legend.position='bottom')
plot(g_enshr)
ggsave(paste0(odir,'/Fig_ShrEneCar.png'),width=6,height=8)

# l_enshr <- g_legend(g_eleshr + theme(legend.position = 'left'))
# 
# g_enshr <- g_hycshr + g_eleshr + {g_hydshr + l_enshr + plot_layout(width=c(2,1))} + plot_layout(ncol=1)
# ggsave(paste0(odir,'/Fig_ShrEneCar.png'),width=8,height=8)
# plot(g_enshr)


# Stranded investment -----------------------------------------------------

df_str <- df %>% 
  filter(Sv %in% c('Str_Inv_Ene_Dem_Ind','Str_Inv_Ene_Dem_Bui','Str_Inv_Ene_Dem_Tra','Str_Inv_Ene_Sup')) %>%
  mutate(Sv=recode(Sv,Str_Inv_Ene_Dem_Ind='Industry',Str_Inv_Ene_Dem_Bui='Buildings',Str_Inv_Ene_Dem_Tra='Transport',Str_Inv_Ene_Sup='Energy supply')) %>%
  pivot_wider(names_from=Sc,values_from=value) %>% 
  mutate(across(starts_with('1.5C'),~.-Baseline)) %>% select(-Baseline) %>% 
  pivot_longer(cols=starts_with('1.5C'),names_to='Sc',values_to='value') %>% 
  mutate(Sv=factor(Sv,levels=c('Industry','Buildings','Transport','Energy supply')),
         Sc=factor(Sc,levels=c('1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel')))

g_str <- df_str %>%
  filter(Y5>=2020) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Sc),size=0.4) +
  geom_point(aes(x=Y5,y=value,color=Sc,shape=Sc),stroke=0.7) +
  geom_hline(yintercept=0,linetype='dashed',color='grey60',size=0.4) +
  scale_color_manual(values=c('darkgoldenrod2','indianred2','deepskyblue3')) +
  scale_shape_manual(values=c(0,1,2)) +
  labs(y=expression(paste('Stranded investment (billion US$ ',{yr^-1},')'))) +
  facet_wrap(vars(Sv),nrow=1) +
  MyTheme +
  theme(legend.position='bottom')
plot(g_str)
ggsave(paste0(odir,'/Sectoral_StrInv_path_BaselineDiff.png'),width=10,height=4)
