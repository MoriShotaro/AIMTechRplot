# Input data --------------------------------------------------------------

df_trade <- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_merged/trade.gdx'),'tradew_gdx') %>%
  rename(Sc=i, Sv=j, Y5=k) %>%
  mutate(Y5=as.numeric(as.character(Y5)),Sv=as.character(Sv),Sc=as.character(Sc)) %>% 
  mutate(Sc=recode(Sc,INDCi2030_500f='1.5C Conv',
                   INDCi2030_500f_Biofueloff='1.5C w/ Synfuel',
                   INDCi2030_500f_Biofueloff_Synfueloff='1.5C w/o Synfuel')) %>%
  complete(Sc,Sv,Y5=full_seq(Y5,5),fill=list(value=0)) %>%
  mutate(Sc=factor(Sc,levels=c('Baseline','1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel'))) %>% 
  filter(Sv!='NGSR2',Sv!='SYMR1') %>% 
  mutate(Sv=recode(Sv,COLR1='Coal',CRUR1='Crude oil',OILR1='Oil products',NGSR1='LNG',CRNR2='Solid biomass',
                   CRNR1='Liquid biomass',NH3R1='Ammonia',SYFR1='Synthetic fuels')) %>% 
  mutate(Sv=factor(Sv,levels=rev(c('Coal','Crude oil','Oil products','LNG','Solid biomass','Liquid biomass','Ammonia','Synthetic fuels'))))

df_stock <- rgdx.param(paste0(ddir,'/2201281605/gams_output/gdx_merged/trade.gdx'),'sto_gdx') %>%
  rename(Sc=i, Sv=j, Y5=k) %>%
  mutate(Y5=as.numeric(as.character(Y5)),Sv=as.character(Sv),Sc=as.character(Sc)) %>% 
  mutate(Sc=recode(Sc,INDCi2030_500f='1.5C Conv',
                   INDCi2030_500f_Biofueloff='1.5C w/ Synfuel',
                   INDCi2030_500f_Biofueloff_Synfueloff='1.5C w/o Synfuel')) %>%
  complete(Sc,Sv,Y5=full_seq(Y5,5),fill=list(value=0)) %>%
  mutate(Sc=factor(Sc,levels=c('Baseline','1.5C Conv','1.5C w/ Synfuel','1.5C w/o Synfuel'))) %>%
  mutate(Load_Cap=case_when(Sv=="BULK"~98.2,
                       Sv=="CRUD"~198,
                       Sv=="PROD"~70.8,
                       Sv=="LNGT"~60.5,
                       Sv=="LPGT"~31.7)) %>%  
  mutate(value=value*Load_Cap/1000) %>% # 10^3 ton -> 10^6 ton (million dwt)
  mutate(Sv=factor(Sv,levels=c('BULK','CRUD','PROD','LNGT','LPGT'))) %>% 
  mutate(Sv=recode(Sv,BULK='Bulk carrier',CRUD='Crude oil tanker',PROD='Products tanker',LNGT='LNG tanker',LPGT='Ammonia tanker')) %>% 
  select(-Load_Cap)



# Plot --------------------------------------------------------------------

df_path <- df_trade %>%
  filter(Y5%in%c(2030,2040,2050),Sc!='Baseline') %>% 
  mutate(Sv2=case_when(
    str_detect(Sv,'biomass')~'Biomass',
    str_detect(Sv,'Ammonia')~'Ammonia',
    str_detect(Sv,'Synthetic fuels')~'Synthetic fuels',
    TRUE~'Fossil'
  )) %>% 
  group_by(Sv2,Sc,Y5) %>% 
  summarise(value=sum(value)) %>% 
  mutate(Sv2=factor(Sv2,levels=c('Synthetic fuels','Ammonia','Biomass','Fossil')))

g_fbar <- df_trade %>%
  filter(Y5%in%c(2030,2040,2050),Sc!='Baseline') %>%
  group_by(Sc,Y5) %>% 
  mutate(SUM=sum(value)) %>% 
  mutate(value=value*100/SUM) %>% 
  select(-SUM) %>% 
  ggplot() +
  geom_bar(aes(x=Y5,y=value,fill=Sv),stat='identity') +
  geom_path(data=df_path,aes(x=Y5,y=value/200*100,color=Sv2),size=0.7) +
  geom_point(data=df_path,aes(x=Y5,y=value/200*100,color=Sv2,shape=Sv2),size=1.75,stroke=1,fill='white') +
  scale_y_continuous(name='Share in energy trade (%)',breaks=seq(0,100,25),
                     sec.axis = sec_axis(~.*200/100,breaks=seq(0,200,50),name=expression(paste('Energy trade by ship (EJ ',{yr^-1},')')))) +
  scale_fill_manual(values=c('mediumorchid1',
                             'thistle2',
                             'darkolivegreen2',
                             'darkolivegreen4',
                             'moccasin',
                             'sandybrown',
                             'tan3',
                             'grey50'),name='left axis') +
  scale_color_manual(values=c('indianred2','darkgoldenrod2','palegreen3','deepskyblue3'),name='right axis') +
  scale_shape_manual(values=c(21,22,23,24),name='right axis') +
  facet_wrap(vars(Sc),nrow=1) +
  guides(fill=guide_legend(order=1)) +
  MyTheme +
  theme(legend.title=element_text())
plot(g_fbar)

ggsave(paste0(odir,'/EnergyTrade.png'),width=6.5,height=6)




# Plot --------------------------------------------------------------------

g_stock <- df_stock %>%
  filter(Y5>=2030,Sc!='Baseline') %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Sc),size=0.5) +
  geom_point(aes(x=Y5,y=value,color=Sc,shape=Sc),size=0.5,stroke=0.7) +
  scale_color_manual(values=c('darkgoldenrod2','indianred2','deepskyblue3')) +
  scale_shape_manual(values=c(0,1,2)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(y='Energy trade vessel (million dwt)') +
  facet_wrap(vars(Sv),scales='free') +
  MyTheme +
  theme(
    legend.position=c(0.85,0.2)
  )
plot(g_stock)  
ggsave(paste0(odir,'/EnergyTradeVessel.png'),width=5,height=6)

g_stock2 <- df_stock %>%
  filter(Y5>=2030,Sc!='Baseline') %>%
  pivot_wider(names_from=Sv,values_from=value) %>%
  mutate(Total=`Bulk carrier`+`Crude oil tanker`+`Products tanker`+`LNG tanker`+`Ammonia tanker`) %>%
  pivot_longer(col=-c(Sc,Y5),names_to='Sv',values_to='value') %>% 
  mutate(Sv=factor(Sv,levels=c('Bulk carrier','Crude oil tanker','Products tanker','LNG tanker','Ammonia tanker','Total'))) %>% 
  ggplot() +
  geom_path(aes(x=Y5,y=value,color=Sc),size=0.5) +
  geom_point(aes(x=Y5,y=value,color=Sc,shape=Sc),size=0.5,stroke=0.7) +
  scale_color_manual(values=c('darkgoldenrod2','indianred2','deepskyblue3')) +
  scale_shape_manual(values=c(0,1,2)) +
  scale_y_continuous(limits=c(0,NA)) +
  labs(y='Energy trade vessel (million dwt)') +
  facet_wrap(vars(Sv),scales='free') +
  MyTheme +
  theme(
    legend.position='bottom'
  )
plot(g_stock2)  
ggsave(paste0(odir,'/EnergyTradeVessel.png'),width=5,height=6)


g_trade <- g_fbar + plot_spacer() + g_stock + plot_layout(width=c(4,0.25,5))
ggsave(paste0(odir,'/Trade_Vessel.png'),width=12,height=5)

g_trade <- g_fbar + plot_spacer() + g_stock2 + plot_layout(width=c(4,0.25,5))
ggsave(paste0(odir,'/Trade_Vessel2.png'),width=12,height=5)

