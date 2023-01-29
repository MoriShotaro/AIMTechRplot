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
library(gdxdt)

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



# Non energy demand -------------------------------------------------------

ebal_33 <- rgdx.param(paste0(ddir,"/ebal.gdx"),"ebal_33")
PR_NE <- rgdx.set(paste0(ddir,"/ebal.gdx"),"PR_NE") %>%  unlist()
serv_t <- rgdx.param(paste0(ddir,"/serv_global_SSP2.gdx"),"serv_t")


# regional non-energy use in PJ
df_shrALL <- ebal_33 %>% mutate(Y=as.numeric(as.character(Y))) %>% 
  filter(FL=="CHM",PR%in%PR_NE,Y>=2005) %>%
  group_by(R33,Y) %>% 
  mutate(ebal_33=ebal_33/sum(ebal_33)) %>%
  ungroup() %>% select(-FL) %>% 
  mutate(PR=as.character(PR)) %>%
  pivot_wider(names_from=PR,values_from=ebal_33,values_fill=0) %>% 
  pivot_longer(cols=-c(R33,Y),names_to="PR",values_to="ebal_33") %>% 
  group_by(R33) %>% nest() %>% 
  mutate(data=map(data,~{
    pivot_wider(.,names_from=PR,values_from=ebal_33) %>%
      bind_rows(data.frame(Y=seq(2019,2100,1),
                           N_COL=rep(.$N_COL[9]),
                           N_CLP=rep(.$N_CLP[9]),
                           N_NGS=rep(.$N_NGS[9]),
                           N_CRU=rep(.$N_CRU[9]),
                           N_OIL=rep(.$N_OIL[9]),
                           N_LPG=rep(.$N_LPG[9])))
  })) %>% unnest(data)

df_srvALL <- serv_t %>% mutate(Y=as.numeric(as.character(t))) %>%
  filter(J=="NEN") %>%
  select(-t,-I,-J) %>% 
  rename("R33"=R)

df_demALL <- df_shrALL %>%
  left_join(df_srvALL) %>%
  pivot_longer(cols=-c(R33,Y,serv_t),names_to="PR_NE",values_to="Value") %>% 
  mutate(Value=Value*serv_t) %>% 
  select(R33,Y,PR_NE,Value) %>% 
  rename("Variable"=PR_NE)


# global non energy use in PJ
df_srvGlo <- df_srvALL %>% 
  group_by(Y) %>% 
  summarize(serv_t=sum(serv_t))

df_demGlo <- df_demALL %>% 
  group_by(Y,Variable) %>% 
  summarize(Value=sum(Value))


# Non energy demand for NH3 and methanol ----------------------------------

# input data
levicullen1 <- read_csv(paste0(ddir,"/Levi_Cullen1.csv"))

# global non-energy use for NH3 production in Mt/yr
Feedstock_NH3 <- levicullen1 %>% 
  select(Feedstock,Ammonia) %>% 
  rename("Dem_NH3"=Ammonia)

# global non-energy use for Methanol production in Mt/yr
Feedstock_Methanol <- levicullen1 %>% 
  select(Feedstock,Methanol) %>% 
  rename("Dem_Methanol"=Methanol)

# NCV in MJ/kg -> GJ/t -> PJ/Mt
df_ncvAmMe <- read_csv(paste0(ddir,"/NCV_AmMe.csv")) %>% 
  rename("NCV"=Value)

# non C/H content of each feed stock
df_nonCH <- levicullen1 %>% 
  select(Feedstock,nonCH)

# feed stock mapping
map_feedstock <- data.frame(Variable=c("N_COL","N_CLP","N_NGS","N_CRU","N_OIL","N_LPG"),
                            Feedstock=c("COL","COL","NGS","OIL","OIL","LPG"))

# total global non-energy use in chemical sector in 2013 in PJ
df_dem2013 <- df_demGlo %>%
  ungroup() %>% 
  filter(Y==2013) %>% select(-Y)

# share of ammonia and methanol production in non-energy use in chemical sector
df_shrAmMe <- df_dem2013 %>%   
  left_join(df_ncvAmMe) %>% 
  mutate(Value=Value/NCV) %>% # kt -> t -> Mt
  select(-NCV) %>%
  left_join(map_feedstock) %>%
  group_by(Feedstock) %>% summarise(Value=sum(Value)) %>% 
  left_join(Feedstock_NH3) %>% left_join(Feedstock_Methanol) %>% 
  transmute(Feedstock,NH3=Dem_NH3/Value,Methanol=Dem_Methanol/Value) %>% 
  drop_na()

# regional non energy use for NH3 and Methanol production in PJ
df_demAmMe <- df_demALL %>% 
  left_join(map_feedstock) %>% left_join(df_shrAmMe) %>% 
  transmute(R33,Y,Variable,NH3=NH3*Value,Methanol=Methanol*Value) %>% 
  replace_na(list(NH3=0,Methanol=0))

# global non energy use for NH3 and Methanol production in PJ
df_demAmMeGlo <- df_demAmMe %>% 
  group_by(Y,Variable) %>% 
  summarise(NH3=sum(NH3),Methanol=sum(Methanol))


# Non energy use for HVC production via steam cracking --------------------

# input data
levicullen2 <- read_csv(paste0(ddir,"/Levi_Cullen2.csv"))

# NCV in MJ/kg -> GJ/t -> PJ/Mt
df_ncvHVC <- read_csv(paste0(ddir,"/NCV_sc.csv")) %>% 
  rename("NCV"=Value,"Feedstock"=Variable)

# global non-energy use for HVC production in PJ
Feedstock_HVC <- levicullen2 %>%
  left_join(df_ncvHVC) %>%
  mutate(Value=Value*NCV) %>%
  select(-NCV) %>%
  group_by(Flag) %>% 
  summarize(Dem_HVC=sum(Value)) %>%
  filter(Flag!="Output") %>% 
  rename("Feedstock"=Flag)
  
# share of HVC production in non-energy use in chemical sector
df_shrHVC <- df_dem2013 %>%   
  left_join(map_feedstock) %>%
  group_by(Feedstock) %>% summarise(Value=sum(Value)) %>% 
  left_join(Feedstock_HVC) %>% 
  transmute(Feedstock,HVC=Dem_HVC/Value) %>% 
  drop_na()

# regional non energy use for HVC production in PJ
df_demHVC <- df_demALL %>% 
  left_join(map_feedstock) %>% left_join(df_shrHVC) %>% 
  transmute(R33,Y,Variable,HVC=HVC*Value) %>% 
  replace_na(list(HVC=0))

# global non energy use for NH3 and Methanol production in PJ
df_demHVCGlo <- df_demHVC%>% 
  group_by(Y,Variable) %>% 
  summarise(HVC=sum(HVC))


# Join ammonia, methanol and HVC data -------------------------------------

df_shrChem <- full_join(df_shrAmMe,df_shrHVC) %>% 
  replace_na(list(NH3=0,Methanol=0,HVC=0)) %>% 
  mutate(Other=1-(NH3+Methanol+HVC))

df_demChem <- df_demALL %>% 
  left_join(map_feedstock) %>% left_join(df_shrChem) %>% 
  transmute(R33,Y,Variable,Ammonia=NH3*Value,Methanol=Methanol*Value,HVC=HVC*Value,Other=Other*Value) %>%
  mutate(Total=Ammonia+Methanol+HVC+Other) %>%  # Ammonia or NH3 ?
  rename("Feedstock"=Variable) %>% 
  pivot_longer(cols=-c(R33,Y,Feedstock),names_to="Product",values_to="Value")


# Aggregate feed stock to product ------------------------------------------

# Feed stock(Mt) -> Product(Mt) efficiency of Ammonia and Methanol production
mass_AmMe <- levicullen1 %>%
  select(Feedstock,Ammonia,Methanol,O_Ammonia,O_Methanol) %>% 
  transmute(Feedstock,Ammonia=O_Ammonia/Ammonia,Methanol=O_Methanol/Methanol)

# Feed stock(Mt) -> Product(Mt) efficiency of HVC production
mass_HVC <- data.frame(Feedstock=c("OIL","LPG"),HVC=c(224.5,91.8),O_HVC=c(71.1+37.2+23.6,60.9+5.9)) %>% 
  transmute(Feedstock,HVC=O_HVC/HVC)

# Join efficiency data
mass_Chem <- full_join(mass_AmMe,mass_HVC) %>% 
  pivot_longer(cols=-c(Feedstock),names_to="Product",values_to="Eff")

df_ncv <- read_csv(paste0(ddir,"/NCV_FP.csv")) %>% 
  rename("NCV"=Value)

# PJ/(PJ/Mt) -> Mt and aggregate by products
df_demChem_Mt <- df_demChem %>%
  filter(Product!="Other",Product!="Total") %>% 
  left_join(df_ncv) %>% 
  mutate(Value=Value/NCV) %>% 
  select(-NCV) %>% # PJ -> Mt
  group_by(Y,Feedstock,Product) %>% 
  summarise(Value=sum(Value)) %>%
  rename("Variable"=Feedstock) %>% 
  left_join(map_feedstock) %>% left_join(mass_Chem) %>% 
  replace_na(list(Ammonia=0,Methanol=0,HVC=0,Eff=0)) %>%
  mutate(Value=Value*Eff) %>% select(-Feedstock,-Eff) %>% 
  rename("Feedstock"=Variable)
  

# Input data to AIM/Technology --------------------------------------------

# Chemical products demand in Mt_product
df_ToAIMTech_Chem <- df_demChem %>%
  filter(Product!="Other",Product!="Total") %>% 
  left_join(df_ncv) %>% 
  mutate(Value=Value/NCV) %>% 
  select(-NCV) %>% # PJ -> Mt
  group_by(R33,Y,Feedstock,Product) %>% 
  summarise(Value=sum(Value)) %>%
  rename("Variable"=Feedstock) %>% 
  left_join(map_feedstock) %>% left_join(mass_Chem) %>% 
  replace_na(list(Ammonia=0,Methanol=0,HVC=0,Eff=0)) %>%
  mutate(Value=Value*Eff) %>% select(-Feedstock,-Eff) %>% 
  rename("Feedstock"=Variable) %>% 
  group_by(R33,Y,Product) %>% 
  summarise(Value=sum(Value))

# Other chemical products demand in PJ_feed-stock
df_ToAIMTech_Oth <- df_demChem %>% 
  filter(Product=="Other") %>%
  group_by(R33,Y,Product) %>% 
  summarise(Value=sum(Value))

# Join data
df_ToAIMTech <- bind_rows(df_ToAIMTech_Chem,df_ToAIMTech_Oth) %>%
  pivot_wider(names_from=Product,values_from=Value) %>% 
  rename("R"=R33,"H"=Y,"NEN_NH3"=Ammonia,"NEN_MOH"=Methanol,"NEN_HVC"=HVC,"NEN_OTH"=Other) %>% 
  mutate(I=rep("NEN")) %>%
  pivot_longer(cols=-c(R,I,H),names_to="J",values_to="Value") %>%
  pivot_wider(names_from=H,values_from=Value) %>% 
  select(R,I,J,everything())

# df_ToAIMTech_Unit <- data.frame(Product=c("Ammonia","Methanol","HVC","Other"),Unit=c("Mt-product","Mt-product","Mt-product","PJ-feedstock"))

# Write GDX file
lst1 <- wgdx.reshape(df_ToAIMTech,symDim=4,symName="serv_t",tName="H")
# lst2 <- wgdx.reshape(df_ToAIMTech_Unit,symDim=2,symName="Unit",tName="fuga")
# lst <- union(lst1,lst2)
wgdx.lst(paste0(odir,"/dem_Chem.gdx"),lst1)


# HVC feed stock share -----------------------------------------------------

# Read original data without column names
df_IEAebal <- readLines(paste0(ddir,"/IEA_Ebal_2013.csv")) %>% 
  str_replace_all('",',"',") %>% 
  str_replace_all('^"|"$', "'") %>%
  str_replace_all(',"', ",'") %>% 
  paste(collapse='\n') %>% 
  fread(quote="'",skip=2) %>% 
  filter(V1>=2000)

# Read and join column names
cols <- readLines(paste0(ddir,"/IEA_Ebal_2013.csv")) %>% 
  str_replace_all('",',"',") %>% 
  str_replace_all('^"|"$', "'") %>%
  str_replace_all(',"', ",'") %>%
  str_replace_all(' ', "") %>%
  paste(collapse='\n') %>%
  fread(quote="'",nrow=1,fill=TRUE) %>% unlist()

names(cols)[[1]] <- "FLOW"; names(cols)[[2]] <- "COUNTRY"
colnames(df_IEAebal) <- names(cols)

# Filter non-energy use
df_NENebal <- df_IEAebal %>% 
  filter(str_detect(FLOW,"Non-energy")) %>%
  pivot_longer(cols=-c(FLOW,COUNTRY),names_to="PRODUCT",values_to="VALUE") %>% 
  mutate(VALUE=str_replace_all(VALUE,"\\.\\.","0")) %>% 
  mutate(VALUE=str_replace_all(VALUE,"c","0")) %>%
  mutate(VALUE=str_replace_all(VALUE,"x","0")) %>%
  mutate(VALUE=as.numeric(VALUE))

df_Chemebal <- df_NENebal %>% 
  filter(FLOW=='Memo: Non-energy use in chemical/petrochemical')

# plot --------------------------------------------------------------------

g <- df_demChem %>% 
  filter(Product!="Total") %>% 
  group_by(Y,Product) %>%
  summarise(Value=sum(Value)) %>%
  ggplot() +
  geom_area(aes(x=Y,y=Value/1000,fill=Product)) +
  scale_fill_brewer(palette="YlGnBu") +
  labs(x="Year",y="Non-energy use (EJ)")
plot(g)
ggsave(paste0(odir,"/demChem_prod.png"),g,width=4,height=4)

g <- df_demChem %>%
  filter(Product!="Total") %>% 
  group_by(Y,Feedstock) %>%
  summarise(Value=sum(Value)) %>% 
  ggplot() +
  geom_area(aes(x=Y,y=Value/1000,fill=Feedstock)) +
  scale_fill_brewer(palette="YlGnBu") +
  labs(x="Year",y="Non-energy use (EJ)")
plot(g)
ggsave(paste0(odir,"/demChem_feed.png"),g,width=5,height=4)

g <- df_demChem %>%
  filter(Product!="Total") %>% 
  group_by(Y,Feedstock,Product) %>%
  summarise(Value=sum(Value)) %>% 
  ggplot() +
  geom_area(aes(x=Y,y=Value/1000,fill=Feedstock)) +
  scale_fill_brewer(palette="YlGnBu") +
  facet_wrap(vars(Product),nrow=1) +
  labs(x="Year",y="Non-energy use (EJ)")
plot(g)
ggsave(paste0(odir,"/demChem_feed_2.png"),g,width=10,height=6)


g <- df_demChem_Mt %>%
  group_by(Y,Product) %>%
  summarise(Value=sum(Value)) %>% 
  ggplot() +
  geom_area(aes(x=Y,y=Value,fill=Product)) +
  scale_fill_brewer(palette="YlGnBu") +
  labs(x="Year",y="Production (Mt)")
plot(g)
ggsave(paste0(odir,"/demChem_feed_Mt.png"),g,width=4,height=4)

g <- df_demChem %>% 
  group_by(Y,Feedstock,Product) %>% 
  summarize(Value=sum(Value)) %>%
  filter(Product!="Total") %>% 
  ggplot() +
  geom_area(aes(x=Y,y=Value,fill=Product)) +
  scale_fill_brewer(palette="YlGnBu") +
  facet_wrap(vars(Feedstock),nrow=1)
plot(g)

# g <- df_nen %>%
#   filter(PR_NE!="Total") %>% 
#   ggplot() +
#   geom_line(aes(x=Y,y=Value,color=PR_NE)) +
#   scale_color_brewer(palette="YlGnBu")
# plot(g)
# 
# g <- ggplot() +
#   geom_line(data=df_nen%>%filter(PR_NE=="Total"),aes(x=Y,y=Value,color=PR_NE)) +
#   geom_area(data=df_nen%>%filter(PR_NE!="Total"),aes(x=Y,y=Value,fill=PR_NE)) +
#   scale_fill_brewer(palette="YlGnBu")
# plot(g)


# Others ------------------------------------------------------------------
# 
# test <- df_demAmMeGlo %>% 
#   left_join(df_ncv) %>%
#   mutate(NH3=NH3/NCV,Methanol=Methanol/NCV)
# test2 <- df_demHVCGlo %>% 
#   left_join(df_ncv) %>%
#   mutate(HVC=HVC/NCV)
# test3 <- df_demChem %>% 
#   group_by(Y,Feedstock) %>%
#   summarise(Value=sum(Value)) %>% 
#   filter(Y==2013) %>%
#   rename("Variable"=Feedstock) %>% 
#   left_join(df_ncvHVC) %>% 
#   transmute(Variable,Value=Value/NCV)
# test4 <- df_demChem_Mt %>% 
#   group_by(Y,Product) %>% 
#   summarise(Value=sum(Value))
