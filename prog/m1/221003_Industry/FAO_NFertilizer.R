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

# Region mapping ----------------------------------------------------------

# Japan(COUNTRY) to JAPAN(IEA) mapping
iea_country <- read.delim(paste0(ddir,"/ieacountry.set"),header=FALSE,sep="\t") %>%
  rename(IEA=1,COUNTRY=2)

# JAPAN(IEA) to JPN(R32) mapping
iea_R32 <- read.delim(paste0(ddir,"/iea32.map"),header=FALSE,sep=".") %>% 
  rename(IEA=1,R32=2) %>% 
  mutate(IEA=str_replace_all(IEA," ","")) %>%
  mutate(IEA=str_replace_all(IEA,"\t",""))

# Japan(COUNTRY) to JPN(R32) mapping
country_R32 <- full_join(iea_country,iea_R32) %>%
  select(COUNTRY,R32) %>% 
  mutate(COUNTRY=recode(COUNTRY,"China (P.R. of China and Hong Kong  China)"="China (P.R. of China and Hong Kong, China)"))

# Read 32 region and 23 region
R32 <- iea_R32 %>% distinct(R32) %>% filter(R32!="INTL")
R23 <- readxl::read_xls(paste0(ddir,"/IEA_EneBal_2005_1012.xls"),sheet="地域コード",skip=4) %>%
  distinct(AIM_GLB) %>% rename(R23=AIM_GLB)

# R23 to R15 mapping
R23_R15 <- read.delim(paste0(ddir,"/R23_15.set"),header=FALSE,sep=" ") %>% 
  rename(R23=1,R15=2)

# R32 to R23 mapping
R32_R23 <- read.delim(paste0(ddir,"/R32_23.set"),header=FALSE,sep=" ") %>% 
  rename(R32=1,R23=2)

# R32 to R15 mapping
R32_R15 <- R32_R23 %>% full_join(R23_R15) %>% select(-R23)

# Input data --------------------------------------------------------------

FAO_FN <- read_csv(paste0(ddir,"/FAO_FN.csv"))
FAO_FP <- read_csv(paste0(ddir,"/FAO_FP.csv"))
FAO_Macro <- read_csv(paste0(ddir,"/FAO_Macro.csv"))
FAO_POP <- read_csv(paste0(ddir,"/FAO_POP.csv"))
FAO_Trade <- read_csv(paste0(ddir,"/FAO_Trade.csv"))
FAO_Trade2 <- read_csv(paste0(ddir,"/FAO_Trade2.csv"))
M49_ISO3 <- read_csv(paste0(ddir,"/M49_ISO3.csv"))

ls_FAO <- list(FAO_FN,FAO_FP,FAO_Macro,FAO_POP)
FAO_all <- map_dfr(ls_FAO,~pivot_longer(.,cols=starts_with("Y"),names_to="Year",values_to="Value",names_prefix="Y") %>% 
                    mutate(Year=as.numeric(Year)) %>% filter(Year>=2000) %>% select(Area,Item,Element,Unit,Year,Value))
ls_Item <- FAO_all %>% distinct(Item) %>%
  filter(Item%in%c('Nutrient nitrogen N (total)','Gross Domestic Product','Population - Est. & Proj.')) %>% unlist()

df_FAO <- FAO_all %>% 
  filter(Item%in%ls_Item)

df_FAO_ind <- df_FAO %>% 
  filter(Item!='Nutrient nitrogen N (total)',Element%in%c('Value US$','Total Population - Both sexes')) %>% 
  select(-Element,-Unit) %>% pivot_wider(names_from='Item',values_from='Value') %>% 
  rename(GDP=3,Population=4)

df_FAO_Trade <- FAO_Trade %>% 
  select(Area,Element,Year,Value) %>% 
  pivot_wider(names_from=Element,values_from=Value) %>% 
  mutate(NetExport=`Export Value`-`Import Value`) %>%
  select(Area,Year,NetExport)

df_FAO_Trade2 <- FAO_Trade2 %>% 
  select(Area,Element,Year,Value) %>% 
  pivot_wider(names_from=Element,values_from=Value) %>% 
  mutate(NetExport2=`Export Value`-`Import Value`)%>%
  select(Area,Year,NetExport2)

CountryCodes <- M49_ISO3 %>%
  rename(iso3c=1) %>% 
  left_join(codelist %>% select(country.name.en,iso3c)) %>% 
  drop_na(country.name.en) %>% rename(COUNTRY=3) %>% 
  left_join(country_R32) %>% 
  rename(R33=4)

df_FAO_FN <- df_FAO %>% 
  filter(Item=='Nutrient nitrogen N (total)') %>% 
  pivot_wider(names_from=Element,values_from=Value) %>%
  drop_na()

df <- df_FAO_FN %>% 
  left_join(df_FAO_ind) %>%
  mutate(ApDem=Production+`Import Quantity`-`Export Quantity`) %>% 
  select(-5,-6,-7) %>% 
  mutate(AgrUse_CAP=`Agricultural Use`/Population,GDP_CAP=GDP/Population) %>% 
  filter(Year>=2005,Year<=2015) %>% 
  left_join(df_FAO_Trade) %>% 
  left_join(df_FAO_Trade2) %>% 
  left_join(CountryCodes) %>% 
  drop_na(R33) %>%
  select(1,4:12,15) %>% select(-7,-8) %>% 
  pivot_longer(cols=-c(Area,Year,R33),names_to="Variable",values_to="Value") %>% 
  group_by(R33,Year,Variable) %>%
  summarise(Value=sum(Value)) %>% 
  pivot_wider(names_from=Variable,values_from=Value) %>%
  mutate(NetExport3=NetExport/GDP) %>% 
  mutate(NetExport=ifelse(NetExport>0,T,F),NetExport2=ifelse(NetExport2>0,T,F)) %>%
  mutate(AgrUse_CAP=`Agricultural Use`/Population,GDP_CAP=GDP/Population)

df_country <- df %>% 
  group_by(Year) %>% 
  mutate(Flag1=ifelse(GDP_CAP>median(GDP_CAP),T,F)) %>%
  group_by(R33) %>% 
  mutate(Flag_Export=ifelse(sum(NetExport)/11>0.5,T,F)) %>% 
  mutate(Flag_GDP=ifelse(sum(Flag1)/11>0.5,T,F)) %>%
  mutate(NetExport3=mean(NetExport3)) %>%
  select(R33,Flag_Export,Flag_GDP,NetExport3) %>% 
  distinct(R33,.keep_all=TRUE) %>% 
  mutate(Flag_Export2=case_when(
    NetExport3<=-16 ~ "a",
    NetExport3<=0 ~ "b",
    NetExport3<=22 ~ "c",
    TRUE ~ "d"
  )) %>% select(-NetExport3)


df2 <- df %>% 
  left_join(df_country) %>%
  mutate(Flag_Export=ifelse(Flag_Export,"Net Export","Net Import")) %>% 
  rename(Flag=Flag_Export) %>%
  rename(AgrUse=`Agricultural Use`)
  
hoge <- summary(lm(AgrUse~Population,data=df2))
alpha <- hoge$coefficients[2]
beta <- hoge$coefficients[1]
R.square <- hoge$r.squared

g1 <- df2 %>% 
  ggplot() +
  geom_point(aes(x=GDP_CAP,y=AgrUse_CAP,color=Flag_GDP))
plot(g1)

g2 <- df2 %>% 
  ggplot() +
  geom_point(aes(x=GDP_CAP,y=AgrUse_CAP,color=Flag)) +
  theme(legend.position="bottom")
plot(g2)

g3 <- df2 %>% 
  ggplot() +
  geom_point(aes(x=Population,y=AgrUse,color=Flag_GDP)) +
  annotate("text",x=8e+05,y=2.0e+06,hjust=-.2,vjust=2,
           label=bquote(paste(R^2,"= ",.(R.square)))) +
  stat_function(fun=function(x) alpha*x+beta) +
  theme(legend.position="bottom")
plot(g3)

g4 <- df2 %>% 
  ggplot() +
  geom_point(aes(x=Population,y=AgrUse,color=Flag)) +
  stat_function(fun=function(x) alpha*x+beta) +
  annotate("text",x=8e+05,y=2.0e+06,hjust=-.2,vjust=2,
           label=bquote(paste(R^2,"= ",.(R.square)))) +
  theme(legend.position="bottom")
plot(g4)

g5 <- df2 %>%
  ggplot() +
  geom_point(aes(x=Population,y=ApDem,color=Flag_Export2))
plot(g5)

g6 <- df2 %>%
  ggplot() +
  geom_point(aes(x=Population,y=ApDem,color=Flag_Export2))
plot(g6)

g <- g1 + g2 + g3 + g4 + g5 + g6 + plot_layout(ncol=2)
plot(g)
g <- g2 + g4 + g3 + plot_layout(ncol=3)
plot(g)

ggsave(paste0(odir,"/NF_plot.png"),width=15,height=5.5)

# df_growth <- df2 %>%
#   filter(Year!=2005) %>% 
#   mutate(Flag_Y=ifelse(Year<=2010,T,F)) %>%
#   group_by(R33) %>% 
#   mutate(GDP_CAP2=mean(GDP_CAP)) %>% 
#   group_by(R33,Flag_Y,GDP_CAP2) %>%
#   summarise(AgrUse_CAP2=mean(AgrUse_CAP)) %>% 
#   pivot_wider(names_from=Flag_Y,values_from=AgrUse_CAP2) %>% 
#   left_join(df_country) %>% 
#   drop_na() %>% 
#   ungroup() %>% 
#   mutate(GrowthRate=(`FALSE`-`TRUE`)/5)
# 
# g <- df_growth %>% 
#   ggplot() +
#   geom_point(aes(x=GDP_CAP2,y=GrowthRate,color=Flag))
# plot(g)
# 
# g <- df_growth %>% 
#   ggplot() +
#   geom_point(aes(x=GDP_CAP2,y=GrowthRate,color=Flag_Export2))
# plot(g)
# 
# g <- df2 %>% 
#   ggplot() +
#   geom_path(aes(x=Year,y=AgrUse_CAP,color=R33))
# plot(g)

# Regression --------------------------------------------------------------

df_reg <- df %>%
  rename(AgrUse=`Agricultural Use`)

df_global <- df_reg %>%
  group_by(Year) %>% 
  summarise(AgrUse=sum(AgrUse),Population=sum(Population))

g <- ggplot(df_global) +
  geom_point(aes(x=Population,y=AgrUse))
plot(g)

hoge <- lm(AgrUse~Population,data=df_reg)
alpha <- summary(hoge)$coefficients[2]
beta <- summary(hoge)$coefficients[1]

fuga <- lm(ApDem~Population,data=df_reg)
alpha <- summary(fuga)$coefficients[2]
beta <- summary(fuga)$coefficients[1]

piyo <- lm(GrowthRate~GDP_CAP2,data=df_growth)
alpha <- summary(piyo)$coefficients[2]
beta <- summary(piyo)$coefficients[1]


# g <- ggplot(df_reg) +
#   geom_point(aes(x=Population,y=AgrUse,color=NetExport)) +
#   stat_function(fun=function(x) alpha*x+beta)
# plot(g)
# 
# R27 <- df_reg %>% ungroup() %>% distinct(R33) %>% unlist()
# 
# df2 <- ind_t %>% 
#   mutate(Estimate=alpha*POP+beta) %>%
#   filter(R33%in%R27) %>% 
#   group_by(Y) %>% summarise(Estimate=sum(Estimate)) %>% 
#   filter(Y>2015) %>% rename(Year=Y) 
# 
# df3 <- df_reg %>%
#   mutate(Estimate=alpha*Population+beta) %>%
#   group_by(Year) %>%
#   summarise(AgrUse=sum(AgrUse),Estimate=sum(Estimate))
# 
# df4 <- bind_rows(df3,df2) %>% 
#   mutate(Value=ifelse(Year<2016,AgrUse,Estimate))
# 
# df5 <- df4 %>% 
#   filter(Year==2020|Year==2050) %>%
#   select(Year,Value) %>% 
#   pivot_wider(names_from=Year,values_from=Value) %>% 
#   mutate(hoge=`2050`/`2020`-1)
# 
# g <- df4 %>% 
#   ggplot() +
#   geom_path(aes(x=Year,y=Value))
# plot(g)
