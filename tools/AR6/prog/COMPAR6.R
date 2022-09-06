# Library -----------------------------------------------------------------

library(tidyverse)
library(gdxrrw)
library(stringr)
library(data.table)


# Make output directory ---------------------------------------------------

if(!dir.exists(paste(odir,"AR6",sep="/"))){dir.create(paste(odir,"AR6",sep="/"),recursive=T)}
pdf(paste(odir,'AR6','plot_main.pdf',sep="/"),width=10,height=7)

# Input data --------------------------------------------------------------

dfAR6 <- fread(paste0(tdir,"/AR6/output/AR6_Scenario_Database_Enduse.csv"),stringsAsFactors=FALSE,header=TRUE) %>% 
  pivot_longer(cols=c(Y2010:Y2100),names_to="Year",values_to="Value",names_prefix="Y") %>%
  mutate(Year=as.numeric(Year)) %>% 
  drop_na(Value)

source(paste0(tdir,"/AR6/prog/inc_prog/func_AR6.R"))

# Scenario selection ------------------------------------------------------

nmls <- c("MESSAGE-GLOBIOM 1.0|SSP2-Baseline","AIM/Hub-Global 2.0|Baseline","WITCH-GLOBIOM 3.1|SSP2-Baseline",
          "IMAGE 3.2|SSP2-baseline","REMIND-MAgPIE 1.5|SSP2-Baseline")
scls <- c("CD-LINKS_INDCi","CO_CurPol","EN_NPi2100","Baseline","EN_INDCi2100","CD-LINKS_NPi","EMF30_Baseline","SSP2-Baseline",
          "SSP2-baseline","EMF33_Baseline")
mdls<- c("AIM/CGE 2.1","AIM/CGE2.2","AIM/Hub-Global 2.0")
ctls<- c()


# Plot --------------------------------------------------------------------

fplot_boxpath("Fin_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bmodel=mdls,bcat=ctls,
              byear=c(2050,2100),name_file="Fin_Ene_BP",ylabel="Final energy comsumption (EJ)",cpt="Fin_Ene")
fplot_jitterpath("Fin_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bscen=scls,
                 byear=c(2050,2100),name_file="Fin_Ene_JP",ylabel="Final energy comsumption (EJ)",cpt="Fin_Ene",labels=T)
fplot_jitterpath("Fin_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,
                 byear=c(2050,2100),name_file="Fin_Ene_JP_CGE",ylabel="Final energy comsumption (EJ)",cpt="Fin_Ene",labels=T)
fplot_linepath("Fin_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="Fin_Ene_LP_CGE",ylabel="Final energy comsumption (EJ)",cpt="Fin_Ene",labels=T)
fplot_linepath("Sec_Ene_Ele",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="Sec_Ele_LP_CGE",ylabel="Power generation (EJ)",cpt="Sec_Ene_Ele")
fplot_linepath("Emi_CO2_Ene_and_Ind_Pro",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="Emi_CO2_FFI",ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')),cpt="Emi_CO2_FFI",labels=T)



# fplot_linepath('Emi_CO2_Ene_and_Ind_Pro',scen="500C",bmodel=mdls,bcat=c("C1"),name_file='EMI_CO2_FFI_LP', ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')), cpt='Emi_CO2FFI',ylim=c(NA,NA))
fplot_linepath('Prc_Car',scen=c("SR15_LOS"),bmodel=mdls,bcat=c("C1"),name_file='Prc_Car', ylabel=expression(paste('Carbon price (US$/t-',CO[2],')')), cpt='Prc_Car',ylim=c(NA,NA))
fplot_jitterpath('Prc_Car',scen=c("SR15_LOS"),bcat=c("C1"),byear=c(2050,2100),name_file='Prc_Car', ylabel=expression(paste('Carbon price (US$/t-',CO[2],')')), cpt='Prc_Car')
fplot_bandspath('Prc_Car',scen=c("SR15_LOS"),bcat=c("C1"),name_file='Prc_Car_C1', ylabel=expression(paste('Carbon price (US$/t-',CO[2],')')), cpt='Prc_Car')
fplot_bandspath('Prc_Car',scen=c("SR15_HOS"),bcat=c("C2"),name_file='Prc_Car_C2', ylabel=expression(paste('Carbon price (US$/t-',CO[2],')')), cpt='Prc_Car')
fplot_bandspath('Fin_Ene',scen=c("SR15_LOS"),bcat=c("C1"),name_file='Fin_Ene_RP_C1', ylabel='Final energy comsumption (EJ)', cpt='Fin_Ene')
fplot_bandspath('Fin_Ene',scen=c("SR15_HOS"),bcat=c("C2"),name_file='Fin_Ene_RP_C2', ylabel='Final energy comsumption (EJ)', cpt='Fin_Ene')
fplot_bandspath("Emi_CO2_Ene_and_Ind_Pro",scen=c("SR15_LOS"),bcat=c("C1"),name_file='Emi_CO2_FFI_C1', ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')), cpt='Emi_CO2_FFI')
fplot_bandspath("Emi_CO2_Ene_and_Ind_Pro",scen=c("SR15_HOS"),bcat=c("C2"),name_file='Emi_CO2_FFI_C2', ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')), cpt='Emi_CO2_FFI')


var_prm <- c("Win","Solar","Coa","Oil","Gas")
fplot_lineshare(var_prm,"Prm_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="SHR_prm",ylabel="Share in primary energy (%)",cpt="SHR_prm")

var_ele <- c("Win","Solar","Coa","Gas")
fplot_lineshare(var_ele,"Sec_Ene_Ele",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="SHR_ele",ylabel="Share in power generation (%)",cpt="SHR_sec")

var_fin <- c("Liq","SolidCoa","Gas","Ele","Hyd")
fplot_boxshare(var_fin,"Fin_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,byear=c(2050,2100),name_file="SHR_fin",ylabel="Share in final energy (%)",cpt="SHR_fin")
fplot_jittershare(var_fin,"Fin_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,byear=c(2050,2100),name_file="SHR_fin_JS",ylabel="Share in final energy (%)",cpt="SHR_fin")

# fplot_boxshare(var_finind,"Fin_Ene_Ind",scen="500C",bname=nmls,byear=c(2050,2100),name_file="SHR_finind",ylabel="Share in final energy (%)",cpt="SHR_finind")
# fplot_jittershare(var_fin,"Fin_Ene_Ind",scen="500C",bmodel=mdls,bcat=c("C1"),byear=c(2050,2100),name_file="SHR_finind_JS",ylabel="Share in final energy (%)",cpt="SHR_finind")
# 
# fplot_boxshare(var_finbui,"Fin_Ene_Res_and_Com",scen="500C",bname=nmls,byear=c(2050,2100),name_file="SHR_finbui",ylabel="Share in final energy (%)",cpt="SHR_finbui")
# fplot_jittershare(var_fin,"Fin_Ene_Res_and_Com",scen="500C",bmodel=mdls,bcat=c("C1"),byear=c(2050,2100),name_file="SHR_finbui_JS",ylabel="Share in final energy (%)",cpt="SHR_finbui")
# 
# fplot_boxshare(var_fintra,"Fin_Ene_Tra",scen="500C",bname=nmls,byear=c(2050,2100),name_file="SHR_fintra",ylabel="Share in final energy (%)",cpt="SHR_fintra")
# fplot_jittershare(var_fin,"Fin_Ene_Tra",scen="500C",bmodel=mdls,bcat=c("C1"),byear=c(2050,2100),name_file="SHR_fintra_JS",ylabel="Share in final energy (%)",cpt="SHR_fintra")

fplot_lineshare(var_fin,"Fin_Ene",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="SHR_fin_Baseline",ylabel="Share in final energy (%)",cpt="SHR_fin")
fplot_lineshare(var_fin,"Fin_Ene_Ind",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="SHR_finind_Baseline",ylabel="Share in final energy (%)",cpt="SHR_finind")
fplot_lineshare(var_fin,"Fin_Ene_Res_and_Com",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="SHR_finbui_Baseline",ylabel="Share in final energy (%)",cpt="SHR_finbui")
fplot_lineshare(var_fin,"Fin_Ene_Tra",scen=c("Baseline","Baseline_w/o_CTLGTL"),bname=nmls,name_file="SHR_fintra_Baseline",ylabel="Share in final energy (%)",cpt="SHR_fintra")

dev.off()
