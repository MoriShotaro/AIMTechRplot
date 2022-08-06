# Make output directory ---------------------------------------------------

if(!dir.exists(paste(odir,"general",sep="/"))){dir.create(paste(odir,"general",sep="/"),recursive=T)}

# Primary energy ----------------------------------------------------------
fplot_area(var=prmene, totvar='Prm_Ene', name_file='PES', ylabel='Primary energy supply (EJ/yr)', cpt='Primary Energy', w=200, h=120)
fplot_fbar(var=prmene, name_file='PES', ylabel='Primary energy supply (EJ/yr)', cpt='Primary Energy', w=200, h=120)

# Secondary energy --------------------------------------------------------

fplot_area(var=secele, totvar='Sec_Ene_Ele', name_file='ELE', ylabel='Electricity generation (EJ/yr)', cpt='Power generation', w=200, h=120)
fplot_fbar(var=secele, name_file='ELE', ylabel='Electricity generation (EJ/yr)', cpt='Power generation', w=200, h=120)
fplot_area(var=sechyd, totvar='Sec_Ene_Hyd',name_file='HYD',ylabel='Hydrogen generation (EJ/yr)',cpt='Hydrogen generation', w=200, h=120)
fplot_fbar(var=sechyd, name_file='HYD',ylabel='Hydrogen generation (EJ/yr)',cpt='Hydrogen generation', w=200, h=120)


# Final energy ------------------------------------------------------------

fplot_area(var=finene, totvar='Fin_Ene', name_file='FEC_src', ylabel='Final energy consumption (EJ/yr)', cpt='Final Energy', w=200, h=120)
fplot_fbar(var=finene, name_file='FEC_src', ylabel='Final energy consumption (EJ/yr)', cpt='Final Energy', w=200, h=120)
fplot_area(var=finind, totvar='Fin_Ene_Ind', name_file='FEC_ind', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Ind', w=200, h=120)
fplot_fbar(var=finind, name_file='FEC_ind', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Ind', w=200, h=120)
fplot_area(var=finbui, totvar='Fin_Ene_Res_and_Com', name_file='FEC_bld', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Buildings', w=200, h=120)
fplot_fbar(var=finbui, name_file='FEC_bld', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Buildings', w=200, h=120)
fplot_area(var=finres, totvar='Fin_Ene_Res', name_file='FEC_res', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Res', w=200, h=120)
fplot_fbar(var=finres, name_file='FEC_res', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Res', w=200, h=120)
fplot_area(var=fincom, totvar='Fin_Ene_Com', name_file='FEC_com', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Com', w=200, h=120)
fplot_fbar(var=fincom, name_file='FEC_com', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Com', w=200, h=120)
fplot_area(var=fintra, totvar='Fin_Ene_Tra', name_file='FEC_trp', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Tra', w=200, h=120)
fplot_fbar(var=fintra, name_file='FEC_trp', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Tra', w=200, h=120)
fplot_area(var=finpss, totvar='Fin_Ene_Tra_Pss', name_file='FEC_pss', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Passenger', w=200, h=120)
fplot_fbar(var=finpss, name_file='FEC_pss', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Passenger', w=200, h=120)
fplot_area(var=finfre, totvar='Fin_Ene_Tra_Fre', name_file='FEC_frg', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Freight', w=200, h=120)
fplot_fbar(var=finfre, name_file='FEC_frg', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Freight', w=200, h=120)
fplot_area(var=finroa, totvar='Fin_Ene_Tra_Roa', name_file='FEC_roa', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Road', w=200, h=120)
fplot_fbar(var=finroa, name_file='FEC_roa', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Road', w=200, h=120)
fplot_area(var=finavi, totvar='Fin_Ene_Tra_Avi', name_file='FEC_avi', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Aviation', w=200, h=120)
fplot_fbar(var=finavi, name_file='FEC_avi', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Aviation', w=200, h=120)
fplot_area(var=finshi, totvar='Fin_Ene_Tra_Shi', name_file='FEC_shi', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Shipping', w=200, h=120)
fplot_fbar(var=finshi, name_file='FEC_shi', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Shipping', w=200, h=120)
fplot_area(var=finrai, totvar='Fin_Ene_Tra_Rai', name_file='FEC_rai', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Rail', w=200, h=120)
fplot_fbar(var=finrai, name_file='FEC_rai', ylabel='Final energy consumption (EJ/yr)', cpt='Fin_Ene_Rail', w=200, h=120)


# Emission ----------------------------------------------------------------

fplot_path(var='Emi_CO2', name_file='EMI_CO2', ylim=c(NA,NA), ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')), cpt='Emi_CO2')
fplot_path(var='Emi_CO2_Ene', name_file='EMI_CO2_ENE', ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')), cpt='Emi_CO2_Ene')
fplot_path(var='Emi_CO2_Ene_and_Ind_Pro', name_file='EMI_CO2_FFI', ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')), cpt='Emi_CO2FFI',ylim=c(NA,NA))
fplot_area(var=emisec, totvar='Emi_CO2_Ene_and_Ind_Pro', name_file='EMI_CO2_area', ylabel=expression(paste(CO[2],' emissions (Mt-',CO[2],'/yr)')), cpt='Emi_CO2FFI', w=200, h=120)
fplot_area(var=carcap, totvar='Car_Cap', name_file='Car_Cap', ylabel=expression(paste('Carbon capture (Mt-',CO[2],'/yr)')), cpt='Car_Cap', w=200, h=120)
fplot_fbar(var=carcap, name_file='Car_Cap', ylabel=expression(paste('Carbon capture (Mt-',CO[2],'/yr)')), cpt='Car_Cap', w=200, h=120)
fplot_area(var=carseq, totvar='Car_Seq_Geo_sto', name_file='Car_Seq', ylabel=expression(paste('Carbon sequestration/utilization (Mt-',CO[2],'/yr)')), cpt='Car_Seq', w=200, h=120)
fplot_fbar(var=carseq, name_file='Car_Seq', ylabel=expression(paste('Carbon sequestration/utilization (Mt-',CO[2],'/yr)')), cpt='Car_Seq', w=200, h=120)


# Cost --------------------------------------------------------------------

fplot_path(var='Prc_Car', name_file='Cost_Price_carbon', ylabel=expression(paste('Carbon price (US$/t-',CO[2],')')), cpt='Prc_Carbon')
fplot_path(var='Pol_Cos_Add_Tot_Ene_Sys_Cos', name_file='Cost_Policy_cost', ylabel='billion US$/yr', cpt='Energy system cost')
fplot_area(var=invadd, totvar='Inv_Add', name_file='Cost_INV', ylabel='billion US$/yr', cpt='Investment Additions', w=200, h=120)

pdf(paste(odir,'general','plot_main.pdf',sep="/"),width=10,height=7)
for (i in plist){
  plot(i)
}
dev.off()
