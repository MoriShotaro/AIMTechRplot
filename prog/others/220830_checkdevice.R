CTL <- c(1,-15000/7300*(1-0.119/0.194)-15000/7300*0.119/0.194,18.5,18.5*0.04+1.35)
CTLwCCS <- c(1,-15000/7300*(1-0.119/0.194)-15000/7300*0.119/0.194*(1-0.9),18.5+119*0.9*60/1000,(18.5+119*0.9*60/1000)*0.04+1.35)
BTL <- c(8760*3.6/1000*0.8*0.4,-8760*3.6/1000*0.8,125*1000/50/21*8760*3.6/1000*0.8*0.4,3.48*8760*3.6/1000*0.8)
OTL <- c(1,1,31.9*1000/(159*365*0.7*0.925*41.868/1000),3.3*1000/(159*365*0.925*41.868/1000))
OTLwCCS <- c(1,1,114*1000/(159*365*0.7*0.925*41.868/1000),3.3*1000/(159*365*0.925*41.868/1000))

df <- data.frame("CTL"=CTL,"CTLwCCS"=CTLwCCS,"BTL"=BTL,"OTL"=OTL,"OTLwCCS"=OTLwCCS) %>% 
  mutate(BTL=BTL/8760/3.6*1000/0.8/0.4)


df <- data.frame("CTL"=CTL,"CTLwCCS"=CTLwCCS,"BTL"=BTL,"OTL"=OTL,"OTLwCCS"=OTLwCCS) %>%
  mutate(Index=c("dev_inp","dev_out","dev_inv","dev_o_m")) %>%
  pivot_longer(cols=c(1:5),names_to="Variable",values_to = "Value") %>% 
  pivot_wider(names_from = Index, values_from = Value) %>% 
  mutate(inpinv=dev_inv/dev_inp) %>% 
  pivot_longer(cols=-c(Variable),names_to="Index",values_to="Value") %>% 
  filter(Index=="inpinv")
