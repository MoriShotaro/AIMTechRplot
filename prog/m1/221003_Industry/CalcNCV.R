# Read original data without column names
df_ncv <- readLines(paste0(ddir,"/IEANCV.csv")) %>% 
  str_replace_all('",',"',") %>% 
  str_replace_all('^"|"$', "'") %>%
  str_replace_all(',"', ",'") %>% 
  paste(collapse='\n') %>% 
  fread(quote="'",skip=1)

# Read and join column names
cols <- fread(paste0(ddir,"/IEANCV.csv"),nrow=1) %>% unlist()
cols[[1]] <-  'COUNTRY'; cols[[2]] <-  'PRODUCT'
colnames(df_ncv) <- cols

PRODs <- c("Naphtha","Gas/diesel oil excl. biofuels")

df <- df_ncv %>% 
  select(1,2,3) %>% 
  rename("VALUE"=3) %>%
  mutate(VALUE=str_replace_all(VALUE,"x","-1")) %>%
  mutate(VALUE=as.numeric(VALUE)) %>%
  filter(VALUE>0) %>% 
  filter(PRODUCT%in%PRODs) %>% 
  group_by(PRODUCT) %>% 
  summarise(VALUE=floor(mean(VALUE)/1000*10)/10) %>% 
  rename("Variable"=1,"Value"=2) %>% 
  mutate(Variable=recode(Variable,Naphtha="N_OIL",`Crude oil`="N_CRU",`Gas/diesel oil excl. biofuels`="Gas oil")) %>% 
  bind_rows(data.frame(Variable=c("N_COL","N_CLP","N_NGS","N_LPG","Ammonia","Methanol"),
                       Value=c(25.8,25.8,54.4,46.2,18.6,19.9))) %>% 
  write_csv(paste0(ddir,"/NCV.csv"))

df_ncv %>% distinct(PRODUCT)


df2 <- df_ncv %>% 
  select(1,2,3) %>% 
  rename("VALUE"=3) %>%
  mutate(VALUE=str_replace_all(VALUE,"x","-1")) %>%
  mutate(VALUE=as.numeric(VALUE)) %>%
  filter(VALUE>0) %>% 
  filter(PRODUCT%in%PRODs) %>% 
  group_by(PRODUCT) %>% 
  summarise(VALUE=floor(mean(VALUE)/1000*10)/10) %>% 
  rename("Variable"=1,"Value"=2) %>% 
  mutate(Variable=recode(Variable,`Gas/diesel oil excl. biofuels`="GasOil")) %>% 
  bind_rows(data.frame(Variable=c("Ethane","Propane","Butane"),
                       Value=c(47.8,46.35,45.75))) %>% 
  write_csv(paste0(ddir,"/NCV_sc.csv"))
