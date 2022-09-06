# Library -----------------------------------------------------------------

library(tidyverse)
library(data.table)

tdir <- paste0(getwd(),"/tools")
year_set <- seq(2010,2100,5)


# Call subprogram ---------------------------------------------------------

# IAMC variable change
source(paste0(tdir,"/AR6/prog/inc_prog/IAMCvchg.R"))

# Fix AR6 scenario database
source(paste0(tdir,"/AR6/prog/inc_prog/FixAR6.R"))
