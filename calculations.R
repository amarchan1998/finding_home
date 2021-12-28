# Alejandra Marchan
# Universidad San Francisco de Quito
# Undergraduate Thesis
# Calculations with Model Statistics


# Load packages 

library(lmtest) # both for heteroskedasticty robust estimation
library(car)
library(sandwich) # for better tables with robust errors
library(tidyverse) # ggplot and others
library(broom) # tidy command for exporting
library(openxlsx) # for Excel exporting
library(stargazer) # for latex and text exporting
library(psych) # for descriptive statistics

# Load base
mig_df<-read.csv('cross_section_mig.csv', fileEncoding = 'UTF-8-BOM')

# Estimate Model 2.2 (Model 2 from Table 2)

mdl_int2<-lm(ims_2015~democracy_2015*(lngdppc_2015+africame)+
             cci_2015+hief_2013+lgpop_2015+timebuss_2015, data=mig_df)
summary(mdl_int2)

mdlh_int2<-coeftest(mdl_int2, vcov=hccm(mdl_int2,type='hc1'))
print(mdlh_int2)

# Extract betas from this model
b_int2<-coef(mdl_int2)

# Median for the log of GDP
mdn_lgdp<-median(mig_df$lngdppc_2015, na.rm=T)

# Calculate MPE for the democracy sign, not on NAME region
mpe_dem1<-b_int2['democracy_2015']+(mdn_lgdp*(b_int2['democracy_2015:lngdppc_2015']))
mpe_dem1

# Calculate practical effect 
sd_ims<-sd(mig_df$ims_2015, na.rm = T) # STDEV for ims
sd_dem<-sd(mig_df$democracy_2015, na.rm=T) # STDEV for democracy
pe_dem<-sd_dem*mpe_dem1
pe_dem