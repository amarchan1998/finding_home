# Alejandra Marchan
# Universidad San Francisco de Quito
# Undergraduate Thesis
# Models without MENA and RICHOIL countires

# For Regressions Without MENA
df<-read.csv('without_mena.csv', fileEncoding = 'UTF-8-BOM')

# Load packages 

library(lmtest) # both for heteroskedasticty robust estimation
library(car)
library(sandwich) # for better tables with robust errors
library(tidyverse) # ggplot and others
library(broom) # tidy command for exporting
library(openxlsx) # for Excel exporting
library(stargazer) # for latex and text exporting
library(psych) # for descriptive statistics

# Run a simple model without MENA

reg1<-lm(ims_2015~lngdppc_2015+democracy_2015+cci_2015+hief_2013+timebuss_2015+
           lgpop_2015, data=df)
summary(reg1)

reg1h<-coeftest(reg1, vcov=hccm(reg1,type='hc1'))
print(reg1h)

# Models
reg2<-lm(ims_2015~democracy_2015*(lngdppc_2015)+cci_2015*(lngdppc_2015+hief_2013+
            timebuss_2015)+lgrefug_2015+lgpop_2015, data=df)
summary(reg2)
reg2h<-coeftest(reg2, vcov=hccm(reg2,type='hc1'))
print(reg2h)
