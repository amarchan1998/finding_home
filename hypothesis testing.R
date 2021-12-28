# Alejandra Marchan
# Universidad San Francisco de Quito
# Undergraduate Thesis
# Paper Script Draft 1 Hypothesis Testing

# Load packages 

library(lmtest) # both for heteroskedasticty robust estimation
library(car) # for wald tests
library(sandwich) # for better tables with robust errors
library(tidyverse) # ggplot and others
library(broom) # tidy command for exporting
library(openxlsx) # for Excel exporting
library(stargazer) # for latex and text exporting
library(psych) # for descriptive statistics

# Load database

mig_df<-read.csv('cross_section_mig.csv', fileEncoding = 'UTF-8-BOM')

# Make joint hypothesis testing for the models in the paper script

# Wald Test (F test for heteroskedasticity) for the public sector regression in Table 1

# Regression
mdl_ps<-lm(ims_2015~lngdppc_2015+unem_2015+timebuss_2015+lgpop_2015+cci_2015+
             democracy_2015+inmp_raise15+inmp_lower15+africame+
             govfcexp_2015+geduc_2015+currenthexp_2015,data=mig_df)
summary(mdl_ps)
mdlh_ps<-coeftest(mdl_ps, vcov=hccm)
print(mdlh_ps)

# Test
h0_t1<-c('unem_2015','timebuss_2015','lgpop_2015','inmp_raise15','africame','govfcexp_2015')
linearHypothesis(mdl_ps,h0_t1,vcov=hccm)

# Wald Test for the hief and other previously significant model in Table 2 richoil model

# Regression
mdl_oil<-lm(ims_2015~lngdppc_2015+timebuss_2015+hief_2013+cci_2015+democracy_2015+
              democracy_2015*lngdppc_2015+richoil_2015+
              lgrefug_2015+lgpop_2015+richoil_2015*democracy_2015
            +richoil_2015*lngdppc_2015+ cci_2015*lngdppc_2015, data=mig_df)

# Joint hypothesis testing
h0_t2a<-c('timebuss_2015','hief_2013')
linearHypothesis(mdl_oil,h0_t2a,vcov=hccm)


# Wald test for democracy square functional form. 

# 4. Rich oil and some interactions

# Estimate model
mdl_demsq<-lm(ims_2015~poly(democracy_2015, 2, raw=T)+richoil_2015*(hief_2013+lngdppc_2015)
              +lgrefug_2015+lgpop_2015+cci_2015*(lngdppc_2015+hief_2013+timebuss_2015),
              data=mig_df)
summary(mdl_demsq)
mdlh_demsq<-coeftest(mdl_demsq, vcov=hccm(mdl_demsq,type='hc1'))
print(mdlh_demsq)

# Wald Test of all democracy

linearHypothesis(mdl_demsq, matchCoefs(mdl_demsq,'democracy_2015'), 
                 vcov=hccm(mdl_demsq,type='hc1'))

# ANOVA

Anova(mdl_demsq, vcov=hccm(mdl_demsq, type='hc1'))

# With the government expenditure model (in final paper script)

mdl_ps<-lm(ims~log(gdp_pc)+unem+timebuss+cci+democracy+imp+mena+hief+govexp,data=df)

h0<-c('govexp','impLower','impRaise', 'unem')
linearHypothesis(mdl_ps,h0,vcov=hccm(mdl_ps, type='hc1'))

# With the model with 3 interactions

mdl_int3<-lm(ims~log(gdp_pc)*(democracy+mena)+(democracy*mena)+timebuss+cci+hief, data=df)
summary(mdl_int3)

mdlh_int3<-coeftest(mdl_int3, vcov=hccm(mdl_int3,type='hc1'))
print(mdlh_int3)

# Test democracy

linearHypothesis(mdl_int3, matchCoefs(mdl_int3,'democracy'),vcov=hccm(mdl_int3,type='hc1'))

# Test GDP

linearHypothesis(mdl_int3, matchCoefs(mdl_int3,'gdp_pc'),vcov=hccm(mdl_int3,type='hc1'))

# Other model with richoil

mdl_roil<-lm(ims~log(gdp_pc)*(democracy+richoil)+(democracy*richoil)+timebuss+cci+hief, data=df)
summary(mdl_roil)

mdlh_roil<-coeftest(mdl_roil, vcov=hccm(mdl_roil,type='hc1'))
print(mdlh_roil)


# Test democracy for this new model

linearHypothesis(mdl_roil, matchCoefs(mdl_roil,'democracy'),vcov=hccm(mdl_roil,type='hc1'))

# Test GDP in this new model

linearHypothesis(mdl_roil, matchCoefs(mdl_roil,'gdp_pc'),vcov=hccm(mdl_roil,type='hc1'))

# Test Richoil

linearHypothesis(mdl_roil, matchCoefs(mdl_roil,'richoil'),vcov=hccm(mdl_roil,type='hc1'))

# For model without Mena

mdl_wmena<-lm(ims~log(gdp_pc)*(democracy)+timebuss+cci+hief, data=df1)
summary(mdl_wmena)

mdlh_wmena<-coeftest(mdl_wmena, vcov=hccm(mdl_wmena,type='hc1'))
print(mdlh_wmena)

h0w<-c('democracy','log(gdp_pc):democracy')

linearHypothesis(mdl_wmena,h0w,vcov=hccm)


