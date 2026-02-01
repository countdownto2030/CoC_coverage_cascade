# last edited 1 Feb 2026
# last run 1 Feb 2026
# Objective: get data files for survey-weighted and stratified results

rm(list=ls())
Start.time <- Sys.time()
########################################################### Load Libraries
library(tidyverse)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

date = substr(date(),5,10)
dhs_date = "Feb  1"


location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
setwd(location)

########################################################### GET DATA FILES
countrydata <- read.csv(paste0(location,"/results/",dhs_date,"_country_indicators.svy.csv"),colClasses=c("v023"="character"))
head(countrydata)

pooleddata <- read.csv(paste0(location,"/results/",dhs_date,"_pooled_mean.svy.csv"),colClasses=c("v023"="character"))

########################################################### RESHAPE WIDE TO CALCULATE CONTACT-CONTENT DIFFERENCES
head(countrydata)
countrydata_wide <- countrydata %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(n, value, se, CIL, CIU),
    names_glue = "{indicator}_{.value}"
  )

dim(countrydata_wide)
head(countrydata_wide)

head(pooleddata)
pooleddata_wide <- pooleddata %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(n, value, se, CIL, CIU),
    names_glue = "{indicator}_{.value}"
  )

dim(pooleddata_wide)
head(pooleddata_wide)

########################################################### CONTACT - CONTENT GAPS - COUNTRY
countrydata_wide$gap_pncnq_value  <- countrydata_wide$pncn_value - countrydata_wide$pncnq_value
countrydata_wide$gap_pncwmq_value <- countrydata_wide$pncwm_value - countrydata_wide$pncwmq_value
countrydata_wide$gap_ancq_value <- countrydata_wide$anc1_value - countrydata_wide$ancq_value

View(countrydata_wide[,c("country","gap_ancq_value","anc1_value","ancq_value",
                         "gap_pncnq_value","pncn_value","pncnq_value",
                         "gap_pncwmq_value","pncwm_value","pncwmq_value")])

########################################################### CONTACT - CONTENT GAPS - POOOLED
pooleddata_wide$gap_pncnq_value  <- pooleddata_wide$pncn_value - pooleddata_wide$pncnq_value
pooleddata_wide$gap_pncwmq_value <- pooleddata_wide$pncwm_value - pooleddata_wide$pncwmq_value
pooleddata_wide$gap_ancq_value <- pooleddata_wide$anc1_value - pooleddata_wide$ancq_value

View(pooleddata_wide[,c("level","gap_ancq_value","anc1_value","ancq_value",
                         "gap_pncnq_value","pncn_value","pncnq_value",
                         "gap_pncwmq_value","pncwm_value","pncwmq_value")])

# USE countrydata_pooled
# calculate the PNC contact-content gap for all newborns
0.8636452-0.3305985
# calculate the PNC contact-content gap for all women
0.8738545-0.3271854
# calculate the ANC contact-content gap for all women
0.9164883-0.7298380





