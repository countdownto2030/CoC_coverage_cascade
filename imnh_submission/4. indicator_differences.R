# last edited 30 Jul 2025
# last run 30 Jul 2025
# Objective: get data files for survey-weighted and stratified results

rm(list=ls())
Start.time <- Sys.time()
########################################################### Load Libraries
library(ggplot2)
library(survey) #allows for design based analysis
library(RColorBrewer)
library(forcats)
library(tidyverse)
library(dplyr)

'%!in%' <- function(x,y)!('%in%'(x,y))

date = substr(date(),5,10)
dhs_date = "Mar  9"


location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
setwd(location)

########################################################### GET DATA FILES
countrydata <- read.csv(paste0(location,"/results/Jul 29_country_indicators.svy.csv"),colClasses=c("v023"="character"))
head(countrydata)

########################################################### RESHAPE WIDE TO CALCULATE DIFFERENCES

head(countrydata)

countrydata_wide <- countrydata %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(n, value, se, CIL, CIU),
    names_glue = "{indicator}_{.value}"
  )

dim(countrydata_wide)
head(countrydata_wide)

countrydata_wide$gap_pncwmq_value <- countrydata_wide$pncwm_value - countrydata_wide$pncwmq_value
countrydata_wide$gap_ancq_value <- countrydata_wide$anc1_value - countrydata_wide$ancq_value

View(countrydata_wide[,c("country","gap_pncwmq_value","pncwm_value","pncwmq_value")])
View(countrydata_wide[,c("country","gap_ancq_value","anc1_value","ancq_value")])

# Look at pooled difference in PNCwm and PNCwmq
countrydata_pooled <- read.csv(paste0(location,"/results/Jul 29_pooled_mean.svy.csv"),colClasses=c("v023"="character"))
head(countrydata_pooled)

# calculate the PNC contact-content gap for all women
0.8784832-0.3373207
# calculate the ANC contact-content gap for all women
0.9556957-0.7760014

