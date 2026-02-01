# last edited 27 Jan 2026
# last run 27 Jan 2026
# Objective: find point estimates for paper results
#            format results

rm(list=ls())
Start.time <- Sys.time()
########################################################### Load Libraries
library(ggplot2)
library(survey) #allows for design based analysis
library(RColorBrewer)
library(forcats)
library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)

'%!in%' <- function(x,y)!('%in%'(x,y))

date = substr(date(),5,10)
dhs_date = "Dec  9"


location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
setwd(location)

########################################################### GET DATA FILES
countrydata <- read.csv(paste0(location,"/results/",dhs_date,"_country_indicators.svy.csv"),colClasses=c("v023"="character"))
head(countrydata)

svydata_pooled <- read_csv(paste0(location,"/Results/",dhs_date,"_pooled_mean.svy.csv"))
head(svydata_pooled)
names(svydata_pooled)[names(svydata_pooled)=="level"] <- "country"
svydata_pooled <- svydata_pooled[ , intersect(colnames(countrydata),colnames(svydata_pooled))]

countrydata <- cbind(countrydata[,1:3],round(countrydata[,4:7],3))
countrydata <- countrydata %>% arrange(indicator, country)

svydata_pooled <- cbind(svydata_pooled[,1:3],round(svydata_pooled[,4:7],3))
svydata_pooled <- svydata_pooled %>% arrange(indicator, country)

supplementary_file1 <- rbind(countrydata, svydata_pooled)


write.xlsx(supplementary_file1,"/Users/EWilson/Desktop/DAC/QoC_continuum/supplementary_file1.xlsx")


########################################################### figure 1 write-up
unique(countrydata$indicator)

pncnq_data <- subset(countrydata, indicator == "pncnq")
pncnq_data <- pncnq_data %>% arrange(value)

pncwmq_data <- subset(countrydata, indicator == "pncwmq")
pncwmq_data <- pncwmq_data %>% arrange(value)


pncn_data <- subset(countrydata, indicator == "pncn")
pncn_data <- pncn_data %>% arrange(value)

pncwm_data <- subset(countrydata, indicator == "pncwm")
pncwm_data <- pncwm_data %>% arrange(value)



# View(svydata_pooled)


########################################################### figure 2 write-up
cascade_pooled <- read.csv(paste0(location,"/results/",dhs_date,"_cascade.svy.csv"),colClasses=c("v023"="character"))
cascade_pooled

cascade_country <- read.csv(paste0(location,"/results/",dhs_date,"_country_cascade.svy.csv"),colClasses=c("v023"="character"))
cascade_country

head(cascade_country)

cascade_country_wide <- cascade_country %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(n, value, se, CIL, CIU),
    names_glue = "{indicator}_{.value}"
  )

dim(cascade_country_wide)
colnames(cascade_country_wide)

########################################################### country-level PNCwm vs PNCn & qPNCwm vs qPNCn
cascade_country_wide$pncwm_vs_pncn <- cascade_country_wide$casc_anc1_anc4_ancq_ideliv_pncwm_value  - cascade_country_wide$casc_anc1_anc4_ancq_ideliv_pncn_value

cascade_country_wide$quality_pncwm_vs_pncn <- cascade_country_wide$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_value - cascade_country_wide$casc_anc1_anc4_ancq_ideliv_pncn_pncnq_value

View(cascade_country_wide[,c("country","pncwm_vs_pncn","quality_pncwm_vs_pncn")])

View(cascade_country_wide[,c("country","casc_anc1_anc4_ancq_ideliv_pncwm_value","casc_anc1_anc4_ancq_ideliv_pncn_value","casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_value","casc_anc1_anc4_ancq_ideliv_pncn_pncnq_value")])





