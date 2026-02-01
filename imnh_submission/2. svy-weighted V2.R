# last edited 29 Jul 2025
# last run 29 Jul 2025
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
countrydata <- read.csv(paste0(location,"/results/delivery_indicators.dhs_",dhs_date,".csv"),colClasses=c("v023"="character"))
head(countrydata)

data_master <- countrydata

# check covariates
table(data_master$wealth, exclude=NULL)

data_master$age[data_master$age==0]  <- "20-49"
data_master$age[data_master$age==2] <- 1
data_master$age[data_master$age==1] <- "15-19"
table(data_master$age, exclude=NULL)

data_master$urban.rural[data_master$urban.rural==0]  <- "rural"
data_master$urban.rural[data_master$urban.rural==1]  <- "urban"
table(data_master$urban.rural, exclude=NULL)

data_master$education[data_master$education %in% c(0)] <- "none"
data_master$education[data_master$education %in% c(1)] <- "primary"
data_master$education[data_master$education %in% c(2)] <- "secondary+"
table(data_master$education, exclude=NULL)

data_master$marital.status[data_master$marital.status==0] <- "never married"
data_master$marital.status[data_master$marital.status==1] <- "married/partnered"
data_master$marital.status[data_master$marital.status==2] <- "widowed/divorced/separated"
table(data_master$marital.status, exclude=NULL)

data_master$first.time.mother[data_master$first.time.mother==0] <- "no"
data_master$first.time.mother[data_master$first.time.mother==1] <- "yes"
table(data_master$first.time.mother, exclude=NULL)

dim(data_master)
data_master <- data_master %>% filter(country!='Philippines' | v023 != 40) # Only 1 PSU in this cluster which is problem for svyby
dim(data_master)

sort(unique(data_master$country))


data_master$sw <- data_master$v005/(10^6)
data_master$v001 <- 1
indicator_data <- data_master
########################################################### SURVEY-WEIGHTED ESTIMATES, COUNTRY
indicators= c("anc1","anc4","ancq","ideliv","pncwm","pncwmq","dps",
              "pncn","pncnq")
indiclist = list()
all_indic = list()



for(k in 1:length(sort(unique(indicator_data$country)))){
  country_indic_data <- subset(indicator_data, country %in% sort(unique(indicator_data$country))[k])
  
  # WEIGHTED ESTIMATES
  svydata1 <- svydesign(id=~caseid,strata=~v023, data=country_indic_data, weights=~sw, nest=TRUE)
  
  for(i in 1:length(indicators)){
    denom <- sum(table(country_indic_data[[indicators[i]]]))
    indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata1, svymean, na.rm=TRUE) 
    indic$n <- denom
    indic$CIL <- indic[,2] - 1.96*indic[,3]
    indic$CIU <- indic[,2] + 1.96*indic[,3]
    
    indic$indicator <- indicators[i]
    indic$country <- sort(unique(indicator_data$country))[k]
    names(indic)[names(indic) == indicators[i]] <- 'value'
    indic <- indic[,c("country","indicator","n","value","se","CIL","CIU")]
    
    print(sort(unique(indicator_data$country))[k])
    print(indic)
    indiclist[[i]] <- indic
  }
  
  all_indic[[k]] <- do.call(rbind,indiclist)
  
}

svy_data = do.call(rbind, all_indic)
svy_data
sort(unique(svy_data$country)) 

write.csv(svy_data,paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/",date,"_country_indicators.svy.csv"), row.names = FALSE)










########################################################### CASCADES
data <- data_master
data$pop <- 1

# WOMEN
data$casc_anc1 <- 0
data$casc_anc1[data$anc1==1] <- 1
table(data$casc_anc1, data$anc1, exclude=NULL)

data$casc_anc1_anc4 <- 0
data$casc_anc1_anc4[data$anc1==1 & data$anc4==1] <- 1
table(data$casc_anc1_anc4, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4, data$anc4, exclude=NULL)

data$casc_anc1_anc4_ancq <- 0
data$casc_anc1_anc4_ancq[data$anc1==1 & data$anc4==1 & data$ancq==1] <- 1
table(data$casc_anc1_anc4_ancq, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4_ancq, data$anc4, exclude=NULL)

data$casc_anc1_anc4_ancq_ideliv <- 0
data$casc_anc1_anc4_ancq_ideliv[data$anc1==1 & data$anc4==1 & data$ancq==1 & data$ideliv==1] <- 1
table(data$casc_anc1_anc4_ancq_ideliv, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv, data$anc4, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv, data$ideliv, exclude=NULL)

data$casc_anc1_anc4_ancq_ideliv_pncwm <- 0
data$casc_anc1_anc4_ancq_ideliv_pncwm[data$anc1==1 & data$anc4==1 & data$ancq==1 & data$ideliv==1 & data$pncwm==1] <- 1
table(data$casc_anc1_anc4_ancq_ideliv_pncwm, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm, data$anc4, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm, data$ideliv, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm, data$pncwm, exclude=NULL)

data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq <- 0
data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq[data$anc1==1 & data$anc4==1 & data$ancq==1 & data$ideliv==1 & data$pncwm==1 & data$pncwmq==1] <- 1
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq, data$anc4, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq, data$ideliv, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq, data$pncwm, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq, data$pncwmq, exclude=NULL)

data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps <- 0
data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps[data$anc1==1 & data$anc4==1 & data$ancq==1 & data$ideliv==1 & data$pncwmq==1 & data$dps==1] <- 1
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps, data$anc4, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps, data$ideliv, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps, data$pncwm, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps, data$pncwmq, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps, data$dps, exclude=NULL)



# NEWBORNS
data$casc_anc1_anc4_ancq_ideliv_pncn <- 0
data$casc_anc1_anc4_ancq_ideliv_pncn[data$anc1==1 & data$anc4==1 & data$ancq==1 & data$ideliv==1 & data$pncn==1] <- 1
table(data$casc_anc1_anc4_ancq_ideliv_pncn, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncn, data$anc4, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncn, data$ideliv, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncn, data$pncn, exclude=NULL)

data$casc_anc1_anc4_ancq_ideliv_pncn_pncnq <- 0
data$casc_anc1_anc4_ancq_ideliv_pncn_pncnq[data$anc1==1 & data$anc4==1 & data$ancq==1 & data$ideliv==1 & data$pncn==1 & data$pncnq==1] <- 1
table(data$casc_anc1_anc4_ancq_ideliv_pncn_pncnq, data$anc1, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncn_pncnq, data$anc4, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncn_pncnq, data$ideliv, exclude=NULL)
table(data$casc_anc1_anc4_ancq_ideliv_pncn_pncnq, data$pncnq, exclude=NULL)



########################################################### SURVEY-WEIGHTED CASCADES, COUNTRY
indicators <- c("casc_anc1","casc_anc1_anc4","casc_anc1_anc4_ancq","casc_anc1_anc4_ancq_ideliv","casc_anc1_anc4_ancq_ideliv_pncwm","casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq","casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps",
                "casc_anc1_anc4_ancq_ideliv_pncn","casc_anc1_anc4_ancq_ideliv_pncn_pncnq")

indic_country_casc = list()
all_country_casc = list()


for(k in 1:length(sort(unique(data$country)))){
  country_casc_data <- subset(data, country %in% sort(unique(data$country))[k])
  print(unique(country_casc_data$country))
  
 svydata1 <- svydesign(id=~caseid,strata=~v023, data=country_casc_data, weights=~sw, nest=TRUE)
  
 for(i in 1:length(indicators)){
  denom <- sum(table(country_casc_data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata1, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  indic$country <- sort(unique(data$country))[k]
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("country","indicator","n","value","se","CIL","CIU")]
  
  print(sort(unique(data$country))[k])
  print(indic)
  indic_country_casc[[i]] <- indic
}

  all_country_casc[[k]] <- do.call(rbind,indic_country_casc)
  
}

country_cascades = do.call(rbind, all_country_casc)
country_cascades
sort(unique(country_cascades$country)) 

write.csv(country_cascades,paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/",date,"_country_cascade.svy.csv"), row.names = FALSE)













########################################################### SURVEY-WEIGHTED CASCADES, POOLED
# count n for each country
sample_population_country <- data %>% group_by(country) %>%
  summarize(n = n())
sample_population_country$num = seq(1, by = 1, length.out = nrow(sample_population_country))
head(sample_population_country) 

# count n for total sample
sample_population_total <- data %>% summarize(n = n())
head(sample_population_total)

# data_cols <- data[,c("country","caseid","v001","v005",indicators)]
all_data <- merge(data,sample_population_country[,c("country","n","num")], by=c("country"))
names(all_data)[names(all_data)=="n"] <- "country_n"
all_data <- merge(all_data,sample_population_total[,c("n")])
names(all_data)[names(all_data)=="y"] <- "n"

# add weights inversely proportional to country size
all_data$prop_pooled <- all_data$country_n/all_data$n # each country's proportion of pooled sample
all_data$adj <- 1/all_data$prop_pooled     # inverse of proportion of pooled sample; adjustment to individual weights

all_data$sw <- all_data$v005/(10^6)
all_data$adj_sw <- all_data$adj*all_data$sw
all_data$v001 <- 1

all_data$pooled_strata <- paste0(all_data$num, all_data$v023)
all_data$pooled_caseid <- paste0(all_data$num, all_data$caseid)

indiclist_all = list()

# CASCADE TOTAL
head(all_data)

# need when more than one country:
svydata <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=all_data, weights=~adj_sw, nest=TRUE) # including strata affects standard error only, not estimate

for(i in 1:length(indicators)){
  denom <- sum(table(all_data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("indicator","n","value","se","CIL","CIU")]
  
  print(indic)
  indiclist_all[[i]] <- indic
}

pooled_svy_combined = do.call(rbind, indiclist_all)
pooled_svy_combined

write.csv(pooled_svy_combined,paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/",date,"_cascade.svy.csv"), row.names = FALSE)








###################################### FIGURE median value
median_list = list()

indicators= c("anc1","anc4","ancq","ideliv","pncwm","pncwmq","dps",
              "pncn","pncnq")

svydata <- svydesign(id=~pooled_caseid,strata=~pooled_strata, data=all_data, weights=~adj_sw, nest=TRUE) # including strata affects standard error only, not estimate

for(i in 1:length(indicators)){
  denom <- sum(table(all_data[[indicators[i]]]))
  indic <-svyby(make.formula(indicators[i]), ~v001 ,svydata, svymean, na.rm=TRUE) 
  indic$n <- denom
  indic$CIL <- indic[,2] - 1.96*indic[,3]
  indic$CIU <- indic[,2] + 1.96*indic[,3]
  
  indic$indicator <- indicators[i]
  indic$level <- "pooled standard"
  names(indic)[names(indic) == indicators[i]] <- 'value'
  indic <- indic[,c("indicator","n","level","value","se","CIL","CIU")]
  
  print(indic)
  median_list[[i]] <- indic
}

pooled_standard_svy_combined = do.call(rbind, median_list)
pooled_standard_svy_combined
write.csv(pooled_standard_svy_combined,paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/",date,"_pooled_mean.svy.csv"), row.names = FALSE)












