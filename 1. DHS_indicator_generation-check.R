#******************************************************************#
#   Creating Delivery Indicators from DHS 8 
#         COUNTDOWN TO 2030 
#         VER. 1 FEB 2026
#******************************************************************#
options(echo=F)

rm(list=ls())
Start.time <- Sys.time()
date = substr(date(),5,10)

########################################################### Load Libraries
library(foreign) # reads in data from STATA file format
library(readstata13)
library(survey)  # allows for design based analysis
library(tidyverse)
library(haven)
library(reshape2)

'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Set Locations 
location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"

data_location  = "/Users/EWilson/Desktop/DAC/QoC_continuum" # folder with survey data sets
export_location = "/Users/EWilson/Desktop/DAC/QoC_continuum/results/"
CDdata = "/ReadyforAnalysis_2025-11-30.csv" # list of country data sets to be analyzed

########################################################### GET DATA FILES
CDdata <- read_csv(paste0(location,CDdata))

# dhs_full <- CDdata %>% filter(source=="DHS")

complete_countries <- sort(c("Angola","Bangladesh","BurkinaFaso","Cambodia","DRC","CotedIvoire",
                             "Ghana","Jordan","Kenya","Lesotho","Mali","Mozambique",
                             "Nepal","Nigeria","Philippines","Senegal","Tajikistan","Tanzania","Zambia"))

dhs_full <- CDdata %>% filter(country %in% complete_countries)
dhs <- dhs_full 
# dhs <-dhs_full[which(dhs_full$country %in% c("Mozambique")),] #temp (non-loop)


indicators= c("anc1","anc1timing","anc4","ancq","ideliv","pncwm","pncwmq1","pncwmq2","pncwmq3","pncwmq","dps",
              "pncn","pncnq1","pncnq2","pncnq3","pncnq4","pncnq5","pncnq")

covariates <- c('wealth','urban.rural','age','education','marital.status','first.time.mother','region')

#****************************************************************************#
#**************** DHS SURVEYS INDICATOR CODING ******************************

##****************************************************************************
##*                     IMPORT DHS DATA SETS
##**************************************************************************##
# for(index in 1:nrow(dhs) )  { # start DHS loop
index <- 1
  year <- dhs$year[index]
  country <- dhs$country[index]
  folder <- paste0("data/",country)
  file <- paste0(dhs$births[index],".DTA")
  path <- paste(data_location,folder,sep="/")
  file.name<-paste(path,file,sep="/")
  dat <-read.dta13(file.name, convert.factors=FALSE)

  # standardize col names to lower case
  colnames(dat) <- tolower(names(dat))
  dat <- dat %>% filter(v015==1) # completed interview
  
  ##****************************************************************************
  ##*                       DATA SET-UP
  ##****************************************************************************
  
  dat$agemo <- dat$v008 - dat$b3 # child's age
  dat <- dat %>% filter(midx==1) %>% filter(agemo < 24) # most recent births in the <2 years only
  dat$birthage <- dat$v008 - dat$agemo # date of interview - baby's age = age of the woman at time of giving birth
  
  ##****************************************************************************
  ##*                  QUALITY INDEX INDICATOR CODING
  ##****************************************************************************
  #1. At least one antental care visit
  #2. Timing of first antental care visit in the first 3m - variable m13
  #3. At least four antenatal care visits
  #4. Proportion of women who gave birth at a facility
  #   include other and DK as home births
  
  #5. Proportion of women who received a postnatal check within 2 days of giving birth
  #6. Proportion of women for whom: bp measured, vaginal bleeding discussed, family planning discussed
  #7. FP indicator
  
  #8. Proportion of newborns who received a postnatal check within 2days of giving birth
  #9. Proprtion of newborns for whom: cord examined, temp measured, 
  #   signs med attn. needed were checked, mother counseled on bf, bf observed

  ## 1. ANC1: Proportion of women who attended one or more antenatal care contact visits with any provider, 
  ##          during their most recent pregnancy in the two years preceding the survey.

  dat$anc1 <- NA
  dat$anc1[!is.na(dat$m14) & dat$m14!=98]  <- 0
  dat$anc1[dat$m14>=1 & dat$m14<98] <- 1
  table(dat$m14, dat$anc1, exclude=NULL)
  
  ## 2. anc1timing: Proportion of women who were seen for their first ANC visit during their first trimester.
  
  dat$anc1timing <- NA
  dat$anc1timing[!is.na(dat$m13) & dat$m13!=98]  <- 0
  dat$anc1timing[dat$m13>=1 & dat$m13<=3] <- 1
  table(dat$m13a, dat$m13, exclude = NULL)
  table(dat$m13, dat$anc1timing, exclude = NULL)
  
  table(dat$anc1, dat$anc1timing, exclude=NULL)
  
  ## 3. ANC4: Proportion of women who attended four or more antenatal care contact visits with any provider, 
  ##          during their most recent pregnancy in the two years preceding the survey.
  
  dat$anc4 <- NA
  dat$anc4[!is.na(dat$m14) & dat$m14!=98]  <- 0
  dat$anc4[dat$m14>=4 & dat$m14<98] <- 1
  table(dat$m14, dat$anc4, exclude=NULL)
  
  ## ANCq Proportion of women who received standard interventions
  # Std interventions: a) bp measurement, b) urine sample taken, c) blood sample taken
  # BP measurement
  dat$ancq1 <- NA
  dat$ancq1[dat$m42c==0] <- 0 
  dat$ancq1[dat$m42c==1] <- 1 
  
  # urine sample
  dat$ancq2 <- NA
  dat$ancq2[dat$m42d==0] <- 0 
  dat$ancq2[dat$m42d==1] <- 1 
  
  # blood sample
  dat$ancq3 <- NA
  dat$ancq3[dat$m42e==0] <- 0 
  dat$ancq3[dat$m42e==1] <- 1 
  
  # check how best to do this  
  # dat$pncwmq <- (rowSums(dat[ , c("pncwmq1","pncwmq2","pncwmq3")], na.rm=TRUE))/3
  dat$ancq <- 0
  dat$ancq[dat$ancq1==1 & dat$ancq2==1 & dat$ancq3==1] <- 1
  table(dat$ancq, exclude = NULL)
  table(dat$ancq, dat$ancq1, exclude = NULL)
  table(dat$ancq, dat$ancq2, exclude = NULL)
  table(dat$ancq, dat$ancq3, exclude = NULL)
  
  table(dat$m14, dat$ancq, exclude=NULL)
  table(dat$m14, dat$anc1, exclude=NULL)
  table(dat$anc1, dat$ancq, exclude=NULL)
  
  ## 4. IDELIV: Proportion of women who gave birth in a health facility (binary)
  
  home <- c(11,12,13,96) # same for all surveys
  facility <- c(21:42) # same for all surveys

  dat$faclevel <- NA # No response
  dat$faclevel[(dat$m15 %in% home)] <- 0
  dat$faclevel[(dat$m15 %in% facility)] <- 1
  
  dat$ideliv <- NA
  dat$ideliv[dat$faclevel==0] <- 0 # home birth
  dat$ideliv[dat$faclevel>0] <- 1 # institutional birth

  
  #5. PNCWM Proportion of women who received a postnatal check within 2 days of giving birth
  # m63 - how long before discharging respondent health check took place
  # m67 - how long after discharge/delivery at home respondent health check took place
  # includes all women (home and facility births)
  
  dat$pncwm=NA
  
    dat$pncwm[ !(is.na(dat$m63)) | !(is.na(dat$m67))] <- 0
    table(dat$m63, dat$pncwm, exclude=NULL)
    table(dat$m67, dat$pncwm, exclude=NULL)
    dat$pncwm[(dat$m63 <= 201 & dat$ideliv==1)] <- 1 # facility births checked at facility <2 days after delivery
    dat$pncwm[(dat$m67 <= 201 & dat$ideliv==0)] <- 1 # home births checked <2 days after delivery
    dat$pncwm[(dat$m67 <= 201 & dat$ideliv==1)] <- 1 # facility births checked at home <2 days after delivery
    
    table(dat$m63, dat$pncwm, exclude=NULL)
    table(dat$m67, dat$pncwm, exclude=NULL)
    
    # m63 and m67 which are 998 will be set to zero unless they are less than 2d in other variable - see example below
    test <- subset(dat, pncwm==1 & m67==998 )
    table(test$m62) # replied yes
    table(test$m63)
    table(test$m66) # replied yes
    table(test$m67) # replied dk
    
    # Set don't know responses to zero- don't drop from data set
    # dat$pncwm[dat$m63 >= 998 & dat$ideliv==1] <- 0 # DK responses (facility births checked at facility)
    # dat$pncwm[(dat$m67 >=998 & dat$ideliv==0)] <- 0 # DK responses (home births)
    # dat$pncwm[(dat$m67 >=998 & dat$ideliv==1)] <- 0 # DK responses (facility births checked at home)

  
  table(dat$pncwm,useNA="ifany")  
  
  # RESPECT - Proportion of women who were treated with respect during delivery
  # NO SURVEY Q RESPONSES CURRENTLY AVAILABLE FOR INDICATORS:
  
  #6. PNCWM CONTENT - Proportion of women with PNC visits who received standard interventions
  # Std interventions: a) bp measurement, b) vaginal bleeding check, c) FP counseling
  # BP measurement
  dat$pncwmq1 <- NA
  dat$pncwmq1[dat$m78f==0] <- 0 
  dat$pncwmq1[dat$m78f==1] <- 1 
  
  # vaginal bleeding check
  dat$pncwmq2 <- NA
  dat$pncwmq2[dat$m78g==0] <- 0 
  dat$pncwmq2[dat$m78g==1] <- 1 
  
  # FP counseling
  dat$pncwmq3 <- NA
  dat$pncwmq3[dat$m78h==0] <- 0 
  dat$pncwmq3[dat$m78h==1] <- 1 
  
# PNCwm quality
  # dat$pncwmq <- (rowSums(dat[ , c("pncwmq1","pncwmq2","pncwmq3")], na.rm=TRUE))/3
  
  # dat$pncwmq <- 0
  # dat$pncwmq[dat$pncwmq1==1 & dat$pncwmq2==1 & dat$pncwmq3==1] <- 1
  # table(dat$pncwmq, exclude = NULL)
  
# should the PNCq indicator be:
# should we exclude the records which do not have all three responses?
# For the women who responded to the pncwm question, we want them included in quality check
  # dat$pncwmq_test[!(is.na(dat$pncwmq1)) & !(is.na(dat$pncwmq2)) & !(is.na(dat$pncwmq3))] <- 0
  dat$pncwmq[!(is.na(dat$pncwm))] <- 0
  dat$pncwmq[dat$pncwmq1==1 & dat$pncwmq2==1 & dat$pncwmq3==1] <- 1 # women can have a quality check that wasn't within 2days
  
  # dat$pncwmq[!(is.na(dat$pncwm)) & dat$pncwmq1==1 & dat$pncwmq2==1 & dat$pncwmq3==1] <- 1
  table(dat$pncwm, dat$pncwmq, exclude = NULL)
  
  table(dat$pncwmq, exclude = NULL)
  table(dat$pncwmq, dat$pncwmq1, exclude = NULL)
  table(dat$pncwmq, dat$pncwmq2, exclude = NULL)
  table(dat$pncwmq, dat$pncwmq3, exclude = NULL)
  
  table(dat$pncwm,dat$pncwmq, exclude = NULL)
  table(dat$pncwm,dat$pncwmq1, exclude = NULL)
  table(dat$pncwm,dat$pncwmq2, exclude = NULL)
  table(dat$pncwm,dat$pncwmq3, exclude = NULL)
  
 test <- subset(dat, !is.na(pncwmq))
  table(test$pncwmq, test$pncwm, exclude=NULL)
  table(test$m63, test$m62, exclude=NULL) # how many had a check but don't know the timing?
  table(test$m67, test$m66, exclude=NULL) # how many had a check but don't know the timing?
  table(test$m63, test$pncwmq, exclude=NULL)
  
### 7. FP - Proportion of women who have their need for family planning satisfied with modern methods
  
  # Generate demand satisfied among married women- definition 3
  table(dat$v502) # married/in union women =1
  table(dat$v313) # currently using modern methods =3
  table(dat$v626a) # met or unmet need (1,2,3,4)
  
  # Demand satisfied by modern methods - from DHS Github
  # https://github.com/DHSProgram/DHS-Indicators-R/blob/main/Chap07_FP/FP_Need.R
  dat$dps <- 0
  dat$dps[dat$v626a==3 | dat$v626a==4 & dat$v313==3] <- 1
  dat$dps[dat$v626a %!in% c(1,2,3,4)] <- NA # eliminate no need from denominator
  table(dat$v626a, dat$dps, exclude=NULL)  
  table(dat$v626a, dat$dps, exclude=NULL)
  table(dat$v313, dat$dps, exclude=NULL)
  
  table(dat$v502,dat$dps, exclude=NULL)
  table(dat$dps, exclude=NULL)

## 8. Proportion of newborns who received a postnatal check within 2 days of being born
  # m63/m75 - how long before discharging respondent health check took place
  # m67/m71 - how long after discharge/delivery at home respondent health check took place
  # includes all women (home and facility births)
  table(dat$m70, exclude=NULL)
  table(dat$m71, exclude=NULL)
  table(dat$m72, exclude=NULL)
  table(dat$m73, exclude=NULL)
  table(dat$m74, exclude=NULL)
  table(dat$m75, exclude=NULL)
  
  dat$pncn=NA
  
 # if (dhs$country[index]=="Mozambique") {
    dat$pncn[ !(is.na(dat$m75)) | !(is.na(dat$m71))] <- 0
    dat$pncn[(dat$m75 <= 201 & dat$ideliv==1)] <- 1 # facility births checked at facility <2 days after delivery
    dat$pncn[(dat$m71 <= 201 & dat$ideliv==0)] <- 1 # home births checked <2 days after delivery
    dat$pncn[(dat$m71 <= 201 & dat$ideliv==1)] <- 1 # facility births checked at home <2 days after delivery
    
    # Set don't know responses to zero- don't drop from data set
    # dat$pncn[dat$m75 >= 998 & dat$ideliv==1] <- 0 # DK responses (facility births)
    # dat$pncn[(dat$m71 >=998 & dat$ideliv==0)] <- 0 # DK responses (home births)
  # }
  
  #9. PNCN CONTENT - Proportion of newborns with PNC visits who received standard interventions
  # Std interventions: a) examine cord, b) measure temperature, c) signs baby needs med attn, 
  #                    d) counsel on breastfeeding, e) observe breastfeeding
  # examine cord
  dat$pncnq1 <- NA
  dat$pncnq1[dat$m78a==0] <- 0 
  dat$pncnq1[dat$m78a==1] <- 1 
  
  # measure temperature
  dat$pncnq2 <- NA
  dat$pncnq2[dat$m78b==0] <- 0 
  dat$pncnq2[dat$m78b==1] <- 1 
  
  # signs baby needs med attn
  dat$pncnq3 <- NA
  dat$pncnq3[dat$m78c==0] <- 0 
  dat$pncnq3[dat$m78c==1] <- 1 
  
  # counsel on bf
  dat$pncnq4 <- NA
  dat$pncnq4[dat$m78d==0] <- 0 
  dat$pncnq4[dat$m78d==1] <- 1 
  
  # observe bf
  dat$pncnq5 <- NA
  dat$pncnq5[dat$m78e==0] <- 0 
  dat$pncnq5[dat$m78e==1] <- 1 
  
  # dat$pncnq <- 0
  dat$pncnq[!(is.na(dat$pncn))] <- 0
  dat$pncnq[dat$pncnq1==1 & dat$pncnq2==1 & dat$pncnq3==1 & dat$pncnq4==1 & dat$pncnq5==1] <- 1
  table(dat$pncnq, exclude = NULL)
  
  
  ## Covariates ..............................................................
  #1. wealth quintile
  #dat$v190     
  dat$wealth <- dat$v190
  
  #2. residence
  #dat$v025
  dat$urban.rural<-dat$v025
  dat$urban.rural[dat$v025==2]<-0 # rural
  #  table(dat$v025,dat$urban.rural)
  
  #3. age group
  #dat$v012 
  dat$age <- NA 
  dat$age[dat$v012 >=20 & dat$v012 <= 49 ] <- 0
  dat$age[dat$v012 >= 15 & dat$v012 <= 17] <- 1
  dat$age[dat$v012 >=18  & dat$v012 <= 19] <- 2
  #  table(dat$age)
  
  #4. education level
  #dat$v106
  dat$education <- dat$v106
  dat$education[dat$v106==9 | dat$v106==6 ]<-NA # missing
  dat$education[dat$v106==3]<-2 # Higher
  dat$education[dat$v106==2]<-2 # Secondary
  dat$education[dat$v106==1]<-1 # Primary
  dat$education[dat$v106==0]<-0 # No education
  # dat$education <- as.numeric(dat$education)
  
  #5. marital status (married or in union)
  # unique(dat$v501)
  dat$marital.status[dat$v501==0] <- 0 # never married
  dat$marital.status[dat$v501==1 | dat$v501==2] <- 1 # married or living with partner
  dat$marital.status[dat$v501==3 | dat$v501==4 | dat$v501==5] <- 2 # widowed, divorced, or separated
  #table(dat$v501, dat$marital.status)
  
  #6. first time mother
  dat$first.time.mother <- ifelse(dat$v224>=2, 0, 1)
  #table(dat$midx, dat$first.time.mother, exclude=NULL)
  
  #7. region
  dat$region <- dat$v024
  
  
  ############### get individual-level data, all in one place, for inferential analysis
  
  dat$country <- dhs$country[index]
  dat$source <- dhs$source[index]
  dat$recode_phase <- dhs$recode_phase[index]
  
  columns <- c('source','recode_phase','caseid','v001','v005','v023','country','v012','midx','birthage','agemo',indicators,covariates) 
  
  # check
  columns %in% colnames(dat)
  columns[which(!(columns %in% colnames(dat)))]
  
  assign(paste0(dhs$country[index]), as.data.frame(dat[,columns]))
  print(file.name)
  
} # end DHS survey loop

  
  
####################################################################################
# remove Cambodia, Colombia, Ethiopia which do not have the time spent at place of delivery variable, m61
# master <- rbind(Mozambique)
master <- rbind(Angola,Bangladesh,BurkinaFaso,Cambodia,DRC,CotedIvoire,
                Ghana,Jordan,Kenya,Lesotho,Mali,Mozambique,
                Nepal,Nigeria,Philippines,Senegal,Tajikistan,Tanzania,Zambia)

export_fi <- paste0("delivery_indicators.dhs_",date,".csv")

write_csv(master,paste0(export_location,export_fi))

End.time <- Sys.time()
Run.time <- Start.time - End.time

Run.time





