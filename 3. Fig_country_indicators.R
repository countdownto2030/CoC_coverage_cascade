# last edited 8 Feb 2026
# last run 8 Feb 2026
# Objective: draft figure

rm(list=ls())

########################################################### Load Libraries
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gridGraphics)
library(gridExtra)
library(reshape2)
library(pals)
'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date = "Feb  1"

location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
setwd(location)

########################################################### GET DATA FILES - survey-weighted estimates
svydata <- read_csv(paste0(location,"/results/",date,"_country_indicators.svy.csv"))
svydata_pooled <- read_csv(paste0(location,"/Results/",date,"_pooled_mean.svy.csv"))
names(svydata_pooled)[names(svydata_pooled)=="level"] <- "country"
svydata <- rbind(svydata,svydata_pooled)

head(svydata)
sort(unique(svydata$country))
n <- table(svydata$country, svydata$indicator)
dim(n)

########################################################### FIGURE 1 - multi-country with median value bars
data <- svydata 
unique(data$indicator)

data$use.this[data$indicator=='anc1'] <-1
data$use.this[data$indicator=='anc4'] <-2
data$use.this[data$indicator=='ancq'] <-3
data$use.this[data$indicator=='ideliv'] <-4
data$use.this[data$indicator=='pncwm'] <-5
data$use.this[data$indicator=='pncwmq'] <-6
data$use.this[data$indicator=='dps'] <-7
data$use.this[data$indicator=='pncn'] <-8
data$use.this[data$indicator=='pncnq'] <-9

data$indicator <- factor(data$indicator, levels = unique(data$indicator[order(data$use.this)]))

df1 <- data %>% group_by(indicator) %>% mutate(med = median(value[country != "pooled standard"], na.rm = TRUE))
df2 <- df1 %>% group_by(indicator) %>%  summarise(n=n())
df1$value <- ifelse(df1$indicator == "score", df1$value*20, df1$value*100)
df1$med <- ifelse(df1$indicator == "score", df1$med*20, df1$med*100)

# make separate plots and then grid arrange
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# my.title <- c("Contact and content indicators\n")
mycolors = c("cornflowerblue","red2","forestgreen","darkblue","tan4","magenta","darkgrey","firebrick4","purple3")

df1_1 <- df1[df1$indicator %in% c("anc1","anc4","ancq","ideliv","pncwm","pncwmq","dps","pncn","pncnq") & df1$country!="pooled standard",]
df1_2 <- subset(df1, country == "pooled standard")
head(df1_2)

graph <- ggplot() +
  geom_point(data=df1_1,aes(x=indicator,y=value),position=position_jitter(width=0.1, height=0.1)) +
  geom_bar(data=df1_2,aes(x=indicator,y=value,fill=indicator),
           position="dodge", stat="identity", alpha=0.65) +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("intervention coverage") +
  # ggtitle(my.title) +
  labs(col="") +
  theme_bw() +
  theme( # axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
          text = element_text(size=12),     
          plot.tag.position = c(0.17,.99),
          axis.title.y=element_text(size=20),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=16, vjust=.5),
          plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_x_discrete(labels=str_wrap(c("anc 1","anc 4","quality anc","facility delivery",
                                     "pnc check 2d wm","quality pnc wm","demand fam plan satisfied",
                                     "pnc check 2d newborn","quality pnc newborn"),width=8)) +
  scale_fill_manual(values=mycolors) +
  scale_colour_manual(values=mycolors) +
  theme(legend.position = "none") 
graph 


ggsave(plot=graph, height = 7 , width = 14, dpi = 300, "/Users/EWilson/Desktop/DAC/QoC_continuum/results/Fig1.jpeg")





