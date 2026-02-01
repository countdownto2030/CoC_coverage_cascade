# last edited 29 July 2025
# last run 29 July 2025
# Objective: draft figure

rm(list=ls())

########################################################### Load Libraries
library(ggplot2)
library(survey) #allows for design based analysis
library(RColorBrewer)
library(forcats)
library(tidyverse)
library(data.table)

library(stringr)
library(cowplot)
library(gridGraphics)
library(gridExtra)

'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date <- "May  19"
location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
# setwd(location)


########################################################### GET DATA FILES
data_country <- read.csv(paste0(location,"/results/Jul 29_country_cascade.svy.csv"))
head(data_country)
data_pooled <- read.csv(paste0(location,"/results/Jul 29_cascade.svy.csv"))
head(data_pooled)

############################################################ POOLED
# CASCADE A
add_row <- data.frame(indicator=c("pop"),
                      n=unique(data_pooled$n),
                      value=1.0,
                      se= NA,
                      CIL = NA,
                      CIU = NA)

data_sums <- rbind(add_row,data_pooled)

data_sums$use.this[data_sums$indicator=='pop'] <-1
data_sums$use.this[data_sums$indicator=='casc_anc1'] <-2
data_sums$use.this[data_sums$indicator=='casc_anc1'] <-3
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4'] <-4
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq'] <-5
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv'] <-6
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm'] <-7
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq'] <-8
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps'] <-9
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncn'] <-10
data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncn_pncnq'] <-11


data_sums$indicator <- factor(data_sums$indicator, levels = unique(data_sums$indicator[order(data_sums$use.this)]))
data_sums$value <- data_sums$value*100 

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# WOMEN
data_wm <- subset(data_sums, !use.this %in% c(10, 11))

# mycolors = c("gray","plum1","purple2","lightgreen","darkgreen")
mycolors = c("#DADADA","#C1C1C1","#ABABAB","#888888","#717171","#bdd7e7","#6baed6","#2171b5")
mylabels = c("population","+anc1","+anc4","+ancq","+facility delivery","+women health check within 2d","+women quality check","+demand FP satisfied, modern")

cascadeA <- ggplot(data_wm, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("percentage of sample population") +
  ggtitle(wrapper("Women", width=30)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.tag.position = c(0.20,.99),
        plot.title = element_text(hjust = 0.20, size = 13)) +
  scale_x_discrete(labels=str_wrap(c("population","+anc1","+anc4","+quality anc","+facility delivery",
                                     "+pnc check 2d wm","+quality pnc wm","+demand fam plan satisfied"),width=8)) +
  scale_fill_manual(values=mycolors) +
  theme(legend.position = "none")
cascadeA



# NEWBORNS
data_nb <- subset(data_sums, !use.this %in% c(7,8,9))

# mycolors = c("#EBEBEB","#DADADA","#C1C1C1","#ABABAB","#888888","#717171","#525252","#000000")
mycolors = c("#DADADA","#C1C1C1","#ABABAB","#888888","#717171","orange","darkorange3")
mylabels = c("population","+anc1","+anc4","+ancq","+facility delivery","+neborn health check within 2d","+newborn quality check")

cascadeB <- ggplot(data_nb, aes(x = indicator, y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  # geom_text(aes(label=round(value,0)), position=position_dodge(width=2.9), vjust=-0.25) +
  # labs(tag = "(A)") +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab("") +
  ggtitle(wrapper("Newborns", width=30)) +
  labs(col="") +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.tag.position = c(0.20,.99),
        # axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of y ticks/text
        # axis.title.y=element_blank(), # remove x axis label
        # axis.text.x = element_text(size=9, vjust=.5),
        # axis.ticks.x = element_blank(),
        # axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.20, size = 13))+
  # plot.title = element_text(hjust = 0.20, size = 13) 
  scale_x_discrete(labels=str_wrap(c("population","+anc1","+anc4","+quality anc","+facility delivery",
                                     "+pnc check 2d newborn","+quality pnc newborn"),width=8)) +
  scale_fill_manual(values=mycolors) +
  theme(legend.position = "none")
cascadeB

cascade <- plot_grid(cascadeA,cascadeB,ncol=2, align='h',rel_widths = c(1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=cascade, height = 7, width = 13, dpi = 300, paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/Continuum_Cascade.jpeg"))




