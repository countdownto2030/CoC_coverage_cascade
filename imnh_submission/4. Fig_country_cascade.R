# last edited 30 July 2025
# last run 30 July 2025
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
########################################################### GET DATA FILES
data_country <- read.csv(paste0(location,"/results/Jul 29_country_cascade.svy.csv"))
head(data_country)

############################################################ COUNTRY CASCADES
cascades = list()

plot_titles <- sort(unique(data_country$country))


wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}

# WOMEN
for(k in 1:length(sort(unique(data_country$country)))){
  country_casc_data <- subset(data_country, country %in% sort(unique(data_country$country))[k])
  print(unique(country_casc_data$country))
  

add_row <- data.frame(country = unique(country_casc_data$country),
                      indicator=c("pop"),
                      n=unique(country_casc_data$n),
                      value=1.0,
                      se= NA,
                      CIL = NA,
                      CIU = NA)

data_sums <- rbind(add_row,country_casc_data)

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


# WOMEN
data_wm <- subset(data_sums, !use.this %in% c(10, 11))

if(!k %in% c(1,8)){
  data_wm <- subset(data_wm, indicator != "pop") 
  mycolors = c("#C1C1C1","#ABABAB","#888888","#717171","#bdd7e7","#6baed6","#2171b5")
  mylabels = c("+anc1","+anc4","+ancq","+facility delivery","+women health check within 2d","+women quality check","+demand FP satisfied, modern")
  ytitle = c("")
}
if(k %in% c(1,8)){
  mycolors = c("#DADADA","#C1C1C1","#ABABAB","#888888","#717171","#bdd7e7","#6baed6","#2171b5")
  mylabels = c("population","+anc1","+anc4","+ancq","+facility delivery","+women health check within 2d","+women quality check","+demand FP satisfied, modern")
  ytitle = c("percentage of sample population")}

cascade <-  ggplot(data_wm, aes(x = reorder(indicator, -value),y= value, fill=indicator)) +
  geom_bar(position="dodge", stat = "identity") +
  # geom_text(aes(label=round(value,0)), position=position_dodge(width=0.9), vjust=-0.25) +
  geom_text(aes(label = ifelse(value == 100, "", paste0(round(value,0), "%"))), position=position_dodge(width=0.9), vjust = -0.25) +
  scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
  xlab("") +
  ylab(ytitle) +
  ggtitle(plot_titles[k]) +
  labs(col="") +
  scale_fill_manual(values=mycolors) +
  scale_fill_manual(values=mycolors, labels=str_wrap(c("population","+anc1","+anc4","+quality anc","+facility delivery",
                                      "+pnc check 2d wm","+quality pnc wm","+demand fam plan satisfied"),width=20)) +
  # scale_x_discrete(labels=str_wrap(c("population","+anc1","+anc4","+quality anc","+facility delivery",
  #                                      "+pnc check 2d wm","+quality pnc wm","+demand fam plan satisfied"),width=8)) +
  scale_colour_manual(values=mycolors) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.y=element_text(size = 16)) +
  if(!k %in% c(1,8)){theme(axis.text.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 legend.position = "none")}

cascades[[k]] <- cascade

}


cascades[[1]]
cascades[[2]]
cascades[[8]]

legend_cascade <- get_legend(cascades[[1]])

# and replot suppressing the legend
cascade_1 <- cascades[[1]] + theme(legend.position='none')
cascade_8 <- cascades[[8]] + theme(legend.position='none')

cascade_facet <- plot_grid(cascade_1,cascades[[2]],cascades[[3]],cascades[[4]],cascades[[5]],cascades[[6]],
                           cascades[[7]],cascade_8,cascades[[9]],cascades[[10]],cascades[[11]],cascades[[12]],cascades[[13]],
                           legend_cascade,nrow=2,rel_widths = c(1.2,1,1,1,1,1,1,1,1,1,1,1,1,.8)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=cascade_facet, height = 7, width = 15,  dpi = 300, paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/Country_Cascade_wm.jpeg"))





# NEWBORNS
for(k in 1:length(sort(unique(data_country$country)))){
  country_casc_data <- subset(data_country, country %in% sort(unique(data_country$country))[k])
  print(unique(country_casc_data$country))

  add_row <- data.frame(country = unique(country_casc_data$country),
                        indicator=c("pop"),
                        n=unique(country_casc_data$n),
                        value=1.0,
                        se= NA,
                        CIL = NA,
                        CIU = NA)

  data_sums <- rbind(add_row,country_casc_data)

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

  # NEWBORN
  data_nb <- subset(data_sums, !use.this %in% c(7,8,9))

  if(!k %in% c(1,8)){
    data_nb <- subset(data_nb, indicator != "pop")
    mycolors = c("#C1C1C1","#ABABAB","#888888","#717171","orange","darkorange3")
    mylabels = c("+anc1","+anc4","+ancq","+facility delivery","+neborn health check within 2d","+newborn quality check")
    ytitle = c("")
  }
  if(k %in% c(1,8)){
    mycolors = c("#DADADA","#C1C1C1","#ABABAB","#888888","#717171","orange","darkorange3")
    mylabels = c("population","+anc1","+anc4","+ancq","+facility delivery","+newborn health check within 2d","+newborn quality check")
    ytitle = c("percentage of sample population")
    }
  
  cascade <-  ggplot(data_nb, aes(x = reorder(indicator, -value),y= value, fill=indicator)) +
    geom_bar(position="dodge", stat = "identity") +
    # geom_text(aes(label=round(value,0)), position=position_dodge(width=0.9), vjust=-0.25) +
    geom_text(aes(label = ifelse(value == 100, "", paste0(round(value,0), "%"))), position=position_dodge(width=0.9), vjust = -0.25) +
    scale_y_continuous(limits=c(0,100),breaks=c(0,20,40,60,80,100)) +
    xlab("") +
    ylab(ytitle) +
    ggtitle(plot_titles[k]) +
    labs(col="") +
    # scale_fill_manual(values=mycolors) +
    scale_fill_manual(values=mycolors, labels=str_wrap(c("population","+anc1","+anc4","+ancq","+facility delivery",
                                                         "+newborn health check within 2d","+newborn quality check"),width=20)) +
    # scale_colour_manual(values=mycolors) +
    theme_bw() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y = element_text(size=12),
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.y=element_text(size = 16)) +
    if(!k %in% c(1,8)){theme(axis.text.y=element_blank(),
                             axis.ticks.y=element_blank(),
                             legend.position = "none")}
  
  cascades[[k]] <- cascade
  
}


cascades[[1]]
cascades[[2]]
cascades[[8]]
cascades[[9]]
legend_cascade <- get_legend(cascades[[1]])

# and replot suppressing the legend
cascade_1 <- cascades[[1]] + theme(legend.position='none')
cascade_8 <- cascades[[8]] + theme(legend.position='none')

cascade_facet <- plot_grid(cascade_1,cascades[[2]],cascades[[3]],cascades[[4]],cascades[[5]],cascades[[6]],
                           cascades[[7]],cascade_8,cascades[[9]],cascades[[10]],cascades[[11]],cascades[[12]],cascades[[13]],
                           legend_cascade,nrow=2,rel_widths = c(1.2,1,1,1,1,1,1,1,1,1,1,1,1,.8)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(plot=cascade_facet, height = 7, width = 15,  dpi = 300, paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/Country_Cascade_nb.jpeg"))






