# last edited 1 Feb 2026
# last run 1 Feb 2026

# Objective: draft figure

rm(list=ls())

########################################################### Load Libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(ggplotify)
library(readr)

library(RColorBrewer)

'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date <- "Feb  1"
location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"

########################################################### FIGURE 3 - Equiplots - MMR
svydatastrata <- read_csv(paste0(location,"/results/",date,"_data.strat.svy.csv"))

# data <- svydatastrata %>% filter(covariate!="wealth" | (covariate=="wealth" & level %in% c("1","5")))
# data$level[data$covariate=="wealth" & data$level=="1"] <- "lowest"
# data$level[data$covariate=="wealth" & data$level=="5"] <- "highest"
# 
# head(data)

# scale score to a proportion
svydatastrata$value <- round(100*svydatastrata$value,3)

data_wealth <- svydatastrata %>% 
  filter(covariate == "wealth") %>%
  select(level %in% c("1","5") %>%
  recode(level,
         "lowest" = "1",
         "highest" = "5")

data_wide <- data_wealth %>%
  pivot_wider(
    id_cols    = c(country, covariate, level),
    names_from = indicator,
    values_from = value
  ) %>%
  mutate(
    anc_gap   = anc1  - ancq,
    pncwm_gap = pncwm - pncwmq,
    pncn_gap  = pncn  - pncnq
  )











##########################
getPalette = colorRampPalette(brewer.pal(11, "RdYlGn"))

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


## Need plots for each indicator x stratifier = 6 x 3
## Get the stratified differences

data_wealth <- subset(data, covariate=="wealth")
data_wide <- data_wealth %>%
  pivot_wider(
    id_cols   = c(country, covariate, level),
    names_from = indicator,
    values_from = value) %>%
  mutate(
    anc1_ancq = anc1 - ancq,
    pncwm_pncwmq = pncwm - pncwmq,
    pncn_pncnq = pncn - pncnq
  )

plot_data <- data_wide %>%
  select(country, level, pncwm, pncwmq) %>%
  tidyr::pivot_longer(
    cols = c(pncwm, pncwmq),
    names_to = "measure",
    values_to = "value"
  )

plot <- ggplot(plot_data,
       aes(x = value, y = country)) +
  
  # connect coverage → quality
  geom_line(
    aes(group = country),
    color = "grey60",
    linewidth = 0.8
  ) +
  
  # the two points
  geom_point(
    aes(color = measure),
    size = 3
  ) +
  
  facet_wrap(~ level, ncol = 2) +
  
  scale_color_manual(
    values = c("pncwm" = "darkblue",
               "pncwmq" = "gold"),
    labels = c("Contact", "Content")
  ) +
  
  labs(
    x = "% PNC (contact vs content)",
    y = ""
  ) +
  
  coord_cartesian(xlim = c(0, 100)) +
  
  theme_bw() +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 11),
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )
