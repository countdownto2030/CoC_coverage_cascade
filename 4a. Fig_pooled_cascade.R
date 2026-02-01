# last edited 31 Jan 2026
# last run 31 Jan 2026
# Objective: draft figure

rm(list=ls())

########################################################### Load Libraries
library(ggplot2)
library(tidyverse)
library(stringr)
# library(cowplot)
# library(gridExtra)

'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date <- "Dec  9"
location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
# setwd(location)

########################################################### GET DATA FILES
data_country <- read.csv(paste0(location,"/results/",date,"_country_cascade.svy.csv"))
head(data_country)
data_pooled <- read.csv(paste0(location,"/results/",date,"_cascade.svy.csv"))
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

# data_sums$use.this[data_sums$indicator=='pop'] <-1
# data_sums$use.this[data_sums$indicator=='casc_anc1'] <-2
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4'] <-3
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq'] <-4
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv'] <-5
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm'] <-6
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq'] <-7
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps'] <-8
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncn'] <-9
# data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncn_pncnq'] <-10

data_sums$value <- data_sums$value*100 

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

data_sums$population <- NA
data_sums$population <- ifelse(data_sums$indicator %in% c("pop","casc_anc1","casc_anc1_anc4","casc_anc1_anc4_ancq","casc_anc1_anc4_ancq_ideliv"),"All",
                               ifelse(data_sums$indicator %in% c("casc_anc1_anc4_ancq_ideliv_pncwm","casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq","casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps"),"Women",
                                         "Newborn"))


plot_data <- data_sums %>%
  mutate(
    indicator_group = case_when(
      indicator %in% c("pop") ~ "population",
      indicator %in% c("casc_anc1") ~ "anc1",
      indicator %in% c("casc_anc1_anc4") ~ "anc4",
      indicator %in% c("casc_anc1_anc4_ancq") ~ "quality anc",
      indicator %in% c("casc_anc1_anc4_ancq_ideliv") ~ "facility delivery",
# shared columns
      indicator %in% c(
        "casc_anc1_anc4_ancq_ideliv_pncwm",
        "casc_anc1_anc4_ancq_ideliv_pncn"
      ) ~ "pnc check 2d",
      
      indicator %in% c(
        "casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq",
        "casc_anc1_anc4_ancq_ideliv_pncn_pncnq"
      ) ~ "quality pnc",
      
      indicator %in% c(
        "casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps"
      ) ~ "demand fam plan satisfied"
    )
  )

plot_data$indicator_group <- factor(
  plot_data$indicator_group,
  levels = c(
    "population",
    "anc1",
    "anc4",
    "quality anc",
    "facility delivery",
    "pnc check 2d",
    "quality pnc",
    "demand fam plan satisfied"
  )
)

plot_data <- plot_data %>%
  mutate(
    fill_group = case_when(
      indicator == c("pop") ~ "pop",
      indicator == c("casc_anc1") ~ "anc1",
      indicator == c("casc_anc1_anc4") ~ "anc4",
      indicator == c("casc_anc1_anc4_ancq") ~ "ancq",
      indicator == c("casc_anc1_anc4_ancq_ideliv") ~ "ideliv",
      indicator == "casc_anc1_anc4_ancq_ideliv_pncwm" ~ "pncwm",
      indicator == "casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq" ~ "pncwmq",
      indicator == "casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps" ~ "dps",
      indicator == "casc_anc1_anc4_ancq_ideliv_pncn" ~ "pncn",
      indicator == "casc_anc1_anc4_ancq_ideliv_pncn_pncnq" ~ "pncnq"
    )
  )

plot_data$fill_group <- factor(
  plot_data$fill_group,
  levels = c("pop","anc1","anc4","ancq","ideliv","pncn","pncnq","pncwm","pncwmq","dps")
)

fill_colors <- c(
  "pop" = "#DADADA",
  "anc1" = "#C1C1C1",
  "anc4" = "#ABABAB",
  "ancq" = "#888888",
  "ideliv" = "#717171",
  
  # Women (blue shades)
  "pncwm"  = "#6BAED6",
  "pncwmq"   = "#2171B5",
  "dps"     = "#08519C",
  
  # Newborn (orange shades)
  "pncn"  = "#FDB863",
  "pncnq"   = "#E08214"
)

legend_labels <- c(
  # "pop" = "Population",
  # "anc1" = "+ANC1",
  # "anc4" = "+ANC4",
  # "ancq" = "Quality ANC",
  # "ideliv" = "Facility delivery",
  "pncwm" = "PNC check 2d (Women)",
  "pncwmq"  = "Quality PNC (Women)",
  "dps"    = "Demand FP satisfied",
  "pncn" = "PNC check 2d (Newborn)",
  "pncnq"  = "Quality PNC (Newborn)"
)


cascade <- ggplot(plot_data, aes(x = indicator_group, y = value)) +
  
  # Grey bars (no legend)
  geom_bar(
    data = subset(plot_data, fill_group %in% c("pop","anc1","anc4","ancq","ideliv")),
    aes(fill = fill_group),
    stat = "identity",
    width = 0.8,
    position = position_dodge2(width = 0.8, preserve = "single"),
    show.legend = FALSE
  ) +
  
  # Colored bars (legend)
  geom_bar(
    data = subset(plot_data, fill_group %in% c("pncn","pncnq","pncwm","pncwmq","dps")),
    aes(fill = fill_group),
    stat = "identity",
    width = 0.8,
    position = position_dodge2(width = 0.8, preserve = "single"),
    show.legend = TRUE
  ) +
  
  # Legend only for colored bars in desired order
  scale_fill_manual(
    values = fill_colors,
    breaks = c("pncn","pncwm","pncnq","pncwmq","dps"),  # legend order
    labels = legend_labels
  ) +
  
  # axes & labels
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_x_discrete(labels = str_wrap(
    c("population", "+anc1", "+anc4", "+quality anc", "+facility delivery",
      "+pnc check 2d", "+quality pnc", "+demand fam plan satisfied"),
    width = 10)) +
  
  labs(x = "", y = "percentage of sample population", fill = "Indicator") +
  theme_bw() +
  theme(
    text = element_text(size = 13),
    axis.text.x = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.width = unit(.5, "cm")
  )
cascade

ggsave(plot=cascade, height = 7, width = 11, dpi = 300, paste0("/Users/EWilson/Desktop/DAC/QoC_continuum/results/Fig2.jpeg"))



