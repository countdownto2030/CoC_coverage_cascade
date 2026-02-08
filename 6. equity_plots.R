# last edited 8 Feb 2026
# last run 8 Feb 2026

# Objective: draft figure

rm(list=ls())

########################################################### Load Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(stringr)
library(ggplotify)
library(readr)

library(RColorBrewer)

library(patchwork)

'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date <- "Feb  1"
location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"

########################################################### FIGURE 3 - Equiplots - MMR
svydatastrata <- read_csv(paste0(location,"/results/",date,"_data.strat.svy.csv"))

# scale score to a proportion
svydatastrata$value <- round(100*svydatastrata$value,3)

data_wealth <- svydatastrata %>% 
  filter(
    covariate == "wealth",
    level %in% c("1", "5")
  ) %>%
  mutate(
    level = recode(
      level,
      "1" = "lowest",
      "5" = "highest"
    ))

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

data_wide <- data_wide %>%
  mutate(
    level = factor(level, levels = c("lowest", "highest")),  # make sure this is correct
    country = factor(country, levels = sort(unique(country)))
  )

paired_plot <- function(df, contact, content, title,
                        show_legend = FALSE,
                        show_y = TRUE) {
  
  contact_nm <- rlang::as_name(enquo(contact))
  content_nm <- rlang::as_name(enquo(content))
  
  plot_data <- df %>%
    select(country, level, {{ contact }}, {{ content }}) %>%
    mutate(
      gap = {{ contact }} - {{ content }},
      mid = ({{ contact }} + {{ content }}) / 2
    ) %>%
    pivot_longer(
      cols = c({{ contact }}, {{ content }}),
      names_to = "measure",
      values_to = "value"
    ) %>%
    mutate(
      measure = recode(
        measure,
        !!contact_nm := "Contact",
        !!content_nm := "Content"
      ),
      measure = factor(measure, levels = c("Content", "Contact"))
    )
  
  gap_labels <- plot_data %>%
    distinct(country, level, gap, mid)
  
  ggplot(plot_data, aes(x = value, y = country)) +
    
    geom_line(aes(group = country), color = "grey65", linewidth = 0.8) +
    geom_point(aes(color = measure), size = 3) +
    
    facet_wrap(~level, ncol = 2, scales = "free_x",
               labeller = labeller(level = c("lowest" = "Lowest", "highest" = "Highest"))) +
    
    geom_text(data = gap_labels, aes(x = mid, y = country, label = sprintf("%.1f", gap)),
              color = "grey30", size = 6, vjust = -0.2) +
    
    scale_color_manual(
      values = c("Contact" = "darkblue", "Content" = "gold"),
      guide = if(show_legend) guide_legend(title = NULL) else "none"
    ) +
    
    labs(x = NULL,
         y = if(show_y) "" else NULL,
         title = title) +
    
    coord_cartesian(xlim = c(0, 100)) +
    
    theme_bw() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      strip.text = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(size = 18),
      axis.text.y = if(show_y) element_text(size = 18) else element_blank(),
      axis.ticks.y = if(show_y) element_line() else element_blank(),
      legend.position = "bottom", # in the theme, for individual plot
      legend.justification = "center",
      legend.text = element_text(size = 14),
      legend.key.size = unit(1.2, "cm"),
      panel.grid.minor = element_blank()
    )
}





plot_anc <- paired_plot(
  data_wide,
  anc1, ancq,
  "% ANC (contact vs content)",
  show_legend = TRUE,   # <- turn on legend in one plot
  show_y = TRUE
)

plot_pncwm <- paired_plot(
  data_wide,
  pncwm, pncwmq,
  "% PNCwm (contact vs content)",
  show_legend = FALSE,
  show_y = FALSE
)

plot_pncn <- paired_plot(
  data_wide,
  pncn, pncnq,
  "% PNCn (contact vs content)",
  show_legend = FALSE,
  show_y = FALSE
)

combined_plot <- plot_anc + plot_pncwm + plot_pncn 

ggsave(plot = combined_plot, height = 12, width = 18, dpi = 300,
       filename = "/Users/EWilson/Desktop/DAC/QoC_continuum/results/Fig5.jpeg")


