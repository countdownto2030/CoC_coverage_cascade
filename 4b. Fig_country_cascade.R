# last edited 8 Feb 2026
# last run 8 Feb 2026
# Objective: draft figure

rm(list=ls())

########################################################### Load Libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(ggplotify)

'%!in%' <- function(x,y)!('%in%'(x,y))

# date = substr(date(),5,10)
date <- "Feb  1"
location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
########################################################### GET DATA FILES
data_country <- read.csv(paste0(location,"/results/",date,"_country_cascade.svy.csv"))
head(data_country)

# --- Colors & legend setup ---
fill_colors <- c(
  "pop"      = "#DADADA",
  "anc1"     = "#C1C1C1",
  "anc4"     = "#ABABAB",
  "ancq"     = "#888888",
  "ideliv"   = "#717171",
  "pncn"     = "orange",
  "pncwm"    = "#bdd7e7",
  "pncnq"    = "darkorange3",
  "pncwmq"   = "#6BAED6",
  "dps"      = "#2171B5"
)

legend_order <- c(
  "anc1",
  "anc4",
  "ancq",
  "ideliv",
  "pncwm",
  "pncwmq",
  "dps",
  "pncn",
  "pncnq"
)

legend_labels <- c(
  "anc1"   = "+ANC1",
  "anc4"   = "+ANC4",
  "ancq"   = "Quality ANC",
  "ideliv" = "Facility delivery",
  "pncn"   = "PNC check 2d (Newborn)",
  "pncwm"  = "PNC check 2d (Women)",
  "pncnq"  = "Quality PNC (Newborn)",
  "pncwmq" = "Quality PNC (Women)",
  "dps"    = "Demand FP satisfied"
)

plot_titles <- sort(unique(data_country$country))
cascades <- list()

for(k in seq_along(plot_titles)){
  
  country_data <- subset(data_country, country == plot_titles[k])
  
  # Add population row
  add_row <- data.frame(
    country = unique(country_data$country),
    indicator = "pop",
    n = unique(country_data$n),
    value = 1,
    se = NA,
    CIL = NA,
    CIU = NA
  )
  
  data_sums <- rbind(add_row, country_data)
  
  # Indicator order
  data_sums$use.this <- NA
  data_sums$use.this[data_sums$indicator=='pop'] <- 1
  data_sums$use.this[data_sums$indicator=='casc_anc1'] <- 2
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4'] <- 3
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq'] <- 4
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv'] <- 5
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm'] <- 6
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq'] <- 7
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncwm_pncwmq_dps'] <- 8
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncn'] <- 9
  data_sums$use.this[data_sums$indicator=='casc_anc1_anc4_ancq_ideliv_pncn_pncnq'] <- 10
  
  # Factor levels for plotting order
  data_sums$indicator <- factor(
    data_sums$indicator,
    levels = data_sums$indicator[order(data_sums$use.this)]
  )
  
  data_sums$value <- data_sums$value * 100
  
  data_sums$indicator_plot <- sub(".*_", "", data_sums$indicator)
  data_sums$indicator_plot <- factor(
    data_sums$indicator_plot,
    levels = c("pop", legend_order)
  )
  
  # -------------------------------------------------------
  # <<< NEW: explicit numeric x positions with a small gap
  # -------------------------------------------------------
  data_sums$x_pos <- NA_real_
  
  data_sums$x_pos[data_sums$indicator_plot == "anc1"]   <- 1
  data_sums$x_pos[data_sums$indicator_plot == "anc4"]   <- 2
  data_sums$x_pos[data_sums$indicator_plot == "ancq"]   <- 3
  data_sums$x_pos[data_sums$indicator_plot == "ideliv"] <- 4
  data_sums$x_pos[data_sums$indicator_plot == "pncwm"]  <- 5
  data_sums$x_pos[data_sums$indicator_plot == "pncwmq"] <- 6
  data_sums$x_pos[data_sums$indicator_plot == "dps"]    <- 7
  
  # --- visual gap ---
  data_sums$x_pos[data_sums$indicator_plot == "pncn"]   <- 8.4
  data_sums$x_pos[data_sums$indicator_plot == "pncnq"]  <- 9.2
  # -------------------------------------------------------
  
  # Remove population bar except first column in each row
  if(!k %in% c(1,6,11,16)) data_sums <- data_sums[data_sums$indicator != "pop",]
  
  # Y-axis settings
  y_axis_text <- element_blank()
  y_axis_ticks <- element_blank()
  if(k %in% c(1,6,11,16)) {
    y_axis_text <- element_text(size=12)
    y_axis_ticks <- element_line()
  }
  
  # --- Create plot ---
  cascade <- 
    ggplot(data_sums, aes(x = x_pos, y = value, fill = indicator_plot)) +  # <<< CHANGED x
    geom_bar(stat = "identity", width = 0.8, na.rm = TRUE) +               # <<< CHANGED
    geom_text(
      aes(label = ifelse(indicator == "pop", "", round(value,0))),
      vjust = -0.05,
      size=5,
      na.rm = TRUE
    ) +
    scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.02, 0.05))) +  # <<< NEW
    scale_fill_manual(
      values = fill_colors,
      breaks = legend_order,
      labels = legend_labels
    ) +
    ggtitle(plot_titles[k]) +
    xlab("") +
    ylab("") +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size=16),
      axis.ticks.y = y_axis_ticks,
      plot.title = element_text(hjust = 0.5, size=16),
      axis.title.y = element_text(size=16),
      legend.title = element_blank(),
      legend.text = element_text(size=12),
      legend.position = "none"
    )
  
  cascades[[k]] <- cascade
}

# --- Extract legend from last plot ---
legend_plot <- get_legend(
  ggplot(data_sums, aes(x=indicator, y=value, fill=indicator_plot)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = fill_colors,
                      breaks = legend_order,
                      labels = legend_labels) +
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size=15))
)

# --- Build grid ---
rows <- list()
for(i in seq(1, 20, by = 5)){
  row_plots <- list()
  for(j in 0:4){
    idx <- i + j
    if(idx > length(cascades)) break
    row_plots[[j+1]] <- cascades[[idx]]
  }
  # Last row: put legend in last slot
  if(i == 16) row_plots[[5]] <- legend_plot
  rows[[ceiling(i/5)]] <- plot_grid(plotlist = row_plots, nrow=1, rel_widths = c(1.1, 1, 1, 1, 1)
)
}

cascade_facet <- plot_grid(plotlist = rows, nrow=4)

# --- Add final y-axis label ---
final_plot <- ggdraw(cascade_facet) +
  draw_label("Percentage of sample population",
             x = 0.02, y = 0.5,
             angle = 90,
             vjust = 0,
             hjust = 0.5,
             size = 20)

# --- Save ---
ggsave(plot = final_plot, height = 12, width = 15, dpi = 300,
       filename = "/Users/EWilson/Desktop/DAC/QoC_continuum/results/Fig3.jpeg")
