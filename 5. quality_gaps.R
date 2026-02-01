# last edited 1 Feb 2026
# last run 1 Feb 2026

# Objective: get data files for survey-weighted and stratified results

rm(list=ls())
Start.time <- Sys.time()
########################################################### Load Libraries
library(tidyverse)
library(dplyr)
library(colorspace)
library(patchwork)

'%!in%' <- function(x,y)!('%in%'(x,y))

date = substr(date(),5,10)
dhs_date = "Feb  1"


location <- "/Users/EWilson/Desktop/DAC/QoC_continuum"
setwd(location)

########################################################### GET DATA FILES
countrydata <- read.csv(paste0(location,"/results/",dhs_date,"_country_indicators.svy.csv"),colClasses=c("v023"="character"))
head(countrydata)

pooleddata <- read.csv(paste0(location,"/results/",dhs_date,"_pooled_mean.svy.csv"),colClasses=c("v023"="character"))

########################################################### RESHAPE WIDE TO CALCULATE CONTACT-CONTENT DIFFERENCES
head(countrydata)
countrydata_wide <- countrydata %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(n, value, se, CIL, CIU),
    names_glue = "{indicator}_{.value}"
  )

dim(countrydata_wide)
head(countrydata_wide)

head(pooleddata)
pooleddata_wide <- pooleddata %>%
  pivot_wider(
    names_from = indicator,
    values_from = c(n, value, se, CIL, CIU),
    names_glue = "{indicator}_{.value}"
  )

dim(pooleddata_wide)
head(pooleddata_wide)

########################################################### CONTACT - CONTENT GAPS - COUNTRY
countrydata_wide$gap_pncnq_value  <- countrydata_wide$pncn_value - countrydata_wide$pncnq_value
countrydata_wide$gap_pncwmq_value <- countrydata_wide$pncwm_value - countrydata_wide$pncwmq_value
countrydata_wide$gap_ancq_value <- countrydata_wide$anc1_value - countrydata_wide$ancq_value

# View(countrydata_wide[,c("country","gap_ancq_value","anc1_value","ancq_value",
                         # "gap_pncnq_value","pncn_value","pncnq_value",
                         # "gap_pncwmq_value","pncwm_value","pncwmq_value")])

########################################################### CONTACT - CONTENT GAPS - POOOLED
pooleddata_wide$gap_pncnq_value  <- pooleddata_wide$pncn_value - pooleddata_wide$pncnq_value
pooleddata_wide$gap_pncwmq_value <- pooleddata_wide$pncwm_value - pooleddata_wide$pncwmq_value
pooleddata_wide$gap_ancq_value <- pooleddata_wide$anc1_value - pooleddata_wide$ancq_value

# View(pooleddata_wide[,c("level","gap_ancq_value","anc1_value","ancq_value",
                         # "gap_pncnq_value","pncn_value","pncnq_value",
                         # "gap_pncwmq_value","pncwm_value","pncwmq_value")])

# USE countrydata_pooled
# calculate the PNC contact-content gap for all newborns
0.8636452-0.3305985
# calculate the PNC contact-content gap for all women
0.8738545-0.3271854
# calculate the ANC contact-content gap for all women
0.9164883-0.7298380





########################################################### PLOTS
countrydata1 <- countrydata %>% filter(indicator %in% c("anc1","pncwm","pncn")) %>% 
                                select(country,indicator,value) %>% 
                                rename(crude = indicator,
                                       value_crude = value)

countrydata2 <- countrydata %>% filter(indicator %in% c("ancq","pncwmq","pncnq")) %>% 
  select(country,indicator,value) %>% 
  rename(quality = indicator,
         value_q = value) %>%
  mutate(crude = recode(quality,
                        "ancq"   = "anc1",
                        "pncwmq" = "pncwm",
                        "pncnq"  = "pncn"))

plot_data <- merge(countrydata1, countrydata2, by=c("country","crude"))
head(plot_data)
# plot_data$value_crude <- 100*round(plot_data$value_crude)
# plot_data$value_q <- 100*round(plot_data$value_q)
plot_data$gap <- 100*round(plot_data$value_crude - plot_data$value_q,3)
plot_data <- plot_data %>%
  mutate(
    value_crude = round(value_crude * 100,3),
    value_q     = round(value_q * 100,3),
  )

# Base colors for crude
base_crude <- c("#66c2a5", "#fc8d62", "#8da0cb")

crude_colors   <- base_crude
quality_colors <- darken(base_crude, amount = 0.4) 


country_order <- plot_data %>% 
  filter(crude == "anc1") %>%
  arrange(desc(gap)) %>%
  pull(country)

# Function to plot one indicator
# Function to plot one indicator with custom legend labels
dumbbell_plot <- function(data, indicator_name, color_index,
                          crude_label = "Crude", quality_label = "Quality",
                          show_y_axis = TRUE) {
  
  df_plot <- data %>% 
    filter(crude == indicator_name) %>%
    mutate(country = fct_reorder(country, gap, .desc = TRUE))
  
  ggplot(df_plot, aes(y = country)) +
    
    # connecting line
    geom_segment(
      aes(
        x = value_crude,
        xend = value_q,
        yend = country
      ),
      color = "grey70",
      linewidth = 1
    ) +
    
    # points with custom legend labels
    geom_point(aes(x = value_crude, color = crude_label), size = 5) +
    geom_point(aes(x = value_q, color = quality_label), size = 5) +
    
    # gap label
    geom_text(
      aes(x = (value_crude + value_q)/2, label = gap),
      size = 5, vjust = -0.5
    ) +
    
    # custom colors
    scale_color_manual(
      name = NULL,
      values = setNames(
        c(quality_colors[color_index], crude_colors[color_index]),
        c(quality_label, crude_label)
        # c(crude_colors[color_index], quality_colors[color_index]),
        # c(crude_label, quality_label)
      ),
      breaks = c(quality_label,crude_label)  # <--- forces left-to-right order
    ) +
    
    # custom x-axis breaks
    scale_x_continuous(
      # breaks = c(0.2, 0.4, 0.6, 0.8, 1.0),
      breaks = c(20, 40, 60, 80, 100),
      labels = scales::number_format(accuracy = 1)
    ) +
    
    # Remove axis labels completely
    labs(x = NULL, y = NULL) +
    
    theme_minimal(base_size = 15) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      axis.text.y = if(show_y_axis) element_text(size = 15) else element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    )
}



p1 <- dumbbell_plot(plot_data, "anc1", 1, crude_label = "ANC1", quality_label = "ANC (Quality)", show_y_axis = TRUE)
p2 <- dumbbell_plot(plot_data, "pncn", 2, crude_label = "PNC-N", quality_label = "PNC-N (Quality)", show_y_axis = TRUE)
p3 <- dumbbell_plot(plot_data, "pncwm", 3, crude_label = "PNC-WM", quality_label = "PNC-WM (Quality)", show_y_axis = TRUE)

combined_plot <- p1 | p2 | p3 + 
  plot_annotation(
    title    = "Crude vs Quality Indicators by Country",
    subtitle = "Quality is darker shade of the same color family"
  )

combined_plot

# Save
ggsave(plot = combined_plot, height = 12, width = 18, dpi = 300,
       filename = "/Users/EWilson/Desktop/DAC/QoC_continuum/results/Fig4.jpeg")
