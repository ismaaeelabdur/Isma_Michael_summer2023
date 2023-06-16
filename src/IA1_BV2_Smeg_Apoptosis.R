# A script that plots the frequency and counts of AMs over time

library(tidyverse)
library(ggpubr)
library(readxl)

Freq_plot <- function(data, y_freq){
(p_freq <- data %>% 
    ggplot() +
    aes(x = fct_relevel(genotype, 'comp', 'KO'), y = get(y_freq)) +
    geom_point(aes(color = genotype)) +
    stat_compare_means(method = "t.test", label = 'p.format') +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap(~hpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('Hours post infection') +
    ylab(as.character(y_freq)) +
    ggtitle(as.character(y_freq)) +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          panel.background = element_rect(fill = 'white'),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          panel.grid.major = 
            element_line(color = 'grey', size = 0.1),
          strip.background = element_blank(),
          strip.placement = 'outside',
          legend.position = 'none',
          strip.text.x = element_text(angle = 45, size = 16),
          axis.line = element_line()))
}

directory <- 'IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/Spreadsheets/'
files <- c('IA:MN1_18hpi_noD2.xlsx', 'IA1_1hpi.xlsx', 'Naive_18hpi.xlsx')
colnames <- c('conditon', "BV2", "Infected", "Apop_Infected", "Apop_BV2",
              "GFP_MFI", "Apop_Infected_Median", "Apop_BV2_Median", "genotype", "hpi")
              
freq <- tibble()
for (file in files){
  data <- read_excel(path = paste(directory, file, sep=""), na = 'N/A', col_names = TRUE)
  colnames(data) <- colnames
  freq <- rbind(freq, data)
}

Freq_plot(filter(freq, hpi != 0), "Infected")
ggsave(filename = "IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/A1_PercentInfected.png")
Freq_plot(filter(freq, hpi != 0), "GFP_MFI")
ggsave(filename = "IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/IA1_GFP_MFI.png")
Freq_plot(filter(freq, hpi != 0), "Apop_Infected_Median")
ggsave(filename = "IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/IA1_MedianApoptosingofInfected.png")
Freq_plot(freq, "Apop_BV2_Median")
ggsave(filename = "IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/IA1_MedianApoptosingofBV2.png")
Freq_plot(freq, "Apop_BV2")
ggsave(filename = "IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/IA1_PercentApoptosingofBV2.png")
Freq_plot(filter(freq, hpi != 0), "Apop_Infected")
ggsave(filename = "IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/IA1_PercentApoptosingofInfected.png")

