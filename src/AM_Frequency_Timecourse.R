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
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45)))
}


directory <- ''
files <- c('Spreadsheets/IA:MN1_18hpi_noD2.xlsx', 'Spreadsheets/IA1_1hpi.xlsx', 'Spreadsheets/Naive_18hpi.xlsx')
#colnames <- c('name', "AM_freq", "Eos_freq", "CD11bmac_freq", "InfMono_freq",
#              "DC_freq", "moDC_freq", "DC103_freq", "PMN_freq", "Tcell_freq", "CD4_freq",
#              "CD44_freq", "CD62_freq", "CD8_freq", "Bcell_freq")
              
freq <- tibble()
for (file in files){
  data <- read_excel(path = paste(directory, file, sep=""), na = 'N/A', col_names = TRUE)
  #colnames(data) <- colnames
  freq <- rbind(freq, data)
}

Freq_plot(filter(freq, hpi != 0),y = `% Apop_infected`)


  # plots with lots of information
(p_freq <- freq %>% 
    filter(hpi != 0) %>%
    ggplot() +
    aes(x = fct_relevel(genotype, 'comp', 'KO'), y = `% Apop_infected`, 
        fill = genotype) +
    geom_point(aes(shape = genotype, color = genotype)) +
    stat_compare_means(method = "t.test", label.y = 9) +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap( ~hpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('Hours post infection') +
    ylab("Apop_Infected") +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45))
)   
(p_counts <- Freq %>% 
    filter(wpi != 12) %>%
    ggplot() +
    aes(x = fct_relevel(genotype, 'fl', 'cre'), y = AM_count, 
        fill = genotype) +
    geom_point(aes(shape = exp, color = genotype)) +
    stat_compare_means(method = "t.test", label.y = 9) +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap( ~wpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('Weeks post infection') +
    ylab("AM Frequency (%CD45+)") +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45))
)   

# a plot to play around with
(p <- Freq %>% 
  filter(wpi == 2) %>%
  ggplot() +
  aes(x = genotype, y = AM_freq) +
  geom_point(aes(shape = exp, color = genotype)))

# plots for the figures
png(filename = 'AM_FrequenciesOverTime_F30/AM_Freq_Timecourse.png')
(p_freq_pup <- Freq %>% 
    ggplot() +
    aes(x = fct_relevel(genotype, 'fl', 'cre'), y = CD4_freq) +
    geom_point(aes(color = genotype)) +
    stat_compare_means(method = "t.test", label.y = 9, label = 'p.format') +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap( ~dpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('dpi') +
    ylab("AM Frequency (%CD45+)") +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          panel.background = element_rect(fill = 'white'),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          panel.grid.major = 
            element_line(color = 'grey', size = 0.1),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45, size = 16),
          legend.position = 'none')
)  
dev.off()

# a plot to change based on cell type of interest
(p_freq_pup <- Freq %>% 
    ggplot() +
    aes(x = fct_relevel(genotype, 'fl', 'cre'), y =  InfMono_freq) +
    geom_point(aes(color = genotype)) +
    #stat_compare_means(method = "t.test", label.y = 9) +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap( ~dpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('dpi') +
    ylab("Infammatory Monocyte Frequency (%CD45+)") +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          panel.background = element_rect(fill = 'white'),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          panel.grid.major = 
            element_line(color = 'grey', size = 0.1),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45, size = 16),
          legend.position = 'none')
)  

(p_counts_pup <- Freq %>% 
    filter(wpi != 12) %>%
    ggplot() +
    aes(x = fct_relevel(genotype, 'fl', 'cre'), y = AM_count) +
    geom_point(aes(color = genotype)) +
    #stat_compare_means(method = "t.test", label.y = 9) +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap( ~wpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('Weeks post infection') +
    ylab("AM Counts (in L lung)") +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'grey', size = 0.1),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45))
)   


CD11c_Freq <- read_csv('AM_FrequenciesOverTime_F30/CD11c_naive_8wpi_AM_Freq.csv') %>%
  mutate(genotype = genotpye, dpi = 7*wpi)
(p_counts_cd11c <- CD11c_Freq %>% 
    ggplot() +
    aes(x = fct_relevel(CD11c_Freq$genotype, 'fl', 'cre'), y = Number) +
    geom_point(aes(color = CD11c_Freq$genotype)) +
    stat_compare_means(method = "t.test", label.y = 9) +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap( ~wpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('Weeks post infection') +
    ylab("AM Counts (in L lung)") +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(color = 'grey', size = 0.1),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45))
)   

(p_freq_CD11c <- CD11c_Freq %>% 
    #filter(dpi == 56) %>%
    ggplot() +
    aes(x = fct_relevel(genotype, 'fl', 'cre'), y = perc_CD45, 
        fill = genotype) +
    geom_point(aes(color = genotype)) +
    stat_compare_means(method = "t.test", label.y = 5) +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap( ~dpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    xlab('dpi') +
    ylab("AM Frequency (%CD45+)") +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
          panel.background = element_rect(fill = 'white'),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          panel.grid.major = 
            element_line(color = 'grey', size = 0.1),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text.x = element_text(angle = 45, size = 16),
          legend.position = 'none')
)   

ggsave(filename = "AM_FrequenciesOverTime_F30/CD11c_AM_Freq.png")
