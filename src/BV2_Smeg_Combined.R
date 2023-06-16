# A script that plots and combines data from IA1 & IA2 
# smeg infected and naive sptlc2 -/- & complimented BV2 cells 1hpi & 18hpi

library(tidyverse)
library(ggpubr)
library(readxl)

Freq_plot <- function(data, y_freq){
(p_freq <- data %>% 
    ggplot(aes(x = fct_relevel(genotype, 'WT', 'KO'), y = get(y_freq))) +
    geom_point(aes(color = genotype, shape = genotype)) +
    #stat_compare_means(method = "t.test", label = 'p.format') +
    geom_signif(comparisons = list(c('WT', 'KO')),test = "t.test", 
               map_signif_level = TRUE, size = 0.4, textsize = 3.5) +
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
         strip.text.x = element_text( size = 16),
         legend.position = 'none',
         axis.line = element_line()))
}

directory_IA1 <- 'IA1_M. smeg infection (apoptosis, phagocytosis, microscopy)/'
files_IA1 <- c('Spreadsheets/IA_MN1_18hpi_noD2.xlsx', 'Spreadsheets/IA1_1hpi.xlsx', 'Spreadsheets/Naive_18hpi.xlsx')
colnames <- c('conditon', "BV2", "Infected", "Apop_Infected", "Apop_BV2",
              "GFP_MFI", "Apop_Infected_Median", "Apop_BV2_Median", "genotype", "hpi")
              
freq <- tibble()
for (file in files_IA1){
  data <- read_excel(path = paste(directory_IA1, file, sep=""), na = 'N/A', col_names = TRUE)
  colnames(data) <- colnames
  freq <- rbind(freq, data)
}

#initiating a matrix for combined data
combined <- mutate(freq, infection = c(rep('Smeg', 12), rep(NA, 6)), exp = 'IA_1')
 
directory_IA2 <- 'IA2_HKTB_Smeg/IA2_spreadsheets/'
files_IA2 <- c('IA2_1hpi_smeg.xlsx', 'IA2_18hpi_HKTB.xlsx', 'IA2_18hpi_smeg.xlsx', 'IA2_Naive18h.xlsx')
colnames <- c('conditon', "BV2", "Infected", "Apop_Infected", "Apop_BV2",
              "GFP_MFI", "Apop_Infected_Median", "Apop_BV2_Median", "genotype", "hpi", "infection")

freq <- tibble()
for (file in files_IA2){
  data <- read_excel(path = paste(directory_IA2, file, sep=""), na = c('NA','N/A'), col_names = TRUE)
  colnames(data) <- colnames
  freq <- rbind(freq, data)
}

freq <- mutate(freq, exp = 'IA_2')

# import MARA_1data 
directory_MARA1 <- 'MARA_Data/'
files_MARA1 <- 'MARA #1.xlsx'
colnames <- c('conditon', "BV2", "Infected", "Apop_Infected", "Apop_BV2",
              "GFP_MFI", "Apop_Infected_Median", "Apop_BV2_Median", "genotype", "hpi", "infection")

freq <- tibble()
for (file in files_MARA1){
  data <- read_excel(path = paste(directory_MARA1, file, sep=""), na = c('NA','N/A'), col_names = TRUE)
  colnames(data) <- colnames
  freq <- rbind(freq, data)
}

freq <- mutate(freq, exp = 'MARA1')


combined <- rbind(combined, freq)
# replace NA infection with Naive
combined$infection <- replace_na(combined$infection, 'Naive')
combined$hpi[combined$hpi == 0] <- 18
combined$genotype[combined$genotype == 'comp'] <- 'WT'

Freq_plot(filter(combined, infection == 'Smeg', hpi != 0), "Infected")
ggsave(filename = "IA_Combined_PercentInfected.png")
Freq_plot(filter(combined, infection == 'Smeg', hpi != 0), "GFP_MFI")
ggsave(filename = "IA_Combined_GFP_MFI.png")
Freq_plot(filter(combined, infection == 'Smeg', hpi != 0), "Apop_Infected_Median")
ggsave(filename = "IA_Combined_MedianApoptosingofInfected.png")
Freq_plot(combined, "Apop_BV2_Median") + facet_wrap(~infection)
ggsave(filename = "IA_Combined_MedianApoptosingofBV2.png")
Freq_plot(filter(combined, hpi != 0), "Apop_BV2") + facet_wrap(~infection)
ggsave(filename = "IA_Combined_PercentApoptosingofBV2.png")
# plot of naive BV2s
Freq_plot(filter(combined, infection == 'Naive'), "Apop_BV2")
ggsave(filename = "IA_Combined_PercentApoptosingofnaiveBV2.png")
Freq_plot(filter(combined, hpi != 0), "Apop_Infected")
ggsave(filename = "IA_Combined_PercentApoptosingofInfected.png",
       width = 6, height = 4.2, units = c("in"), dpi = 300)


Freq_plot(filter(combined, infection == 'Naive', exp == 'IA_2'), "Apop_BV2")


Freq_plot(filter(combined, infection == 'HKTB'), "Apop_BV2")
ggsave(filename = "IA_Combined_PercentApoptosingofInfected.png",
       width = 6, height = 4.2, units = c("in"), dpi = 300)

(p_MARA <- combined %>% 
    filter(exp == 'MARA1', hpi == 18, infection == 'Smeg') %>%
    ggplot(aes(x = fct_relevel(genotype, 'WT', 'KO'), y = Apop_Infected_Median)) +
    geom_point(aes(color = genotype, shape = exp)) +
    #stat_compare_means(method = "t.test", label = 'p.format') +
    geom_signif(comparisons = list(c('WT', 'KO')),test = "t.test", 
               map_signif_level = TRUE, size = 0.4, textsize = 3.5) +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    #facet_wrap(~hpi, strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    ylab('FLICA MFI') +
    xlab('Genotype') +
    guides(fill = 'none', color = 'none') +
    theme(panel.spacing = unit(0, 'lines'),
         panel.background = element_rect(fill = 'white'),
         axis.text = element_text(size = 16),
         axis.title = element_text(size = 20),
         panel.grid.major = 
           element_line(color = 'grey', size = 0.1),
         strip.background = element_blank(),
         strip.placement = 'outside',
         strip.text.x = element_text( size = 16),
         #legend.position = 'none',
         axis.line = element_line()))
ggsave(filename = 'MARA_Data/FLICA_MFI_MARA1.png')
