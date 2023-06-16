## A quick script to plot CellTiterGlo data

library(tidyverse)
library(xlsx)
library(ggpubr)

# import data from spreadsheet modified and with missing values removed and metadata added
data <- read.xlsx(file = 'IA6_CytoToxGlo_Sptlc2_AMs.xlsx', sheetName = 'ForR', na = "NA")

# plot but filter out naive mouse 2 with 6250 cells because it is an outlier
(data %>% ggplot(aes(x = fct_relevel(genotype, c('fl', 'cre')), 
                     y = as.double(RBCLysed_Cytotox))) +
    ylab('Lum') +
    xlab('Genotype')) +
    geom_point(aes(color = genotype)) +
    #stat_compare_means(method = "t.test", label = 'p.format') +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap(~fct_relevel(treatment, c('mock', 'infected')), strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    ggtitle('CytotoxGlo') +
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
          axis.line = element_line())
ggsave(filename = 'IA6_AM_CytoToxGlo_Spltc2_RBCLysedBAL.png')

(data %>% ggplot(aes(x = fct_relevel(genotype, c('fl', 'cre')), 
                     y = as.double(Total_Cytotox))) +
    ylab('Lum') +
    xlab('Genotype')) +
    geom_point(aes(color = genotype)) +
    stat_compare_means(method = "t.test", label = 'p.format') +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    facet_wrap(~fct_relevel(treatment, c('mock', 'infected')), strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    ggtitle('CytotoxGlo') +
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
          strip.text.x = element_text(angle = 0, size = 16),
          axis.line = element_line())
ggsave(filename = 'IA6_AM_CytoToxGlo_Spltc2_TotalBAL.png')

(data %>% ggplot(aes(x = fct_relevel(genotype, c('fl', 'cre')), 
                     y = as.double(Total_Cytotox))) +
    ylab('Lum') +
    xlab('Genotype')) +
    geom_point(aes(color = genotype, shape = genotype)) +
    facet_wrap(~fct_relevel(treatment, c('mock', 'infected')), strip.position = 'bottom', scales = 'free_x', nrow = 1)+
    ggtitle('CytotoxGlo') +
    guides(fill = 'none', color = 'none') +
   # stat_compare_means(method = "t.test", label.y = 9, label = 'p.format') +
    stat_summary(geom = "pointrange", fun.data = mean_se, size = 0.25) +
    geom_signif(comparisons = list(c('fl', 'cre')),test = "t.test", map_signif_level = TRUE, size = 0.4, textsize = 3.5) +
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
        axis.line = element_line())



ggsave(filename = 'IA6_AM_CytoToxGlo_Spltc2_TotalBAL.png',
       width = 6, height = 4.2, units = c("in"), dpi = 300)


