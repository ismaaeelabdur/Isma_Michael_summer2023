## A quick script to plot CellTiterGlo data

library(tidyverse)
library(xlsx)
#library(ggpubr)

# import data from spreadsheet modified and with missing values removed and metadata added
data <- read.xlsx(file = 'IA4_AM_CTG_Pilot/IA4_CTG_wLid.xlsx', sheetName = 'forR', na = "NA")

# plot but filter out naive mouse 2 with 6250 cells because it is an outlier
(filter(data, lum != 972890) %>% ggplot(aes(x = cells, y = as.double(lum), color = treatment)) +
    geom_line(aes(linetype = as.character(mouse))) + 
                geom_point() +
    ylab('Lum') +
    xlab('Cells/well'))

ggsave(filename = 'IA4_AM_CTG_Pilot/IA_CTG_wLid_plot.png')
