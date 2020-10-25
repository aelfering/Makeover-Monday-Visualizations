# makeovermonday 2020 w43
# clothing exports to the United States

# Load packages
list.of.packages <- c("ggplot2", 
                      "dplyr", 
                      'tidyr', 
                      'tidyverse', 
                      'scales', 
                      'ggrepel', 
                      'extrafont')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)  
library(dplyr) 
library(tidyr)
library(tidyverse)
library(scales)
library(ggrepel)
library(extrafont)
library(directlabels)

setwd("~/GitHub/Makeover-Monday-Visualizations/2020 W43 - Clothing Exports")

clothing_exports <- read.csv('data source.csv')

head(clothing_exports)

# fix the column names
colnames(clothing_exports) <- c('Country', 'Year', 'Month', 'Exports')

# just remember the exports are in USD million


min_year_exports <- dplyr::filter(clothing_exports, Year == min(Year))
max_year_exports <- dplyr::filter(clothing_exports, Year == max(Year))

compare_years <- full_join(min_year_exports, max_year_exports, by = c('Country' = 'Country', 'Month' = 'Month'))

yoy_comparison <- compare_years %>%
  select(Country,
         Month,
         Exports.2019 = Exports.x,
         Exports.2020 = Exports.y) %>%
  mutate(yoy = (Exports.2020-Exports.2019)/Exports.2019 )

yoy_comparison$Month <- factor(yoy_comparison$Month, levels = unique(c('March', 'April', 'May', 'June')))

ggplot(yoy_comparison,
       aes(x = Month,
           y = yoy,
           group = Country,
           color = Country)) +
  geom_hline(yintercept = 0,
             linetype = 'dashed') +
  geom_line(size = 1.5) +
  geom_point(subset(yoy_comparison, Month == 'June'),
             mapping = aes(x = Month),
             size = 2.5) +
  geom_dl(aes(label = paste('  ', Country, sep = '')), 
          method = list(dl.combine("last.points")), 
          cex = 0.8) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c('#1b9e77', '#d95f02', '#7570b3', '#e7298a', '#66a61e', '#e6ab02')) +
  labs(title = 'Apparel Export Value to the United States is still down from March',
       subtitle = 'Year-over-year change in value since last year (USD millions)\n\nAs of June 2020, export value of apparel is down between -19% to -86% among countries',
       y = '',
       x = '',
       caption = '\nVisualization by Alex Elfering | Data Source: Data manually lifted from source article. Original source is Otexa') +
    theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
          legend.position = 'none',
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.text = element_text(size = 12, family = 'Arial'),
          legend.title = element_text(size = 12, family = 'Arial'),
          plot.subtitle = element_text(size = 15, family = 'Arial'),
          plot.caption = element_text(size = 12, family = 'Arial'),
          axis.title = element_text(size = 12, family = 'Arial'),
          axis.text = element_text(size = 12, family = 'Arial'),
          strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
          strip.background = element_rect(fill = NA),
          panel.background = ggplot2::element_blank(),
          axis.line = element_line(colour = "#222222", linetype = "solid"),
          panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
          panel.grid.major.x = element_blank()) 
  























