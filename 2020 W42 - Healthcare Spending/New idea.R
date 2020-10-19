# load data
list.of.packages <- c("ggplot2", 
                      'dplyr', 
                      'tidyverse', 
                      'tidyr',
                      'ggrepel',
                      'directlabels',
                      'scales')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggrepel)
library(directlabels)
library(scales)

# set wd and data source
setwd("~/GitHub/Makeover-Monday-Visualizations/2020 W42 - Healthcare Spending")

health_spending <- read.csv('health spending.csv',
                            fileEncoding="UTF-8-BOM")

# what does this data look like?
head(health_spending)
str(health_spending)

# filter on total spending based on usd per capita
total_spending <- health_spending %>%
  filter(SUBJECT == 'TOT',
         MEASURE == 'USD_CAP') %>%
  mutate(LOCATION1 = LOCATION)
  
  
  dplyr::filter(health_spending,
                                SUBJECT == 'TOT',
                                MEASURE == 'USD_CAP')

# which countries have the least spending as 2019?
rank_min <- total_spending %>%
  filter(TIME == max(TIME)) %>%
  mutate(RankDense = dense_rank((Value))) %>%
  filter(RankDense <= 5)

# which countries have the most spending as of 2019?
# this will be for reference in the final visualization
rank_max <- total_spending %>%
  filter(TIME == max(TIME)) %>%
  mutate(RankDense = dense_rank(desc(Value))) %>%
  filter(RankDense <= 3)

# the final visualization
ggplot(subset(total_spending, LOCATION %in% rank_min$LOCATION), 
       aes(x = TIME, 
           y = Value/100,
           group = LOCATION1)) + 
  geom_line(data = transform(total_spending, LOCATION = NULL), 
            aes(group = LOCATION1), 
            size = 1.5,
            color = '#d4d4d4',
            alpha = 0.4) +   
  geom_line(data = transform(total_spending, LOCATION = NULL) %>% filter(LOCATION1 %in% rank_max$LOCATION), 
            aes(group = LOCATION1), 
            size = 1.5,
            color = '#969696') +   
  geom_point(data = transform(total_spending, LOCATION = NULL) %>% filter(LOCATION1 %in% rank_max$LOCATION, TIME == max(TIME)), 
            aes(group = LOCATION1), 
            size = 3,
            color = '#969696') +   
  geom_hline(yintercept = 0,
             color = '#898989',
             linetype = 'dashed',
             size = 1) +
  geom_line(subset(total_spending, LOCATION %in% rank_min$LOCATION),
            mapping = aes(group = LOCATION), 
            color = '#F56831',
            size = 1.5) + 
  geom_point(data = subset(total_spending, LOCATION %in% rank_min$LOCATION & TIME == max(TIME)),
             mapping = aes(x = TIME,
                           y = Value/100), 
             color = '#F56831',
             size = 3) +
  geom_label_repel(data = transform(total_spending, LOCATION = NULL) %>% filter(LOCATION1 %in% rank_max$LOCATION, TIME == max(TIME)),
                   mapping = aes(x = TIME,
                                 y = Value/100,
                                 label = LOCATION1),
                   box.padding = 2) +
  geom_label_repel(data = subset(total_spending, LOCATION %in% rank_min$LOCATION & TIME == max(TIME)),
                   mapping = aes(x = TIME,
                                 y = Value/100,
                                 label = round(Value, 2)),
                   box.padding = 2) +
  scale_colour_identity() + 
  facet_wrap(~LOCATION, nrow = 1) +
  labs(title = 'Which Countries Spends the Least on Compulsory Health Spending?',
       subtitle = 'Based on US per Capita as 2019\n\nAs of 2019, the only country to see a decline in compulsory health spending was Hungary \n',
       x = 'Year\n',
       y = 'Percent of GDP\n',
       caption = 'Compulsory health spending includes government spending and health insurance.\nVisualization by Alex Elfering | Data Source: OECD\nDesign inspired by John Burn-Murdoch') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12, family = 'Arial'),
        legend.title = element_text(size = 12, family = 'Arial'),
        plot.subtitle = element_text(size = 15, family = 'Arial'),
        plot.caption = element_text(size = 12, family = 'Arial'),
        axis.title = element_text(size = 12, family = 'Arial'),
        axis.text = element_text(size = 8, family = 'Arial'),
        strip.text = ggplot2::element_text(size = 12, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", linetype = "dashed"),
        panel.grid.major.x = element_blank()) 






