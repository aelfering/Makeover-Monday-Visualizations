list.of.packages <- c("ggplot2", 
                      'dplyr', 
                      'tidyverse', 
                      'tidyr',
                      'ggrepel')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggrepel)

setwd("~/GitHub/Makeover-Monday-Visualizations/2020 W42 - Healthcare Spending")

health_spending <- read.csv('health spending.csv',
                            fileEncoding="UTF-8-BOM")

head(health_spending)
str(health_spending)

total_spending <- dplyr::filter(health_spending,
                                SUBJECT == 'TOT',
                                MEASURE == 'PC_GDP')

# How has the change in health care spending based on GDP changed since 1971?
init_gdp <- total_spending %>%
  group_by(LOCATION) %>%
  slice(which.min(TIME)) %>%
  ungroup() %>%
  select(LOCATION,
         Init_Pct = Value)

spend_change <- total_spending %>%
  inner_join(init_gdp) %>%
  mutate(Change = Value - Init_Pct)

rank_max <- spend_change %>%
  filter(TIME == max(TIME)) %>%
  mutate(RankDense = dense_rank(desc(Value))) %>%
  filter(RankDense <= 5)
  
ggplot(spend_change,
       aes(x = TIME,
           y = Value/100,
           group = LOCATION)) +
  geom_hline(yintercept = 0, size = 1) +
  # plotting the other countries
  geom_line(color = '#d4d4d4',
            alpha = 0.4,
            size = 1.5) +
  # plotting the top 5 countries
  geom_line(data = subset(spend_change, LOCATION %in% rank_max$LOCATION),
            mapping = aes(x = TIME,
                          y = Value/100,
                          color = LOCATION),
            size = 1.5) +
  geom_point(data = subset(spend_change, LOCATION %in% rank_max$LOCATION & TIME == max(TIME)),
             mapping = aes(x = TIME,
                           y = Value/100,
                           color = LOCATION),
             size = 3) +
  geom_label_repel(data = subset(spend_change, LOCATION %in% rank_max$LOCATION & TIME == max(TIME)),
                   mapping = aes(x = TIME,
                                 y = Value/100,
                                 color = LOCATION,
                                 label = LOCATION)) +
  # other ggplot features
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854'),
                     labels = c('Switzerland', 'Japan', 'Norway', 'Portugal', 'United States')) +
  labs(title = 'Which Countries saw the Largest Increase in Healthcare Spending as a Percent of GDP?',
       subtitle = '\n',
       x = 'Year\n',
       y = 'Change in Percent of GDP\n',
       color = 'Countries',
       caption = 'Visualization by Alex Elfering | Data Source: OECD') +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'top',
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






