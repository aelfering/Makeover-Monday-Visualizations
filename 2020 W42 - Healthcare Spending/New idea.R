list.of.packages <- c("ggplot2", 
                      'dplyr', 
                      'tidyverse', 
                      'tidyr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

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
  mutate(Change = Value - Init_Pct,
         LOCATION1 = LOCATION)

rank_max <- spend_change %>%
  filter(TIME == max(TIME)) %>%
  mutate(RankDense = dense_rank(desc(Value))) %>%
  filter(RankDense <= 6)

spend_change$LOCATION1 <- factor(spend_change$LOCATION1, levels = unique(spend_change$LOCATION1[order(spend_change$Change)]))

spend_change[order(-spend_change$Change),]

ggplot(subset(spend_change, LOCATION %in% rank_max$LOCATION), 
       aes(x = TIME, 
           y = Change/100,
           group = LOCATION1)) + 
  geom_line(data = transform(spend_change,
                             LOCATION = NULL), 
            aes(group = LOCATION1), 
            size = 1.5,
            color = '#d4d4d4',
            alpha = 0.4) +   
  geom_hline(yintercept = 0,
             color = '#898989',
             linetype = 'dashed',
             size = 1) +
  geom_line(subset(spend_change, LOCATION %in% rank_max$LOCATION),
            mapping = aes(group = LOCATION), 
            color = '#F56831',
            size = 1.5) + 
  geom_point(data = subset(spend_change, LOCATION %in% rank_max$LOCATION & TIME == max(TIME)),
             mapping = aes(x = TIME,
                           y = Change/100), 
             color = '#F56831',
             size = 3) +
  geom_label_repel(data = subset(spend_change, LOCATION %in% rank_max$LOCATION & TIME == max(TIME)),
                   mapping = aes(x = TIME,
                                 y = Change/100,
                                 label = paste('+', round(Change, 2), '%', sep = '')),
                   box.padding = 2) +
  scale_colour_identity() + 
  facet_wrap(~LOCATION, nrow = 2) +
  labs(title = 'Which Countries saw the Largest Increase in Health Care Spending as a Percent of GDP?',
       subtitle = 'Based on Change in Percent of Total GDP\n\nHealth care spending is defined as \n',
       x = 'Year\n',
       y = 'Change in Percent of GDP\n',
       color = 'Countries',
       caption = 'Visualization by Alex Elfering | Data Source: OECD\nDesign inspired by John Burn-Murdoch') +
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






