library(tidyverse)
library(ggplot2)
library(ggrepel)
loadfonts(device = "win")
library(extrafont)

#font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)

setwd("~/GitHub/Makeover-Monday-Visualizations/2020 W51 - Arsenal FC")

arsenal <- read.csv('Copy of Week51.csv')

# clean the data 

colnames(arsenal) <- c('Season',
                       'Played',
                       'Won',
                       'Drawn',
                       'Lost',
                       'GoalsFor',
                       'GoalsAgainst',
                       'GoalDifference',
                       'Points',
                       'Position',
                       'FinalPoints',
                       'FinalPosition')

arsenal_cleaned <- arsenal %>%
  filter(!is.na(Played)) %>%
  mutate(Season = as.numeric(substr(Season, 1, 4)),
         GoalsScoredPerGame = GoalsFor/Played,
         GoalsAllowedPergame = GoalsAgainst/Played,
         PctWon = Won/Played) %>%
  mutate(PctGroup = cut(PctWon, c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c('0-20%', '20-40%', '40-60%', '60-80%', '80-100%'), include.lowest=TRUE),
         AverageGoalsScored = mean(GoalsScoredPerGame),
         AverageGoalsAllowed = mean(GoalsAllowedPergame))
  
# visualizations
top_seasons <- arsenal_cleaned %>%
  arrange(desc(GoalDifference)) %>%
  filter(dense_rank(desc(GoalDifference)) <= 3)

bottom_seasons <- arsenal_cleaned %>%
  arrange((GoalDifference)) %>%
  filter(dense_rank((GoalDifference)) <= 3)


ggplot(arsenal_cleaned,
       aes(x = GoalsScoredPerGame,
           y = GoalsAllowedPergame,
           group = Season)) +
  geom_hline(aes(yintercept = AverageGoalsAllowed),
             size = 1,
             linetype = 'solid') +
  geom_vline(aes(xintercept = AverageGoalsScored),
             size = 1,
             linetype = 'solid') +
  geom_point(aes(color = PctGroup),
             size = 4) +
  scale_color_manual(values = c('0-20%' = '#e41a1c', 
                                '20-40%' = '#377eb8', 
                                '40-60%' = '#4daf4a', 
                                '60-80%' = '#984ea3', 
                                '80-100%' = '#ff7f00')) +
  geom_label_repel(data = top_seasons,
                   mapping = aes(x = GoalsScoredPerGame,
                                 y = GoalsAllowedPergame,
                                 label = Season),
                   box.padding = 1) +
  geom_label_repel(data = bottom_seasons,
                   mapping = aes(x = GoalsScoredPerGame,
                                 y = GoalsAllowedPergame,
                                 label = Season),
                   box.padding = 1) +
  labs(title = "2020 is shaping up to be Arsenal's Worst Start so far",
       subtitle = "It's been over a month since Arsenal's last victory. The Gunners have won four games through 18-December and are 15th\nin the League, their slowest start in 28 seasons.\n",
       x = '',
       y = '',
       color = 'Percent of Games Won',
       caption = '\nVisualization by Alex Elfering\nSource: 11v11\nData is through 18-December of each season') +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 18, 
                                  color = 'black',
                                  family = 'Arial'),
        legend.position = 'top',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 12,
                                   color = 'black',
                                   family = 'Arial'),
        legend.title = element_text(size = 12, 
                                    family = 'Arial'),
        plot.subtitle = element_text(size = 15, 
                                     family = 'Arial', 
                                     color = 'black'),
        plot.caption = element_text(size = 12, 
                                    family = 'Arial', 
                                    color = 'black'),
        axis.title = element_text(size = 12,
                                  family = 'Arial', 
                                  color = 'black'),
        axis.text = element_text(size = 12, 
                                 family = 'Arial', 
                                 color = 'black'),
        strip.text = ggplot2::element_text(size = 12, 
                                           hjust = 0, 
                                           face = 'bold', 
                                           color = 'black', 
                                           family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", 
                                 linetype = "solid"),
        panel.grid.major.y = element_line(colour = "#c1c1c1", 
                                          linetype = "dashed"),
        panel.grid.major.x = element_line(colour = "#c1c1c1", 
                                          linetype = "dashed")) 


