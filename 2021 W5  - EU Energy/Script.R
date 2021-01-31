library(tidyverse)
library(ggplot2)
library(ggrepel)

energy <- read.csv('Data-file-Europe-Power-Sector-2020.csv',
                   fileEncoding="UTF-8-BOM")

energy_df_clean <- energy %>%
  rename(Country = Area,
         Generation = Generation..TWh.,
         Share = Share.of.production....) %>%
  select(-Change.on.last.year....,
         -Change.on.last.year..TWh.) %>%
  filter(!Variable %in% c('Production',
                          'Net imports',
                          'Demand',
                          'Wind',
                          'Solar')) %>%
  filter(!Country %in% c('United Kingdom',
                         'EU27+1')) %>%
  mutate(Share = Share/100)

energy_pivot_df <- energy_df_clean %>%
  filter(Variable == 'Fossil',
         Year %in% c(2000, 2010, 2020)) %>%
  select(-Variable,
         -TotalVariable,
         -PctTotalVariable,
         -Generation) %>%
  pivot_wider(names_from = Year,
              values_from = Share )

ggplot(energy_pivot_df, 
       aes(x = reorder(Country, -`2020`),
           y = `2010`)) + 
  coord_flip() +
  #geom_segment(data = subset(energy_pivot_df, Country == 'EU-27'),
  #             mapping = aes(x = reorder(Country, -`2020`),
  #                           xend = reorder(Country, -`2020`),
  #                           y = -Inf,
  #                           yend = Inf),
  #             color = 'gray',
  #             alpha = 0.4,
  #             size = 12) +
  geom_point(mapping = aes(x = reorder(Country, -`2020`),
                           y = `2010`),
             size = 7,
             color = '#fc8d59') +
  geom_point(mapping = aes(x = reorder(Country, -`2020`),
                           y = `2020`),
             size = 7,
             color = 'steelblue') +
  geom_segment(aes(xend = reorder(Country, -`2020` ),
                   yend = `2020` + 0.01),
               arrow = arrow(length = unit(0.01, "npc"),
                             type = 'open'),
               size = 1.5,
               color = 'black') +
  geom_hline(yintercept = 0.5,
             size = 1,
             color = 'gray',
             linetype = 'dashed') +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1)) +
  labs(title = '2010-2020: Which EU Countries Produce Electricity with Fossil Fuels?',
       subtitle = 'Between 2010 and 2020, all 27 EU countries saw a decline in electricity production through fossil fuels. However, nine\ncountries still get a majority of their electrcity through fossil fuels.The EU set a target of lowering carbon emissions to\n55% of 1990 levels by 2030.\n',
       color = '',
       x = '',
       y = '\nShare of Electricity Production',
       caption = '\nVisualization by Alex Elfering\nData Source: Ember' ) +
  geom_text(mapping = aes(x = reorder(Country, -`2020`),
                                y = `2020`,
                                label = scales::percent(`2020`-`2010`)),
                  size = 5,
                  vjust = 0.5,
                  hjust = 1.4) +
  #scale_color_manual(labels = c("Fossil Fuels", "T888")) +
  theme(plot.title = element_text(face = 'bold', size = 18, family = 'Arial'),
        legend.position = 'none',
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 14, color = 'black', family = 'Arial'),
        legend.title = element_text(size = 14,color = 'black',  family = 'Arial'),
        plot.subtitle = element_text(size = 15,color = 'black',  family = 'Arial'),
        plot.caption = element_text(size = 14,color = 'black',  family = 'Arial'),
        axis.title = element_text(size = 14,color = 'black',  family = 'Arial'),
        axis.text = element_text(size = 14,color = 'black',  family = 'Arial'),
        strip.text = ggplot2::element_text(size = 14, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
        strip.background = element_rect(fill = NA),
        panel.background = ggplot2::element_blank(),
        axis.line = element_line(colour = "#222222", linetype = "solid"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed")) 
