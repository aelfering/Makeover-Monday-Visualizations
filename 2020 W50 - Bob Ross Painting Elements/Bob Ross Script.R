library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

#setwd("~/GitHub/Makeover-Monday-Visualizations/2020 W50 - Bob Ross Painting Elements")

bob_ross <- read.csv('Bob Ross.csv')

#### data cleaning
colnames(bob_ross) <- c('Element',
                        'Episode',
                        'Title',
                        'Included')

# separate the season from episode number
clean_br_df <- bob_ross %>%
  mutate(Episode.Clean = gsub('S', '', Episode),
         Episode.Clean = gsub('E', ' ', Episode.Clean)) %>%
  mutate(Season = as.numeric(substr(Episode.Clean, 1, 2)),
         Episode.No = as.numeric(substr(Episode.Clean, 4, 5)))

episode_index <- clean_br_df %>%
  distinct(Episode,
           Season,
           Episode.No) %>%
  arrange(Season,
          Episode.No) %>%
  mutate(Episode.Overall = row_number())

bob_ross_elememt <- inner_join(clean_br_df, episode_index)


# building the r shiny dashboard
ui <- shinyUI(fluidPage(  
  titlePanel("Bob Ross Shiny Dashboard"),  
  sidebarLayout(  
    sidebarPanel(
      selectInput("Base", "Select a Base Season",
                  seq(1, 31),
                  selected = 5),
      selectInput("Compare", "Select a Compare Season",
                  seq(1, 31),
                  selected = 15),
      width=2),
    mainPanel(
      plotOutput("Descending", width = "100%"),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      plotOutput("Ascending", width = "100%")
    )  
  )  
))

server <- shinyServer(function(input, output) { 
  # time series graph
  output$Descending <- renderPlot({  
    
    
    a_episodes <- bob_ross_elememt %>%
      filter(Season == input$Base) %>%
      summarise(A_Episodes = n_distinct(Episode))
    
    b_episodes <- bob_ross_elememt %>%
      filter(Season == input$Compare) %>%
      summarise(B_Episodes = n_distinct(Episode))
    
    a_index <- bob_ross_elememt %>%
      filter(Season == input$Base,
             Included == 1) %>%
      group_by(Element) %>%
      summarise(a_Included = sum(Included)) %>%
      ungroup() %>%
      mutate(Pct = a_Included/a_episodes$A_Episodes) %>%
      arrange(desc(a_Included))
    
    b_index <- bob_ross_elememt %>%
      filter(Season == input$Compare,
             Included == 1) %>%
      group_by(Element) %>%
      summarise(b_Included = sum(Included)) %>%
      ungroup() %>%
      mutate(Pct = b_Included/b_episodes$B_Episodes) %>%
      arrange(desc(b_Included))
    
    index_join <- full_join(a_index,
                            b_index,
                            by = c('Element' = 'Element')) %>%
      mutate(Index = Pct.y-Pct.x) %>%
      arrange(desc(Index)) %>%
      filter(Index >= 0.01)
    
    ggplot(index_join,
           aes(x = reorder(Element, Index),
               y = Pct.y)) +
      coord_flip() +
      geom_point(size = 6,
                 color = 'orange') +
      geom_point(mapping = aes(x = reorder(Element, Index),
                               y = Pct.x),
                 color = 'gray',
                 size = 6) +
      labs(title = paste('Which Elements Appeared in More Bob Ross Paintings in\nSeason ', input$Compare, ' from Season ', input$Base, '?\n', sep = ''),
           x = 'Element\n',
           y = '\nFrequency in Episodes') +
      geom_text(data = index_join %>% slice(which.max(Index)),
                mapping = aes(x = reorder(Element, Index),
                              y = Pct.x,
                              label = paste('Season ', input$Base, sep = '')),
                vjust = -1,
                size = 6) +
      geom_text(data = index_join %>% slice(which.max(Index)),
                mapping = aes(x = reorder(Element, Index),
                              y = Pct.y,
                              label = paste('Season ', input$Compare, sep = '')),
                vjust = -1,
                size = 6) +
      geom_text(mapping = aes(x = reorder(Element, Index),
                              y = Pct.y,
                              label = paste('+', scales::percent(Index), sep = '')),
                vjust = 2,
                size = 4) +
      geom_curve(aes(x = reorder(Element, Index),
                     xend =  reorder(Element, Index),
                     y = Pct.x + 0.005,
                     yend = Pct.y - 0.005),
                 arrow = arrow(length = unit(0.02, "npc"),
                               type = 'open'),
                 curvature = -0.03,
                 size = 1) +
      scale_y_continuous(labels = scales::percent) +
      theme(plot.title = element_text(face = 'bold', size = 24, family = 'Arial'),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 14, family = 'Arial'),
            legend.title = element_text(size = 14, family = 'Arial'),
            plot.subtitle = element_text(size = 15, family = 'Arial'),
            plot.caption = element_text(size = 14, family = 'Arial'),
            axis.title = element_text(size = 14, family = 'Arial'),
            axis.text = element_text(size = 14, family = 'Arial'),
            strip.text = ggplot2::element_text(size = 14, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", linetype = "solid"),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed")) 
    
    
    
  }, height = 800, width = 'auto')
  
  output$Ascending <- renderPlot({  
    
    
    a_episodes <- bob_ross_elememt %>%
      filter(Season == input$Base) %>%
      summarise(A_Episodes = n_distinct(Episode))
    
    b_episodes <- bob_ross_elememt %>%
      filter(Season == input$Compare) %>%
      summarise(B_Episodes = n_distinct(Episode))
    
    a_index <- bob_ross_elememt %>%
      filter(Season == input$Base,
             Included == 1) %>%
      group_by(Element) %>%
      summarise(a_Included = sum(Included)) %>%
      ungroup() %>%
      mutate(Pct = a_Included/a_episodes$A_Episodes) %>%
      arrange(desc(a_Included))
    
    b_index <- bob_ross_elememt %>%
      filter(Season == input$Compare,
             Included == 1) %>%
      group_by(Element) %>%
      summarise(b_Included = sum(Included)) %>%
      ungroup() %>%
      mutate(Pct = b_Included/b_episodes$B_Episodes) %>%
      arrange(desc(b_Included))
    
    index_join <- full_join(a_index,
                            b_index,
                            by = c('Element' = 'Element')) %>%
      mutate(Index = Pct.y-Pct.x) %>%
      arrange(desc(Index)) %>%
      filter(Index <= -0.01)
    
    ggplot(index_join,
           aes(x = reorder(Element, -Index),
               y = Pct.y)) +
      coord_flip() +
      geom_point(size = 6,
                 color = 'orange') +
      geom_point(mapping = aes(x = reorder(Element, -Index),
                               y = Pct.x),
                 color = 'gray',
                 size = 6) +
      labs(title = paste('Which Elements Appeared in Less Bob Ross Paintings in\nSeason ', input$Compare, ' from Season ', input$Base, '?\n', sep = ''),
           x = 'Element\n',
           y = '\nFrequency in Episodes') +
      geom_text(data = index_join %>% slice(which.min(Index)),
                mapping = aes(x = reorder(Element, Index),
                              y = Pct.x,
                              label = paste('Season ', input$Base, sep = '')),
                vjust = -1,
                size = 6) +
      geom_text(data = index_join %>% slice(which.min(Index)),
                mapping = aes(x = reorder(Element, Index),
                              y = Pct.y,
                              label = paste('Season ', input$Compare, sep = '')),
                vjust = -1,
                size = 6) +
      geom_text(mapping = aes(x = reorder(Element, Index),
                              y = Pct.y,
                              label = scales::percent(Index)),
                vjust = 2,
                size = 4) +
      geom_curve(aes(x = reorder(Element, Index),
                     xend =  reorder(Element, Index),
                     y = Pct.x - 0.005,
                     yend = Pct.y + 0.005),
                 arrow = arrow(length = unit(0.02, "npc"),
                               type = 'open'),
                 curvature = 0.03,
                 size = 1) +
      scale_y_continuous(labels = scales::percent) +
      theme(plot.title = element_text(face = 'bold', size = 24, family = 'Arial'),
            legend.position = 'top',
            legend.background=element_blank(),
            legend.key=element_blank(),
            legend.text = element_text(size = 14, family = 'Arial'),
            legend.title = element_text(size = 14, family = 'Arial'),
            plot.subtitle = element_text(size = 15, family = 'Arial'),
            plot.caption = element_text(size = 14, family = 'Arial'),
            axis.title = element_text(size = 14, family = 'Arial'),
            axis.text = element_text(size = 14, family = 'Arial'),
            strip.text = ggplot2::element_text(size = 14, hjust = 0, face = 'bold', color = 'black', family = 'Arial'),
            strip.background = element_rect(fill = NA),
            panel.background = ggplot2::element_blank(),
            axis.line = element_line(colour = "#222222", linetype = "solid"),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.major.x = element_line(colour = "#c1c1c1", linetype = "dashed")) 
    
    
    
  }, height = 800, width = 'auto')

})

shinyApp(ui=ui, server=server)