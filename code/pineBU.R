# pine stems by BU and Size

# make a chart for each size, over time, highlight BU5

# load libraries
pacman::p_load(tidyverse, readr, dplyr,  plotly, ggsci, ggforce)
theme_set(theme_bw())

# read data
averagePineStemsBU <- read_csv("data/averagePineStemsBU.csv")  #renamed AVERAGE_STEMS to STEMS
View(averagePineStemsBU)

pineBU5all <- read_csv("data/pineBU5all.csv")
View(pineBU5all)


# faceted chart by size (need to highlight BU 5)

stemsSizeYearBU <-

  ggplot(data = averagePineStemsBU, aes(x= MONYEAR, y= STEMS, group = BU, color = as.factor(BU))) +
  geom_line(size=1.0) +
  geom_point(size =  2.0) +
  geom_smooth(data = pineBU5all, method=lm, size = 2.0, color = "black" ) +
  facet_wrap_paginate(~ SIZE, ncol = 1, nrow = 3, page = 1, scales = "free_y") +


  # My top color palette
  scale_color_simpsons() +

  # Theme
  theme_bw() +


  # Update the labels of the axes
  labs(x = "Year",
       y = "Mean number of stems per year at ABRP",
       title = "Summarizing number of stems across size categories",
       subtitle = "Black line represented smoothed data and 95% confidence interval from BU 5",
       caption = "Data from Florida Chapter",
       color = "Burn Unit")

  #Adjust the axes
  theme(plot.title = element_text(face="bold", size =14),
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))

ggplotly(stemsSizeYearBU)
