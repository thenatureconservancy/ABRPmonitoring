# try plots of divergence from average of BU 5 stems

#load libraries
pacman::p_load(tidyverse, ggsci, ggtext, readr, dplyr, DT, crosstalk, plotly, ggforce)
theme_set(theme_bw())

# read in data
library(readr)
fiveStems <- read_csv("data/fiveStems.csv")
View(fiveStems)

pineDiverge <- read_csv("data/pineDiverge.csv")
View(pineDiverge)

# make new DF by merging

comparePine <- merge(fiveStems, pineDiverge, by.x = "FIVE_SIZE_MONYEAR", by.y = "SIZE_MONYEAR")

#almost, but only kept fiveStems years...want to keep all

comparePine <- merge(fiveStems, pineDiverge, by.x = "FIVE_SIZE_MONYEAR", by.y = "SIZE_MONYEAR", all.y = TRUE)

# add in "difference" column

comparePine$difference <-comparePine$AVE_STEMS - comparePine$STEMS

# try plots



#make shared data for crosstalk
sdPine <- highlight_key(comparePine)



pinePlotStems <-
  ggplot(sdPine, aes(MONYEAR.y, difference, color = SIZE.y, group = BP)) +
  geom_line(size=1.5, position = position_jitter(width = 0.0001, height = 1.0)) +

  geom_point() +
  theme_bw() +
  labs(
    title = "Pine stems per plot-difference from reference",
    subtitle = "Including size class",
    caption = "Data from TNC's Florida Chapter",
    x = "Monitoring Year",
    y = "Number of stems" ) +
  scale_color_simpsons()

bscols(widths = c(5, 12),
       filter_select("BP", "Plot:", sdPine, ~ BP),
       ggplotly(pinePlotStems)
)

## maybe try faceted heatmaps https://rud.is/b/2016/02/14/making-faceted-heatmaps-with-ggplot2/

## another way r-craft heatmaps

https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.r-craft.org%2Fr-news%2Fggplot2-time-series-heatmaps%2F&psig=AOvVaw2EZXGB7-pXXkZPGmFc5vk6&ust=1625796790609000&source=images&cd=vfe&ved=0CAwQ3YkBahcKEwj40vWYs9LxAhUAAAAAHQAAAAAQCQ
