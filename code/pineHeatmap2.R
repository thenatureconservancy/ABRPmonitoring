# pine heatmap 2




pacman::p_load(tidyverse, ggsci, ggtext, readr, dplyr, DT, crosstalk, plotly, ggforce, RColorBrewer)
theme_set(theme_classic())

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

# clean up.  I need to keep MONYEAR.x, SIZE.x, BP and difference columns

comparePineClean <- subset(comparePine, select = c(MONYEAR.x, SIZE.x, BP, difference))

comparePineClean$MONYEAR.x <- factor(comparePineClean$MONYEAR.x, levels = 1995:2020)


# will start with size = A

comparePineCleanA <- subset(comparePineClean, SIZE.x == "A")

# try to fix difference column to make it easier to chart

comparePineCleanA2 <- comparePineCleanA %>%
  mutate(diffFactor = cut(difference, breaks = c(-120, -100, -80, -60, -40, -20, 0, 20),
         labels = c("<-100", "-100 - -80", "-80 - -60", "-60 - -40", "-40 - -20", "-20 - 0", "0 - 20")))


textcol <- "grey40"

ggplot(comparePineCleanA2,aes(x=MONYEAR.x,y=BP,fill=diffFactor))+
  geom_tile(colour="white",size=0.2)+
  scale_x_discrete(breaks=c("1995","2000","2005","2010","2015","2020"),
                   drop = F) +
  guides(fill=guide_legend(title="Difference in stem count\ncompared to mean of BU 5"))+
  labs(x="Year",
       y="Burn Unit_Plot Number",
       title="Difference between mean pine stem counts for BU 5, and individual BU-Plot combinations.",
       subtitle = "Size Class A",
       caption = "Plot 5 is considered reference.  White indicates no data.") +
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#e6f598","#abdda4","#ddf1da"),na.value = "grey90")+
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill = NA, color = "black"),
        #legend.title = element_blank(),
        legend.position="right",
        strip.text = element_text(face="bold", size=9),
        axis.text=element_text(face="bold"),
        axis.title = element_text(face="bold"),
        plot.title = element_text(face = "bold", size=13),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot")









