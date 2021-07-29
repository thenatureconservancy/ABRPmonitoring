

# pine heatmap effort


pacman::p_load(tidyverse, ggsci, ggtext, readr, dplyr, DT, crosstalk, plotly, ggforce, RColorBrewer)
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

# clean up.  I need to keep MONYEAR.x, SIZE.x, BP and difference columns

comparePineClean <- subset(comparePine, select = c(MONYEAR.x, SIZE.x, BP, difference))


# try heatmap with this tutorial https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/

# will start with size = A

comparePineCleanA <- subset(comparePineClean, SIZE.x == "A")

view (comparePineCleanA)

# try the plot for seedling size A


ggplot(data = comparePineCleanA, aes(x = factor(MONYEAR.x), y = BP, fill = difference)) +
  geom_tile(colour="white",size=0.2)+
  scale_x_discrete(limits=c("1995", "2019"),
                  breaks=c("1995","2000","2005","2010","2015","2020"))+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=10,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))





