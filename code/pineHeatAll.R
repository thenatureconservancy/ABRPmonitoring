

# try to do all da heatmaps except seedlings

# do all but seedlings with comparePineClean.  Seedlings range is too great-will have to redo data for that.



# libraries
pacman::p_load(tidyverse, dplyr, RColorBrewer)


# read in data
library(readr)
fiveStems <- read_csv("data/fiveStems.csv")

pineDiverge <- read_csv("data/pineDiverge.csv")


# make new DF by merging

comparePine <- merge(fiveStems, pineDiverge, by.x = "FIVE_SIZE_MONYEAR", by.y = "SIZE_MONYEAR")

#almost, but only kept fiveStems years...want to keep all

comparePine <- merge(fiveStems, pineDiverge, by.x = "FIVE_SIZE_MONYEAR", by.y = "SIZE_MONYEAR", all.y = TRUE)

# add in "difference" column

comparePine$difference <-comparePine$AVE_STEMS - comparePine$STEMS

# clean up.  I need to keep MONYEAR.x, SIZE.x, BP and difference columns

comparePineClean <- subset(comparePine, select = c(MONYEAR.x, SIZE.x, BP, difference))

comparePineClean$MONYEAR.x <- factor(comparePineClean$MONYEAR.x, levels = 1995:2020)

# try to fix difference column to make it easier to chart

comparePineClean <- comparePineClean %>%
  mutate(diffFactor = cut(difference, breaks = c(-510, -120, -100, -80, -60, -40, -20, 0, 20, 40, 60),
                          labels = c( "< -120", "-120 - -100", "-100 - -80", "-80 - -60", "-60 - -40", "-40 - -20", "-20 - 0", "0 - 20", "20 - 40", "40 - 60")))


###  set up heatmap


heatmap <-
ggplot(comparePineClean,aes(x=MONYEAR.x,y=BP,fill=diffFactor))+
  geom_tile(colour="white",size=0.2)+
  scale_x_discrete(breaks=c("1995","2000","2005","2010","2015","2020"),
                   drop = F) +
  guides(fill=guide_legend(title="Difference in stem count\ncompared to mean of BU 5. \nNote negatives."))+
  labs(x="Year",
       y="Burn Unit_Plot Number",
       title="Difference between mean pine stem counts for BU 5, and individual BU-Plot combinations.",
       subtitle = "Size Class XXXXXXXXXX", ### MAKE SURE TO CHANGE THIS AS NEEDED
       caption = "Plot 5 is considered reference.  White indicates no data.") +
  scale_fill_manual(values=c("#450c13", # PLUM < -120
                             "#8c1c29", # MAROON -120 - -100
                             "#d53e4f", # SALMON -100 - -80
                             "#f46d43", # ORANGE -80 - -60
                             "#fdae61", # LIGHT ORANGE -60 - -40
                             "#fee08b", # YELLOW -40 - -20
                             "#e6f598", # LIGHT GREEN/YELLOW -20 - 0
                             "#ddf1da", # LIGHT GREEN  0-20
                             "#abdda4", # MEDIUM GREEN 20-40
                             "#5a8a53"  # DARK GREEN 40-60
                             ),
                              na.value = "grey90")+
  theme(panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill = NA, color = "black"),
         legend.position="right",
        strip.text = element_text(face="bold", size=9),
        axis.text=element_text(face="bold"),
        axis.title = element_text(face="bold"),
        plot.title = element_text(face = "bold", size=13),
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        plot.caption.position =  "plot")

#########


heatmapA <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("A"))
heatmapA
ggsave(heatmapA, filename="heatmapA.png",width=10,height=8,units='in',dpi=300)

heatmapB <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("B"))
heatmapB
ggsave(heatmapB, filename="heatmapB.png",width=10,height=8,units='in',dpi=300)

heatmapC <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("C"))
heatmapC
ggsave(heatmapC, filename="heatmapC.png",width=10,height=8,units='in',dpi=300)

heatmapD <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("D"))
heatmapD
ggsave(heatmapD, filename="heatmapD.png",width=10,height=8,units='in',dpi=300)

heatmapE <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("E"))
heatmapE
ggsave(heatmapE, filename="heatmapE.png",width=10,height=8,units='in',dpi=300)

heatmapF <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("F"))
heatmapF
ggsave(heatmapF, filename="heatmapF.png",width=10,height=8,units='in',dpi=300)

heatmapG <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("G"))
heatmapG
ggsave(heatmapG, filename="heatmapG.png",width=10,height=8,units='in',dpi=300)

heatmapH <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("H"))
heatmapH
ggsave(heatmapH, filename="heatmapH.png",width=10,height=8,units='in',dpi=300)

heatmapI <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("I"))
heatmapI
ggsave(heatmapI, filename="heatmapI.png",width=10,height=8,units='in',dpi=300)

heatmapJ <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("J"))
heatmapJ
ggsave(heatmapJ, filename="heatmapJ.png",width=10,height=8,units='in',dpi=300)

heatmapSeedlings <- heatmap %+% subset(comparePineClean, SIZE.x %in% c("SEEDLING"))
heatmapSeedlings
ggsave(heatmapSeedlings, filename="heatmapSeedlings.png",width=10,height=8,units='in',dpi=300)





















