# pine data explore

## One small thing to fix (e-mailed team 6 July 21) with rows per BP

yearObsBP <- pine %>%
  count (MONYEAR, BP)

yearObsBPChart <-
  yearObsBP %>%
  ggplot(aes(x = MONYEAR, y = n, group =  BP, color = as.factor(BP))) +
  geom_line(size=1.0) +
  #geom_point(size =  2.0) +
  # labels
  labs(x = "Year",
       y = "Number of rows",
       title = "Number of observations per year colored by BU-Plot combo",
       caption = "Data from Florida Chapter")  +

  #Adjust the axes
  theme(plot.title = element_text(face="bold", size =14),
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))


yearObsBPChart


