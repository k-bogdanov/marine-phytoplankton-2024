library(ggplot2)

My_Theme = theme_bw() +
  theme(axis.title.x = element_text(size=18),
        axis.text.x = element_text(angle=0, colour = "black", vjust=1, hjust = 0.5, size=18), 
        axis.text.y = element_text(colour = "black", size=18),
        axis.title.y = element_text(size=18),
        plot.title = element_text(size = 18),
        legend.title =element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position="right",
        legend.key.size = unit(1, "cm"),
        strip.text.x = element_text(size=18),
        strip.text.y = element_text(size=18),
        #panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(colour="black"))


#Culture_type colours
Cond.col <- c("None" = "black",
              "Coccolithophore" = "#D83706FF",
              "Diatom" = "#117733")

Cond.sha = c("BacterialControl" = 1, #Hollow circle
             "CoccolithophoreBacteria" = 1, #Hollow circle
             "CoccolithophoreOnly" = 16, #Filled circle
             "DiatomBacteria" = 1, #Hollow circle
             "DiatomOnly" = 16) #Filled circle

BactStat.sha = c("Present" = 16, #Filled circle 
                 "Absent" = 1) #Hollow circle