##Import data

library(plyr)
library(dplyr)
library(stringr)


#Import dataframe
Nutrients.df = read.table(file = "/Nutrients_02.11.21.csv", header = T, row.names = 2, sep = ",")

Nutrients.df = Nutrients.df %>% 
  mutate(BacterialStatus = if_else(str_detect(Culture_type, "Bacteria"), "Present", "Absent"))



#Nutrient patterns
##Line plots - by time


library(tidyr)
library(Rmisc)

#Convert data from wide to long for easier plotting
SiO2_long.df = gather(Nutrients.df, SiO2, Value, c("SiO2_ug.L"), factor_key = F)

#Convert from character to numeric
SiO2_long.df$Value = as.numeric(as.character(SiO2_long.df$Value))
SiO2_long.df$HoursFromStart = as.numeric(as.character(SiO2_long.df$HoursFromStart))

#Summarise data
SiO2_sum.df = summarySE(SiO2_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

SiO2_sum.df$Cultivation_Stage = factor(SiO2_sum.df$Cultivation_Stage,
                                       levels = c("Lag",
                                                  "Exponential",
                                                  "Stationary",
                                                  "Death"))


#Plot
SiO2_line.plot = ggplot(SiO2_sum.df, aes(x = HoursFromStart, 
                                         y = Value, 
                                         colour = Phyto, 
                                         shape = BacterialStatus,
                                         group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  # scale_color_manual("Phytoplankton",values = Cond.col)+
  scale_color_manual(values = cbbPalette)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("SiO2 \n(ug per L)")+
  xlab("Time (hours)")+
  theme_bw()+
  My_Theme

#Visualise plot
SiO2_line.plot


#Convert data from wide to long for easier plotting
NOx_long.df = gather(Nutrients.df, NOx, Value, c("NOx_ug.L"), factor_key = F)

#Convert from character to numeric
NOx_long.df$Value = as.numeric(as.character(NOx_long.df$Value))
NOx_long.df$HoursFromStart = as.numeric(as.character(NOx_long.df$HoursFromStart))

#Summarise data
NOx_sum.df = summarySE(NOx_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

NOx_sum.df$Cultivation_Stage = factor(NOx_sum.df$Cultivation_Stage,
                                      levels = c("Lag",
                                                 "Exponential",
                                                 "Stationary",
                                                 "Death"))


#Plot
NOx_line.plot = ggplot(NOx_sum.df, aes(x = HoursFromStart, 
                                       y = Value, 
                                       colour = Phyto, 
                                       shape = BacterialStatus,
                                       group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("NOx \n(ug per L)")+
  xlab("Time (hours)")+
  theme_bw()+
  My_Theme

#Visualise plot
NOx_line.plot


#Convert data from wide to long for easier plotting
Chla_long.df = gather(Nutrients.df, Chla, Value, c("Chla"), factor_key = F)

#Convert from character to numeric
Chla_long.df$Value = as.numeric(as.character(Chla_long.df$Value))
Chla_long.df$HoursFromStart = as.numeric(as.character(Chla_long.df$HoursFromStart))

#Summarise data
Chla_sum.df = summarySE(Chla_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Chla_sum.df$Cultivation_Stage = factor(Chla_sum.df$Cultivation_Stage,
                                       levels = c("Lag",
                                                  "Exponential",
                                                  "Stationary",
                                                  "Death"))


#Plot
Chla_line.plot = ggplot(Chla_sum.df, aes(x = HoursFromStart, 
                                         y = Value, 
                                         colour = Phyto, 
                                         shape = BacterialStatus,
                                         group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Chla \n(ug per L)")+
  xlab("Time (hours)")+
  theme_bw()+
  My_Theme

#Visualise plot
Chla_line.plot


#Convert data from wide to long for easier plotting
Spectrometer_long.df = gather(Nutrients.df, Spectrometer, Value, c("Spectrometer"), factor_key = F)

#Convert from character to numeric
Spectrometer_long.df$Value = as.numeric(as.character(Spectrometer_long.df$Value))
Spectrometer_long.df$HoursFromStart = as.numeric(as.character(Spectrometer_long.df$HoursFromStart))

#Summarise data
Spectrometer_sum.df = summarySE(Spectrometer_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Spectrometer_sum.df$Cultivation_Stage = factor(Spectrometer_sum.df$Cultivation_Stage,
                                               levels = c("Lag",
                                                          "Exponential",
                                                          "Stationary",
                                                          "Death"))


#Plot
Spectrometer_line.plot = ggplot(Spectrometer_sum.df, aes(x = HoursFromStart, 
                                                         y = Value, 
                                                         colour = Phyto, 
                                                         shape = BacterialStatus,
                                                         group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Fluorometer \n(ug per L)")+
  xlab("Time (hours)")+
  theme_bw()+
  My_Theme

#Visualise plot
Spectrometer_line.plot


#Convert data from wide to long for easier plotting
BacterialAbundance_long.df = gather(Nutrients.df, BacterialAbundance, Value, c("BacterialAbundance"), factor_key = F)

#Summarise data
BacterialAbundance_sum.df = summarySE(BacterialAbundance_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

BacterialAbundance_sum.df$Cultivation_Stage = factor(BacterialAbundance_sum.df$Cultivation_Stage,
                                                     levels = c("Lag",
                                                                "Exponential",
                                                                "Stationary",
                                                                "Death"))


#Plot
BacterialAbundance_line.plot = ggplot(BacterialAbundance_sum.df, aes(x = HoursFromStart, 
                                                                     y = Value, 
                                                                     colour = Phyto, 
                                                                     shape = BacterialStatus,
                                                                     group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("BacterialAbundance \n(ug per L)")+
  xlab("Time (hours)")+
  theme_bw()+
  My_Theme

#Visualise plot
BacterialAbundance_line.plot


#Convert data from wide to long for easier plotting
Orthophosphate_long.df = gather(Nutrients.df, Orthophosphate, Value, c("Orthophosphate_ug.L"), factor_key = F)

#Convert from character to numeric
Orthophosphate_long.df$Value = as.numeric(as.character(Orthophosphate_long.df$Value))
Orthophosphate_long.df$HoursFromStart = as.numeric(as.character(Orthophosphate_long.df$HoursFromStart))

#Summarise data
Orthophosphate_sum.df = summarySE(Orthophosphate_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Orthophosphate_sum.df$Cultivation_Stage = factor(Orthophosphate_sum.df$Cultivation_Stage,
                                                 levels = c("Lag",
                                                            "Exponential",
                                                            "Stationary",
                                                            "Death"))


#Plot
Orthophosphate_line.plot = ggplot(Orthophosphate_sum.df, aes(x = HoursFromStart, 
                                                             y = Value, 
                                                             colour = Phyto, 
                                                             shape = BacterialStatus,
                                                             group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Orthophosphate \n(ug per L)")+
  xlab("Time (hours)")+
  theme_bw()+
  My_Theme

#Visualise plot
Orthophosphate_line.plot


#Convert data from wide to long for easier plotting
Ammonia_long.df = gather(Nutrients.df, Ammonia, Value, c("Ammonia_ug.L"), factor_key = F)

#Convert from character to numeric
Ammonia_long.df$Value = as.numeric(as.character(Ammonia_long.df$Value))
Ammonia_long.df$HoursFromStart = as.numeric(as.character(Ammonia_long.df$HoursFromStart))

#Summarise data
Ammonia_sum.df = summarySE(Ammonia_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Ammonia_sum.df$Cultivation_Stage = factor(Ammonia_sum.df$Cultivation_Stage,
                                          levels = c("Lag",
                                                     "Exponential",
                                                     "Stationary",
                                                     "Death"))


#Plot
Ammonia_line.plot = ggplot(Ammonia_sum.df, aes(x = HoursFromStart, 
                                               y = Value, 
                                               colour = Phyto, 
                                               shape = BacterialStatus,
                                               group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Ammonia \n(ug per L)")+
  xlab("Time (hours)")+
  theme_bw()+
  My_Theme

#Visualise plot
Ammonia_line.plot

```


```{r Combine}

ggpubr::ggarrange(Chla_line.plot,
                  BacterialAbundance_line.plot,
                  SiO2_line.plot,
                  Ammonia_line.plot,
                  NOx_line.plot,
                  Orthophosphate_line.plot,
                  Spectrometer_line.plot,
                  ncol = 2,
                  nrow = 4,
                  common.legend = T,
                  legend = "right")

#tiff("/Volumes/micro-shared$/MoralesLab/Manuscripts/Sven_PhytoBact_CultExp/temp_plots/SomeMetadata.tiff", width = 15, height = 15, units = "in", res = 300)
#last_plot()
#dev.off()




#Convert data from wide to long for easier plotting
SiO2_long.df = gather(Nutrients.df, SiO2, Value, c("SiO2_ug.L"), factor_key = F)

#Convert from character to numeric
SiO2_long.df$Value = as.numeric(as.character(SiO2_long.df$Value))
SiO2_long.df$HoursFromStart = as.numeric(as.character(SiO2_long.df$HoursFromStart))

#Summarise data
SiO2_sum.df = summarySE(SiO2_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

SiO2_sum.df$Cultivation_Stage = factor(SiO2_sum.df$Cultivation_Stage,
                                       levels = c("Lag",
                                                  "Exponential",
                                                  "Stationary",
                                                  "Death"))


#Plot
SiO2_line.plot = ggplot(SiO2_sum.df, aes(x = Cultivation_Stage, 
                                         y = Value, 
                                         colour = Phyto, 
                                         shape = BacterialStatus,
                                         group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("SiO2 \n(ug per L)")+
  xlab("")+
  theme_bw()+
  My_Theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualise plot
SiO2_line.plot


#Convert data from wide to long for easier plotting
NOx_long.df = gather(Nutrients.df, NOx, Value, c("NOx_ug.L"), factor_key = F)

#Convert from character to numeric
NOx_long.df$Value = as.numeric(as.character(NOx_long.df$Value))
NOx_long.df$HoursFromStart = as.numeric(as.character(NOx_long.df$HoursFromStart))

#Summarise data
NOx_sum.df = summarySE(NOx_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

NOx_sum.df$Cultivation_Stage = factor(NOx_sum.df$Cultivation_Stage,
                                      levels = c("Lag",
                                                 "Exponential",
                                                 "Stationary",
                                                 "Death"))


#Plot
NOx_line.plot = ggplot(NOx_sum.df, aes(x = Cultivation_Stage, 
                                       y = Value, 
                                       colour = Phyto, 
                                       shape = BacterialStatus,
                                       group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("NOx \n(ug per L)")+
  xlab("")+
  theme_bw()+
  My_Theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualise plot


#Convert data from wide to long for easier plotting
Chla_long.df = gather(Nutrients.df, Chla, Value, c("Chla"), factor_key = F)

#Convert from character to numeric
Chla_long.df$Value = as.numeric(as.character(Chla_long.df$Value))
Chla_long.df$HoursFromStart = as.numeric(as.character(Chla_long.df$HoursFromStart))

#Summarise data
Chla_sum.df = summarySE(Chla_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Chla_sum.df$Cultivation_Stage = factor(Chla_sum.df$Cultivation_Stage,
                                       levels = c("Lag",
                                                  "Exponential",
                                                  "Stationary",
                                                  "Death"))


#Plot
Chla_line.plot = ggplot(Chla_sum.df, aes(x = Cultivation_Stage, 
                                         y = Value, 
                                         colour = Phyto, 
                                         shape = BacterialStatus,
                                         group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Chla \n(ug per L)")+
  xlab("")+
  theme_bw()+
  My_Theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualise plot
Chla_line.plot



#Convert data from wide to long for easier plotting
Spectrometer_long.df = gather(Nutrients.df, Spectrometer, Value, c("Spectrometer"), factor_key = F)

#Convert from character to numeric
Spectrometer_long.df$Value = as.numeric(as.character(Spectrometer_long.df$Value))
Spectrometer_long.df$HoursFromStart = as.numeric(as.character(Spectrometer_long.df$HoursFromStart))

#Summarise data
Spectrometer_sum.df = summarySE(Spectrometer_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Spectrometer_sum.df$Cultivation_Stage = factor(Spectrometer_sum.df$Cultivation_Stage,
                                               levels = c("Lag",
                                                          "Exponential",
                                                          "Stationary",
                                                          "Death"))


#Plot
Spectrometer_line.plot = ggplot(Spectrometer_sum.df, aes(x = Cultivation_Stage, 
                                                         y = Value, 
                                                         colour = Phyto, 
                                                         shape = BacterialStatus,
                                                         group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Fluorometer \n(ug per L)")+
  xlab("")+
  theme_bw()+
  My_Theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualise plot
Spectrometer_line.plot


#Convert data from wide to long for easier plotting
BacterialAbundance_long.df = gather(Nutrients.df, BacterialAbundance, Value, c("BacterialAbundance"), factor_key = F)

#Convert from character to numeric
BacterialAbundance_long.df$Value = as.numeric(as.character(BacterialAbundance_long.df$Value))
BacterialAbundance_long.df$HoursFromStart = as.numeric(as.character(BacterialAbundance_long.df$HoursFromStart))

#Summarise data
BacterialAbundance_sum.df = summarySE(BacterialAbundance_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

BacterialAbundance_sum.df$Cultivation_Stage = factor(BacterialAbundance_sum.df$Cultivation_Stage,
                                                     levels = c("Lag",
                                                                "Exponential",
                                                                "Stationary",
                                                                "Death"))


#Plot
BacterialAbundance_line.plot = ggplot(BacterialAbundance_sum.df, aes(x = Cultivation_Stage, 
                                                                     y = Value, 
                                                                     colour = Phyto, 
                                                                     shape = BacterialStatus,
                                                                     group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Bacterial Abundance \n(ug per L)")+
  xlab("")+
  theme_bw()+
  My_Theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualise plot
BacterialAbundance_line.plot

#Convert data from wide to long for easier plotting
Orthophosphate_long.df = gather(Nutrients.df, Orthophosphate, Value, c("Orthophosphate_ug.L"), factor_key = F)

#Convert from character to numeric
Orthophosphate_long.df$Value = as.numeric(as.character(Orthophosphate_long.df$Value))
Orthophosphate_long.df$HoursFromStart = as.numeric(as.character(Orthophosphate_long.df$HoursFromStart))

#Summarise data
Orthophosphate_sum.df = summarySE(Orthophosphate_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Orthophosphate_sum.df$Cultivation_Stage = factor(Orthophosphate_sum.df$Cultivation_Stage,
                                                 levels = c("Lag",
                                                            "Exponential",
                                                            "Stationary",
                                                            "Death"))


#Plot
Orthophosphate_line.plot = ggplot(Orthophosphate_sum.df, aes(x = Cultivation_Stage, 
                                                             y = Value, 
                                                             colour = Phyto, 
                                                             shape = BacterialStatus,
                                                             group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Orthophosphate \n(ug per L)")+
  xlab("")+
  theme_bw()+
  My_Theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualise plot
Orthophosphate_line.plot


#Convert data from wide to long for easier plotting
Ammonia_long.df = gather(Nutrients.df, Ammonia, Value, c("Ammonia_ug.L"), factor_key = F)

#Convert from character to numeric
Ammonia_long.df$Value = as.numeric(as.character(Ammonia_long.df$Value))
Ammonia_long.df$HoursFromStart = as.numeric(as.character(Ammonia_long.df$HoursFromStart))

#Summarise data
Ammonia_sum.df = summarySE(Ammonia_long.df, measurevar = "Value", groupvars = c("Culture_type", "Phyto", "HoursFromStart", "Cultivation_Stage", "BacterialStatus"))

Ammonia_sum.df$Cultivation_Stage = factor(Ammonia_sum.df$Cultivation_Stage,
                                          levels = c("Lag",
                                                     "Exponential",
                                                     "Stationary",
                                                     "Death"))


#Plot
Ammonia_line.plot = ggplot(Ammonia_sum.df, aes(x = Cultivation_Stage, 
                                               y = Value, 
                                               colour = Phyto, 
                                               shape = BacterialStatus,
                                               group = Culture_type))+
  geom_point(size=3)+
  geom_line()+
  geom_errorbar(aes(ymin=Value-sd, 
                    ymax=Value+sd), 
                width=0.2) +
  # scale_y_log10()+
  scale_color_manual("Phytoplankton", values = Cond.col)+
  scale_shape_manual("Microbiome", values = BactStat.sha)+
  ylab("Ammonia \n(ug per L)")+
  xlab("")+
  theme_bw()+
  My_Theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualise plot
Ammonia_line.plot


Combined_plot_stage = ggpubr::ggarrange(Chla_line.plot,
                                        BacterialAbundance_line.plot,
                                        SiO2_line.plot,
                                        Ammonia_line.plot,
                                        NOx_line.plot,
                                        Orthophosphate_line.plot,
                                        Spectrometer_line.plot,
                                        ncol = 2,
                                        nrow = 4,
                                        common.legend = T,
                                        legend = "right")

#tiff("C:/Users/hunefeldt/Dropbox/PhytoBact_CultivationExperiment/Analysis/R_analysis/Phytoplankton_NZ/SomeMetadata_Stage.tiff", width = 15, height = 15, units = "in", res = 300)
#Combined_plot_stage
#dev.off()




##Statistics

library(RVAideMemoire)
##SIgnificant differences between Culture_types i.t.o. nutrients
```{r}
#Change from german to english punctuation
Nutrients.df$SiO2 = gsub(",", ".", Nutrients.df$SiO2)
Nutrients.df$NOx = gsub(",", ".", Nutrients.df$NOx)
Nutrients.df$Chla = gsub(",", ".", Nutrients.df$Chla)
Nutrients.df$Spectrometer = gsub(",", ".", Nutrients.df$Spectrometer)
Nutrients.df$BacterialAbundance = gsub(",", ".", Nutrients.df$BacterialAbundance)
Nutrients.df$HoursFromStart = gsub(",", ".", Nutrients.df$HoursFromStart)

#Convert from character to numeric
Nutrients.df$SiO2 = as.numeric(as.character(Nutrients.df$SiO2))
Nutrients.df$NOx = as.numeric(as.character(Nutrients.df$NOx))
Nutrients.df$Chla = as.numeric(as.character(Nutrients.df$Chla))
Nutrients.df$Spectrometer = as.numeric(as.character(Nutrients.df$Spectrometer))
Nutrients.df$BacterialAbundance = as.numeric(as.character(Nutrients.df$BacterialAbundance))
Nutrients.df$HoursFromStart = as.numeric(as.character(Nutrients.df$HoursFromStart))




#Calculate significance

summary(aov(SiO2_ug.L ~ Culture_type*HoursFromStart, data = Nutrients.df)) #Both + interaction
summary(aov(NOx_ug.L ~ Culture_type*HoursFromStart, data = Nutrients.df)) # Hours
summary(aov(Chla ~ Culture_type*HoursFromStart, data = Nutrients.df)) # Hours
summary(aov(Spectrometer ~ Culture_type*HoursFromStart, data = Nutrients.df)) # Hours
summary(aov(BacterialAbundance ~ Culture_type*HoursFromStart, data = Nutrients.df)) # Hours  + interaction
summary(aov(Orthophosphate_ug.L ~ Culture_type*HoursFromStart, data = Nutrients.df)) # Hours
summary(aov(Ammonia_ug.L ~ Culture_type*HoursFromStart, data = Nutrients.df)) # interaction
#When experimental stage is added
summary(aov(SiO2_ug.L ~ Culture_type*HoursFromStart*Cultivation_Stage, data = Nutrients.df)) #All + interactions
summary(aov(NOx_ug.L ~ Culture_type*HoursFromStart*Cultivation_Stage, data = Nutrients.df)) # All + interactions
summary(aov(Chla ~ Culture_type*HoursFromStart*Cultivation_Stage, data = Nutrients.df)) #All
summary(aov(Spectrometer ~ Culture_type*HoursFromStart*Cultivation_Stage, data = Nutrients.df)) # All
summary(aov(BacterialAbundance ~ Culture_type*HoursFromStart*Cultivation_Stage, data = Nutrients.df)) # All + interactions
summary(aov(Orthophosphate_ug.L ~ Culture_type*HoursFromStart*Cultivation_Stage, data = Nutrients.df)) # All + interactions
summary(aov(Ammonia_ug.L ~ Culture_type*HoursFromStart*Cultivation_Stage, data = Nutrients.df)) # Culture_type:Hours interaction
```

##Pairwise comparisons - need to split by both Culture_type and experimental phase for best analysis
```{r}
####SiO2 Pairwise PERMANOVA####

#SiO2_ug.L
Group_SiO2_ug.L = get_variable(Nutrients.df, "Culture_type")
df.dist = dist(Nutrients.df$SiO2_ug.L)
ado = pairwise.perm.manova(df.dist, Group_SiO2_ug.L, nperm = 999, p.method = "bonferroni")
ado
#Coccolithophore and bacteria is different compared to everything else

Group_time = get_variable(Nutrients.df, "HoursFromStart")
df.dist = dist(Nutrients.df$SiO2_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#This would first have to be split by Culture_type for it to make sense to do use this stats test

Group_time = get_variable(Nutrients.df, "Cultivation_Stage")
df.dist = dist(Nutrients.df$SiO2_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Lag is different from everything else



####NOx_ug.L Pairwise PERMANOVA####

#NOx_ug.L
Group_NOx_ug.L = get_variable(Nutrients.df, "Culture_type")
df.dist = dist(Nutrients.df$NOx_ug.L)
ado = pairwise.perm.manova(df.dist, Group_NOx_ug.L, nperm = 999, p.method = "bonferroni")
ado
#Nothing significant

Group_time = get_variable(Nutrients.df, "HoursFromStart")
df.dist = dist(Nutrients.df$NOx_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#This would first have to be split by Culture_type for it to make sense to do use this stats test

Group_time = get_variable(Nutrients.df, "Cultivation_Stage")
df.dist = dist(Nutrients.df$NOx_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Everything is significantly different to everything else



####Chla Pairwise PERMANOVA####

#Chla
Group_Chla = get_variable(Nutrients.df, "Culture_type")
df.dist = dist(Nutrients.df$Chla)
ado = pairwise.perm.manova(df.dist, Group_Chla, nperm = 999, p.method = "bonferroni")
ado
#Nothing is significantly different

Group_time = get_variable(Nutrients.df, "HoursFromStart")
df.dist = dist(Nutrients.df$Chla)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Only the first time point is significantly different, but this would first have to be split by Culture_type for it to make sense to do use this stats test

Group_time = get_variable(Nutrients.df, "Cultivation_Stage")
df.dist = dist(Nutrients.df$Chla)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Lag is different from everything else



####Spectrometer Pairwise PERMANOVA####

#Spectrometer
Group_Spectrometer = get_variable(Nutrients.df, "Culture_type")
df.dist = dist(Nutrients.df$Spectrometer)
ado = pairwise.perm.manova(df.dist, Group_Spectrometer, nperm = 999, p.method = "bonferroni")
ado
#Nothing is significantly different

Group_time = get_variable(Nutrients.df, "HoursFromStart")
df.dist = dist(Nutrients.df$Spectrometer)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Only the first time point is significantly different, with one 165h to 812.75h, but this would first have to be split by Culture_type for it to make sense to do use this stats test

Group_time = get_variable(Nutrients.df, "Cultivation_Stage")
df.dist = dist(Nutrients.df$Spectrometer)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Only Stationary to death is not significantly different



####BacterialAbundance Pairwise PERMANOVA####

#BacterialAbundance
Group_BacterialAbundance = get_variable(Nutrients.df, "Culture_type")
df.dist = dist(Nutrients.df$BacterialAbundance)
ado = pairwise.perm.manova(df.dist, Group_BacterialAbundance, nperm = 999, p.method = "bonferroni")
ado
#Nothing is significantly different

Group_time = get_variable(Nutrients.df, "HoursFromStart")
df.dist = dist(Nutrients.df$BacterialAbundance)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Seem to be early vs everything else (early = 0 & 165h)

Group_time = get_variable(Nutrients.df, "Cultivation_Stage")
df.dist = dist(Nutrients.df$BacterialAbundance)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Seem to have an early and late group (early = Lag & Exponential)



####Orthophosphate_ug.L Pairwise PERMANOVA####

#Orthophosphate_ug.L
Group_Orthophosphate_ug.L = get_variable(Nutrients.df, "Culture_type")
df.dist = dist(Nutrients.df$Orthophosphate_ug.L)
ado = pairwise.perm.manova(df.dist, Group_Orthophosphate_ug.L, nperm = 999, p.method = "bonferroni")
ado
#Nothing is significantly different

Group_time = get_variable(Nutrients.df, "HoursFromStart")
df.dist = dist(Nutrients.df$Orthophosphate_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#0 is significantly different toi everything else (except 476.75 hours), and 236.75 vs 812.75 hours

Group_time = get_variable(Nutrients.df, "Cultivation_Stage")
df.dist = dist(Nutrients.df$Orthophosphate_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#All are significantly different from each other.



####Ammonia_ug.L Pairwise PERMANOVA####

#Ammonia_ug.L
Group_Ammonia_ug.L = get_variable(Nutrients.df, "Culture_type")
df.dist = dist(Nutrients.df$Ammonia_ug.L)
ado = pairwise.perm.manova(df.dist, Group_Ammonia_ug.L, nperm = 999, p.method = "bonferroni")
ado
#Nothing is significantly different

Group_time = get_variable(Nutrients.df, "HoursFromStart")
df.dist = dist(Nutrients.df$Ammonia_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Nothing is significantly different

Group_time = get_variable(Nutrients.df, "Cultivation_Stage")
df.dist = dist(Nutrients.df$Ammonia_ug.L)
ado = pairwise.perm.manova(df.dist, Group_time, nperm = 999, p.method = "bonferroni")
ado
#Nothing is significantly different

```

