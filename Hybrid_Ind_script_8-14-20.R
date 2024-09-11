#Hybrid Ind Script#

#Load required packages
library(tidyverse)
library(lme4)
library(cowplot)
library(lmerTest)
library(agricolae)
library(MASS)
library(emmeans)
library(ggpubr)
library(lubridate)
library(MuMIn)

datum = read.csv("hybrid_ind_data_R.csv", na.strings = "NA")
str(datum)

#Make new column with asbolute value of O2 consumption (mmol O2 consumed per min)
datum$Oxygen_size_abs = abs(datum$Oxygen_size)

#Subset for data frame to get just the rows with astaxanthin and remove individuals that died during data collection
datum %>%
  filter(Mortality != "Died", Carot == "Astaxanthin") -> datum.asta

#Subset data fram to remove one outlier 
datum.asta <- datum.asta[-c(16),]

###Plotting -------------------------------
mod.2 <- lmer(Conc_ng_mm~Oxygen_size_abs + (1|O2_plate_ID), data =subset(datum.asta, Cross == "SDxBB"))
summary(mod.2)
r.squaredGLMM(mod.2)

mod.3 <- lmer(Conc_ng_mm~Oxygen_size_abs + (1|O2_plate_ID), data =subset(datum.asta, Cross == "BBxSD"))
summary(mod.3)
r.squaredGLMM(mod.3)

#Asta by O2 consumption
tiff(file = "Asta by O2 consumption.tif", units = "in", width = 5.5, height = 5, res = 500)
datum.asta%>%
  ggplot(aes(x=Oxygen_size_abs, y = Conc_ng_mm, col = Cross))+
  geom_smooth(method="lm", aes(fill = Cross), alpha = 0.2)+
  geom_point(size = 4) +
  scale_color_manual(values = c("deepskyblue1", "gray35"), name = "")+
  scale_fill_manual(values = c("deepskyblue1", "gray35"), name = "") +
  theme_cowplot()+
  ylab("Astaxanthin \n(ng mm-1)") +
  xlab(bquote('Respiration rate (mmol O2'~min^-1~'mm'^-1*')')) +
  theme(legend.position = "none", legend.justification = "left")+
  annotate("text", x = 0.25, y = 20, label = bquote('SDxBB: p=0.016, R'^2*'=0.27, n=58'), 
                                                        size = 4, color ="deepskyblue2")+
  annotate("text", x = 0.25, y = 18, label = bquote('BBxSD: p=0.004, R'^2*'=0.34, n=59'), 
                                                        size = 4, color ="gray35")
dev.off()

#Jpeg for low file size
jpeg(file = "Asta by O2 consumption.jpg", units = "in", width = 5.5, height = 5, res = 500)
datum.asta%>%
  ggplot(aes(x=Oxygen_size_abs, y = Conc_ng_mm, col = Cross))+
  geom_smooth(method="lm", aes(fill = Cross), alpha = 0.2)+
  geom_point(size = 4) +
  scale_color_manual(values = c("deepskyblue1", "gray35"), name = "")+
  scale_fill_manual(values = c("deepskyblue1", "gray35"), name = "") +
  theme_cowplot()+
  ylab("Astaxanthin \n(ng mm-1)") +
  xlab(bquote('Respiration rate (mmol O2'~min^-1~'mm'^-1*')')) +
  theme(legend.position = "none", legend.justification = "left")+
  annotate("text", x = 0.25, y = 20, label = bquote('SDxBB: p=0.016, R'^2*'=0.27, n=58'), 
           size = 4, color ="deepskyblue2")+
  annotate("text", x = 0.25, y = 18, label = bquote('BBxSD: p=0.004, R'^2*'=0.34, n=59'), 
           size = 4, color ="gray35")
dev.off()
