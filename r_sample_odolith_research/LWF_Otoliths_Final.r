# FINAL CODE FOR LWF OTOLITH RESEARCH 
# 07/24/2020, 01/2021, 02/2022, 04/2022, 07/2022
# CHANGING INTERMEDIATE AGER TO NOVICE B AND NOVICE TO NOVICE A
# Changed Novice A to N1 and Novice B to N2


install.packages("tidyverse", dependencies = TRUE )
library(tidyverse)
install.packages(doBy)
library(doBy)
install.packages("fitdistrplus")
library(fitdistrplus)
install.packages(ggpubr)
library(ggpubr)
install.packages("FSA")
library("FSA")
library(plotrix)
library(magrittr)
library(dplyr)
install.packages("psych")
library("psych")
library(dplyr)
library(data.table)
library(ggplot2)
library(car)
library(emmeans)
library(purrr)
library(magrittr)
library(lme4)
library(stats)
library(tidyr)
install.packages("grafify")


# PERCENT AGREEMENT PLOT 
Percentagreement <-
  ggplot(data = PERCENT_AGREEMENT, aes(x = Age.Difference, y = Percent.Agreement, group = Ager)) +
  geom_line(aes(linetype = Ager), size = 1, col= "black") +
  scale_linetype_manual(values = c ("solid", "twodash", "dotted")) + 
  labs(x = "Age Difference (Yrs)", y = "Percent Agreement (%)") +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.position = c(.75,.25),
    legend.box.background = element_rect(color = "black",size = 1.5 ,fill = "white")
  )

Percentagreement
tiff("Percent Agreement.tiff",height= 7, width= 7, units="in", res = 300)
plot(Percentagreement)
dev.off()

# BLAND ALT PLOT

# 02/06/2020
# Redo BLAND ALT data using "Ages" data to format for a new BLAND ALT plot

###Band Altman plot, which shows that the most inexperienced ager was not good at aging older fish
bandaltplot <- #Make plot
  ggplot(data = BLAND_ALT, aes(x = Mean, y = Diff)) +
  geom_jitter(aes(fill = Ager), shape = 21, width = 1, height = 1) +
  stat_smooth(method = "lm", color = "grey25", se = F) +
  geom_hline(yintercept = mean(BLAND_ALT$Diff), color = "blue", linetype = "longdash") + 
  geom_hline(yintercept = mean(BLAND_ALT$Diff)+1.96*sd(BLAND_ALT$Diff), color = "brown", linetype = "longdash") + 
  geom_hline(yintercept = mean(BLAND_ALT$Diff)+-1.96*sd(BLAND_ALT$Diff), color = "brown", linetype = "longdash") + 
  annotate(geom = "text", x = 25, y = 2,label = paste0("Mean = ", round(mean(BLAND_ALT$Diff), 2)), color = "blue", size = 4) +
  annotate(geom = "text", x = 25, y = 8.3,label = paste0("uLOA = ", round(mean(BLAND_ALT$Diff)+(1.96*sd(BLAND_ALT$Diff)), 2)), color = "brown", size = 4) +
  annotate(geom = "text", x = 25, y = -3.5,label = paste0("lLOA = ", round(mean(BLAND_ALT$Diff)+(-1.96*sd(BLAND_ALT$Diff)), 2)), color = "brown", size = 4) +
  labs(x = "Mean Age of Both Techniques (Yrs)", y = "Mean Estimated Age Difference \n (Thin Section - Crack and Burn) (Yrs)") +
  scale_fill_manual(name = "Ager: ", values = c("grey100", "grey70", "grey20")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.position = "bottom")

bandaltplot #View plot
tiff("BlandAlt.tiff",height= 7, width= 7, units="in", res = 300)
plot(bandaltplot)
dev.off()



# SCATTER PLOT
# Scatter_Plot data combinds the Novice agers into one category to compare between the expert and novice 
scatter <- #Make plot 
  ggplot(data = BLAND_ALT, aes(x = Thin, y = Burn)) +
  geom_jitter(aes(fill = Ager), shape = 21, width = 1, height = 1) +
  scale_fill_manual(name = "Experience Level: ", values = c("grey20", "grey100")) +
  labs(x = "Thin Section Estimated Ages (Yrs)", y = "CracK and Burn Estimated Ages (Yrs)") +
  scale_x_continuous(limits = c(0,30), breaks= c(0,5,10,15,20,25,30)) +
  scale_y_continuous(limits = c(0,30), breaks=c(0,5,10,15,20,25,30)) +
  stat_smooth(method = "lm", color="black") +
  geom_abline(intercept = 0, slope = 1, linetype="longdash", color="grey70") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

scatter
# How to Save a plot for journal publication 
tiff("Scatter.tiff",height= 7, width= 7, units="in", res = 300)
plot(scatter)
dev.off()


# SCATTER WITHOUT REGRESSION LINE 
# DONT KNOW WHY THERE ARE MISSING POINTS
# Override the first scatter design
scatter <- #Make plot 
  ggplot(data = BLAND_ALT, aes(x = Thin, y = Burn)) +
  geom_jitter(aes(fill = Ager), shape = 21, width = 1, height = 1) +
  scale_fill_manual(name = "Experience Level ", values = c("grey20", "grey100")) +
  labs(x = "Thin Section Estimated Ages (Yrs)", y = "CracK and Burn Estimated Ages (Yrs)") +
  scale_x_continuous(limits = c(0,30), breaks= c(0,5,10,15,20,25,30)) +
  scale_y_continuous(limits = c(0,30), breaks=c(0,5,10,15,20,25,30)) +
  geom_abline(intercept = 0, slope = 1, linetype="longdash", color="grey70") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

scatter
tiff("Scatter09242022.tiff",height= 6, width= 6, units="in", res = 600)
plot(scatter)
dev.off()


# NEW BOX PLOTS JITTER BOX PLOTS

## Experience level as X and fill as technique
## 6 differetn box plots, but black data points 
## Box plot can help compare the different experience levels
## The data points lines (mean) can show differences within a technique 
## Seed set to 23
ggplot(data = Ages,aes(x = Experience.Level, y = Age, fill = Technique)) +
        geom_boxplot(show.legend = T, outlier.shape = NA, alpha = 0.5, width=0.75, coef=0) +
        geom_jitter(position = position_jitterdodge(
          jitter.width = 0.4,
          jitter.height = 0.5,
          dodge.width = 0.8,
          seed = 23)) +
        stat_summary(fun=mean, geom = "crossbar", position = position_dodge()) +
        labs(x = "Experience Level", y = "Age Estimate (Yrs)") +
        scale_y_continuous(breaks = c(5,10,15,20,25,30)) +
        theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA) 
        
        )


## SUBMITTED IN MANUSCRIPT FOR EDITS
## Experience level as X and fill as technique
## 6 differetn box plots, but black data points
jitter <-
ggplot(data = Ages,aes(x = Experience.Level, y = Age, fill = Technique)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = NA, alpha = 0.15, width=0.75, coef=0, aes(color = Technique)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.4,
                                              jitter.height = 0.5,
                                              dodge.width = 0.8,
                                              seed = 23),
                                          pch=21) +
  stat_summary(fun=mean, geom = "crossbar", position = position_dodge(width = 0.75), width = 0.75) +
  labs(x = "Experience Level", y = "Estimated Age (Yrs)") +
  scale_y_continuous(breaks = c(5,10,15,20,25,30)) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA) 
    
  )

jitter
tiff("JitterBox09242022.tiff",height= 6, width= 6, units="in", res = 600)
plot(jitter)
dev.off()






## Jitter Pot Submitted 
## Experience level as X and fill as technique
## The data points lines (mean) can show differences within a technique 
## set.seed = 23
Age_Jitter <-
ggplot(data = Ages,aes(x = Experience.Level, y = Age, color = Technique)) +
    geom_jitter(position = position_jitterdodge(
                jitter.width = 0.4,
                jitter.height = 0.5,
                dodge.width = 0.8,
                seed = 23)) +
  stat_summary(fun=mean, geom = "crossbar", position = position_dodge(width = 0.85), 
               width = 0.5, size = 0.5, pch = 21, stroke = 0, show.legend = FALSE) +
  labs(x = "Experience Level", y = "Age Estimate (Yrs)") +
  scale_y_continuous(breaks = c(5,10,15,20,25,30)) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA) 
    
  )

Age_Jitter
tiff("AgeJitterPlot.tiff",height= 7, width= 7, units="in", res = 300)
plot(Age_Jitter)
dev.off()


ggsave(filename = "AgeJitterPlot.tiff", plot = Age_Jitter,height= 7, width= 7)




## Not Working
## Technique as X and fill as experience level 
ggplot(data = Ages,aes(x = Technique, y = Age, color= Experience.Level)) +
  geom_jitter(show.legend = T, position = position_jitterdodge(
                                                                jitter.width = NULL,
                                                                jitter.height = 0,
                                                                dodge.width = 0.75,
                                                                seed = NA) +
  width = 0.25, shape = 21, color = "black") +
  labs(x = "Technique", y = "Age Estimate (Yrs)") +
  scale_y_continuous(breaks = c(5,10,15,20,25,30)) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA) 
    
  )

## Not Working
## Technique as X and no fill. Does not capture experience level
ggplot(data = Ages,aes(x = Technique, y = Age)) +
  geom_boxplot(show.legend = T, outlier.shape = NA, alpha = 0.5, width=0.75, coef=0) +
  geom_jitter(show.legend = T, width = 0.25, shape = 21, color = "black") +
  labs(x = "Technique", y = "Age Estimate (Yrs)") +
  scale_y_continuous(breaks = c(5,10,15,20,25,30)) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA) 
    
  )


## VIOLIN PLOT
## Violin plot that capture ths shape of the distributions
ggplot(data = Ages, aes(x = Technique, y = Age, fill=Experience.Level)) +
  geom_violin() +
  geom_boxplot(show.legend = T, outlier.shape = NA, alpha = 0.5, width=0.5, coef=0) +
  labs(x = "Technique", y = "Age Estimate (Yrs)") +
  scale_y_continuous(breaks = c(5,10,15,20,25,30)) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA) 
    
  )



# AGE PLOTS
age_boxplot <- #Make a boxplot of the age estimates
  ggplot(data = Ages, aes(x = Experience.Level, y = Age, fill = Technique)) +
  geom_boxplot(show.legend = T) +
  labs(x = "Experience Level", y = "Age Estimate (Yrs)") +
  scale_y_continuous(breaks = c(5,10,15,20,25,30)) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

age_boxplot
tiff("AgeBoxplot1.tiff",height= 7, width= 7, units="in", res = 300)
plot(age_boxplot)
dev.off()




#AGE BIAS PLOTS 01/21/2021

# EXPERT AGER VS MEAN AGE (THIN)
ab.ET <- ageBias(Thin_Expert~Thin_Mean, data=Mean.Ages, ref.lab = "Mean Ages for Thin Section", nref.lab = "Expert Thin Section Ages")
summary(ab.ET, what = "bias")
plotAB(ab.ET,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))

tiff("ab.ET.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.ET,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))
dev.off()

# Playing with the scale, still not right
scale(axis(1, at = seq(0,30, by = 5), las=2))


# EXPERT AGER VS MEAN AGE (BURN)
ab.EB <- ageBias(Burn_Expert~Burn_Mean, data=Mean.Ages, ref.lab = "Mean Ages for Crack and Burn", nref.lab = "Expert Crack and Burn Ages")
summary(ab.EB, what = "bias")
plotAB(ab.EB,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))


tiff("ab.EB.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.EB,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))
dev.off()


# EXPERT THIN VS EXPERT BURN 
ab.EBT <- ageBias(Burn_Expert~Thin_Expert, data=Mean.Ages, ref.lab = "Expert Thin Section Ages", nref.lab = "Expert Crack and Burn Ages")
summary(ab.EBT, what = "bias")
plotAB(ab.EBT,col.CIsig="black", ylim=c(1,30), xlim=c(1,31))

tiff("ab.EBT.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.EBT,col.CIsig="black", ylim=c(1,30), xlim=c(1,31))
dev.off()


# NOVICE 1 VS MEAN AGE (THIN)
ab.N1T <- ageBias(Thin_Novice.A~Thin_Mean, data=Mean.Ages, ref.lab = "Mean Ages for Thin Section", nref.lab = "N1 Thin Section Ages")
summary(ab.N1T, what = "bias")
plotAB(ab.N1T,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))

tiff("ab.N1T.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N1T,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))
dev.off()


# NOVICE 1 VS MEAN AGE (BURN)
ab.N1B <- ageBias(Burn_Novice.A~Burn_Mean, data=Mean.Ages, ref.lab = "Mean Ages for Crack and Burn", nref.lab = "N1 Crack and Burn Ages")
summary(ab.N1B, what = "bias")
plotAB(ab.N1B,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))

tiff("ab.N1B.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N1B,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))
dev.off()


# NOVICE 1 THIN VS Novice 1 BURN 
ab.N1BT <- ageBias(Burn_Novice.A~Thin_Novice.A, data=Mean.Ages, ref.lab = "N1 Thin Section Ages", nref.lab = "N1 Crack and Burn Ages")
summary(ab.N1BT, what = "bias")
plotAB(ab.N1BT,col.CIsig="black", ylim=c(1,30), xlim=c(1,31))

tiff("ab.N1BT.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N1BT,col.CIsig="black", ylim=c(1,30), xlim=c(1,31))
dev.off()


# NOVICE 2 VS MEAN AGE (THIN)  
ab.N2T <- ageBias(Thin_Novice.B~Thin_Mean, data=Mean.Ages, ref.lab = "Mean Ages for Thin Section", nref.lab = "N2 Thin Section Ages")
summary(ab.N2T, what = "bias")
plotAB(ab.N2T,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))

tiff("ab.N2T.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N2T,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))
dev.off()


# NOVICE 2 VS MEAN AGE (BURN) 
ab.N2B <- ageBias(Burn_Novice.B~Burn_Mean, data=Mean.Ages, ref.lab = "Mean Ages for Crack and Burn", nref.lab = "N2 Crack and Burn Ages")
summary(ab.N2B, what = "bias")
plotAB(ab.N2B,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))

tiff("ab.N2B.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N2B,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))
dev.off()


# NOVICE 2 THIN VS NOVICE 2 BURN
ab.N2BT <- ageBias(Burn_Novice.B~Thin_Novice.B, data=Mean.Ages, ref.lab = "N2 Thin Section Ages", nref.lab = "N2 Crack and Burn Ages")
summary(ab.N2BT, what = "bias")
plotAB(ab.N2BT,col.CIsig="black", ylim=c(1,30), xlim=c(1,31))

tiff("ab.N2BT.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N2BT,col.CIsig="black", ylim=c(1,30), xlim=c(1,31))
dev.off()



# EXPERT VS NOVICE 1 (THIN)
ab.EN1T <- ageBias(Thin_Novice.A~Thin_Expert, data=Mean.Ages, ref.lab = "Expert Thin Section Ages", nref.lab = "N1 Thin Section Ages")
summary(ab.EN1T, what = "bias")
plotAB(ab.EN1T,col.CIsig="black", ylim=c(1,32), xlim=c(1,30))

tiff("ab.EN1T.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.EN1T,col.CIsig="black", ylim=c(1,32), xlim=c(1,30))
dev.off()


# EXPERT VS NOVICE 1 (BURN)
ab.EN1B <- ageBias(Burn_Novice.A~Burn_Expert, data=Mean.Ages, ref.lab = "Expert Crack and Burn Ages", nref.lab = "N1 Crack and Burn Ages")
summary(ab.EN1B, what = "bias")
plotAB(ab.EN1B,col.CIsig="black",ylim=c(1,32), xlim=c(1,30))

tiff("ab.EN1B.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.EN1B,col.CIsig="black", ylim=c(1,32), xlim=c(1,30))
dev.off()


# EXPERT VS NOVICE 2 (THIN)
ab.EN2T <- ageBias(Thin_Novice.B~Thin_Expert, data=Mean.Ages, ref.lab = "Expert Thin Section Ages", nref.lab = "N2 Thin Section Ages")
summary(ab.EN2T, what = "bias")
plotAB(ab.EN2T,col.CIsig="black", ylim=c(1,32), xlim=c(1,30))

tiff("ab.EN2T.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.EN2T,col.CIsig="black", ylim=c(1,32), xlim=c(1,30))
dev.off()



# EXPERT VS NOVICE 2 (BURN)
ab.EN2B <- ageBias(Burn_Novice.B~Burn_Expert, data=Mean.Ages, ref.lab = "Expert Crack and Burn Ages", nref.lab = "N2 Crack and Burn Ages")
summary(ab.EN2B, what = "bias")
plotAB(ab.EN2B,col.CIsig="black",ylim=c(1,32), xlim=c(1,30))

tiff("ab.EN2B.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.EN2B,col.CIsig="black", ylim=c(1,32), xlim=c(1,30))
dev.off()



# NOVICE 1 VS NOVICE 2 (BURN)
ab.N1N2B <- ageBias(Burn_Novice.B~Burn_Novice.A, data=Mean.Ages, ref.lab = "N1 Crack and Burn Ages", nref.lab = "N2 Crack and Burn Ages")
summary(ab.N1N2B, what = "bias")
plotAB(ab.N1N2B,col.CIsig="black",ylim=c(1,32), xlim=c(1,30))

tiff("ab.N1N2B.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N1N2B,col.CIsig="black", ylim=c(1,30), xlim=c(1,30))
dev.off()


# NOVICE 1 VS NOVICE 2 (THIN)
ab.N1N2T <- ageBias(Thin_Novice.B~Thin_Novice.A, data=Mean.Ages, ref.lab = "N1 Thin Section Ages", nref.lab = "N2 Thin Section Ages")
summary(ab.N1N2T, what = "bias")
plotAB(ab.N1N2T,col.CIsig="black",ylim=c(1,32), xlim=c(1,30))

tiff("ab.N1N2T.tiff",height= 7, width= 7, units="in", res = 300)
plotAB(ab.N1N2T,col.CIsig="black", ylim=c(1,32), xlim=c(1,30))
dev.off()



# PRECISION STATS 

# PRECISION VALUES FOR ALL THIN AGES 
ap.Thin <- agePrecision(~Thin_Novice.A+Thin_Novice.B+Thin_Expert, data=Mean.Ages)
summary(ap.Thin, what="precision")


# PRECISION VALUES FOR ALL CRACK AND BURN AGES
ap.Burn <- agePrecision(~Burn_Novice.A+Burn_Novice.B+Burn_Expert, data=Mean.Ages)
summary(ap.Burn, what="precision")


# PRECISION FOR NOVICE 1 BETWEEN METHODS
ap.Novice1 <- agePrecision(~Burn_Novice.A+Thin_Novice.A, data=Mean.Ages)
summary(ap.Novice1, what="precision")


# PRECISION FOR NOVICE 2 BETWEEN METHODS
ap.Novice2 <- agePrecision(~Burn_Novice.B+Thin_Novice.B, data=Mean.Ages)
summary(ap.Novice2, what="precision")


# PRECISION FOR EXPERT BETWEEN METHODS
ap.Expert <- agePrecision(~Burn_Expert+Thin_Expert, data=Mean.Ages)
summary(ap.Expert, what="precision")

# PRECISION FOR ALL AGERS BETWEEN METHODS 
ap.Agers <- agePrecision(~Thin+Burn, data=BLAND_ALT)
summary(ap.Agers, what="precision")

# Download Data
install.packages("writexl")
library("writexl")
write_xlsx(Ages,"\\Volumes\\MADDOG\\Research\\Otolith Aging\\Otolith Data\\LWF_Otolith_Final\\Ages.xlsx")





