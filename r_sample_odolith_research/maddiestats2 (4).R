library(tidyverse)
setwd("C:/Users/phutchins/Desktop/Otolith")

#Define functions
##Create a function to calculate CV
CV <- function(x){
  sdevx <- sd(x)
  meanx <- mean(x)
  
  CV <- (sdevx/meanx)
  return(CV)
}

##Create a function to calculate percent error of each ager compared to the expert (true value)
perror <- function(data, id = names(data[1]), values = names(data[length(data)]), conditions = names(data[2:(length(data)-1)]), true){
  #data is the data frame
  #id is the character name of the column containing sample identifiers
  #values is the character name of the column containing measurements to calculate percent error over
  #conditions a charcter vector of the name(s) of the column(s) containing the condition(s) of the measurement (must be same length as true)
  #true a character vector of the name(s) of the condition(s) that represent(s) the true values (must be same length as conditions)
  #... is any number of character names of the conditions that represent the unknown values
  
  #Dependencies
  library(dplyr)
  library(data.table)
  
  #Check to see if true conditions have been identified
  if(!length(true) == length(conditions)){stop("\'true\' must have the same length as \'condition\'. Use empty quotes where needed")}
  
  #Create a dataframe with a sample ID column, followed by columns for all unique combination of conditions
  cond <- list()
  for(c in conditions){
    cond[[c]] <- unique(data[!data[,c] == true,c])
  }
  
  #Create some simple objects that we will need for the calculation
  truths <- which(nchar(true) >0)
  truename <- paste0(true[!true == ""], collapse = "_")
  meansdf <- group_by(data, .dots = c(id,names(cond))) %>% summarise(means = mean(!!sym(values)))
  meansdfcast <- dcast(setDT(meansdf), paste0(id, " + ... ~ ", paste0(conditions[truths], collapse = " + ")), value.var = "means")
  truemean = meansdfcast[[truename]]
  coln <- names(meansdfcast)[!names(meansdfcast) %in% conditions & !names(meansdfcast) %in% id & !names(meansdfcast) == truename]
  names(meansdfcast)[names(meansdfcast) %in% coln] <- paste0("mean_", coln)
  
  #Calculate the percent error
  for(col in paste0("mean_", coln)){
    meansdfcast[,paste0("perror_", gsub("mean_", "", col))] <- -100*(truemean - meansdfcast[[col]])/(truemean)
  }
  
  #Remove the column corresponding to the true values
  meansdfcast[[truename]] <- NULL
  
  #Make the dataframe
  out <- melt(setDT(meansdfcast), 
              id.vars = c(id, conditions[which(nchar(true) == 0)]), 
              variable.name = conditions[truths],
              value.name = c("mean", "perror"),
              measure.vars = patterns("mean.+", "perror.+"))
  
  #Return the dataframe 
  return(out)
}

#Format Data
##Read and format data for first file
rep2 <- read.csv("SecondAges1.csv", header = T) #Read the first file
rep2 <- gather(rep2, key = "Method_Ager", value = "Age", -Individual) %>% #Restructure the data into long format
  mutate(Method = str_sub(Method_Ager, start = 1, end = 4)) %>% #Create a column for the method variable
  mutate(Ager = str_sub(Method_Ager, start = 10, end = 10)) %>% #Create a column for the ager variable
  mutate(Method_Ager = NULL)#Remove this column as it is no longer needed
rep2 <- rep2[!is.na(rep2$Age),c(1,3,4,2)] #Remove any samples that were NA and reorder the columns to be more intuitive

##As above, read and format data for the second file
rep1 <- read.csv("FinalAges.csv", header = T)
rep1 <- gather(rep1, key = "Method_Ager", value = "Age", -Individual) %>%
  mutate(Method = str_sub(Method_Ager, start = 1, end = 4)) %>%
  mutate(Ager = str_sub(Method_Ager, start = 10, end = 10)) %>%
  mutate(Method_Ager = NULL)
rep1 <- rep1[!is.na(rep1$Age),c(1,3,4,2)]

##Combine the two data files into one dataframe
agedata <- rbind(rep1, rep2)

##Filter dataframe so that only fish that were analyzed by Ager 1 and 2 twice with both methods and by Ager 3 once
##with both methods (i.e. 10 measurements) are selected
samples <- vector() #Create an empty vector to populate with data
for(n in unique(agedata$Individual)){ #Create a for-loop to apply to every unique fish
  #Populate the vector with fish sample names that have exactly 10 measurements
  if(length(agedata$Individual[agedata$Individual == n]) == 10) {samples <- c(samples, n)} 
}

agedatatrim <- agedata[agedata$Individual %in% samples,] #Filter the data to include only the above fish samples
agedatatrim$Ager <- gsub(1, "Novice", agedatatrim$Ager) #Replace numeric identifiers with letters so R doesn't treat
agedatatrim$Ager <- gsub(2, "Intermediate", agedatatrim$Ager) #these like continuous variables and it is better for plots
agedatatrim$Ager <- gsub(3, "Expert", agedatatrim$Ager)
agedatatrim$Ager <- factor(agedatatrim$Ager, c("Novice", "Intermediate", "Expert")) #This will tell R to plot in this order

#Remove things that you no longer need
rm(rep1)
rm(rep2)
rm(agedata)
rm(n)
rm(samples)

#Calculate percent error and CV for each paired measure
#Use the perror function to return the means and percent error of the paired measurments
agestats <- perror(data = agedatatrim, id = "Individual", values = "Age", conditions = c("Method", "Ager"), true = c("", "Expert"))
agestats$Ager <- gsub(1, "Novice", agestats$Ager) #Replace numeric identifiers with letters so R doesn't treat
agestats$Ager <- gsub(2, "Intermediate", agestats$Ager) #these like continuous variables
agestats$Ager <- factor(agestats$Ager, c("Novice", "Intermediate", "Expert")) #This will tell R to plot in this order

#Apply the CV function to age values for unique combinations of individual fish, method, and ager (i.e. two age measurements)
agestats$CV <-  aggregate(Age~Individual+Method+Ager, data = agedatatrim, FUN = CV)[1:368,4]

#Plots
##Regression (not very useful)
rxnorm <- 
  ggplot(agestats, aes(x = Method, y = mean)) +
  geom_jitter(width = .2, height = 0) +
  geom_segment(size = 2, color = "blue", 
               aes(x = "Thin", 
                   xend = "Burn", 
                   y = mean(agestats$mean[agestats$Method == "Thin"]), 
                   yend = mean(agestats$mean[agestats$Method == "Burn"]))) + 
  labs(y = "Age Estimate") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

rxnorm

##Density plots for each method for each ager (useful in showing potential biases for managers trying to make decisions)
trueages <- agedatatrim[agedatatrim$Ager == "Expert",]
names(trueages)[4] <- "mean"
agestats_combined <- rbind(agestats, trueages, fill = T)

agedensity <- 
  ggplot(agestats_combined, aes(x = mean, fill = Method)) +
  geom_density(alpha = 0.4) +
  labs(x = "Age Estimate", y = "Density") +
  scale_fill_manual(labels = c("Crack-and-Burn", "Thin-Sectioning"), values = c("purple", "forestgreen")) +
  geom_vline(xintercept = mean(agestats$mean[agestats$Method == "Burn"]), linetype = "dashed", size = 1, color = "purple") +
  geom_vline(xintercept = mean(agestats$mean[agestats$Method == "Thin"]), linetype = "dashed", size = 1, color = "forestgreen") +
  annotate("text", x = 23, y = 0.11, color = "purple", size = 5,
           label = paste0("Crack-and-Burn mean: ", round(mean(agestats$mean[agestats$Method == "Burn"]), digits = 2))) +
  annotate("text", x = 23, y = 0.1, color = "forestgreen", size = 5, 
           label = paste0("Thin-Sectioning mean: ", round(mean(agestats$mean[agestats$Method == "Thin"]), digits = 2))) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  facet_grid(~Ager)

agedensity

###Make a dataframe of means from the replicate measurements
meandata <- agestats_combined %>% pivot_wider(id_cols = c(Individual, Ager), names_from = Method, values_from = mean)
rsq <- paste("R^2 == ", round(sqrt(cor(meandata$Burn, meandata$Thin, method = "pearson")), 2))

install.packages("dplyr")
library(dplyr)

install.packages("purrr")
library(purrr)

install.packages("ggplot2")
library(ggplot2)

scatter <- #Make plot
  ggplot(data = meandata, aes(x = Thin, y = Burn)) +
  geom_jitter(aes(color = Ager)) +
  labs(x = "Thin Sectioning", y = "Crack-and-Burn") +
  stat_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1) +
  annotate("text", x = 28.5, y = 25.5, label = rsq, parse = T) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

scatter #View plot

###Make a dataframe of differences between the paired observations
bandalt <- meandata %>% mutate(Diff = Thin - Burn)
####Add a column for the mean measurement of the paired observations
bandalt$Mean <- apply(bandalt[,3:4], 1, mean)


###Band Altman plot, which shows that the most inexperienced ager was not good at aging older fish
bandaltplot <- #Make plot
  ggplot(data = bandalt, aes(x = Mean, y = Diff)) +
  geom_jitter(aes(fill = Ager), shape = 21, width = 1, height = 1) +
  geom_smooth(method = "lm", se=FALSE) +  
  geom_hline(yintercept = mean(bandalt$Diff), color = "blue", linetype = "longdash") + 
  geom_hline(yintercept = mean(bandalt$Diff)+1.96*sd(bandalt$Diff), color = "brown", linetype = "longdash") + 
  geom_hline(yintercept = mean(bandalt$Diff)+-1.96*sd(bandalt$Diff), color = "brown", linetype = "longdash") + 
  annotate(geom = "text", x = 25, y = 2,label = paste0("Mean = ", round(mean(bandalt$Diff), 2)), color = "blue", size = 4) +
  annotate(geom = "text", x = 25, y = 8.3,label = paste0("uLOA = ", round(mean(bandalt$Diff)+(1.96*sd(bandalt$Diff)), 2)), color = "brown", size = 4) +
  annotate(geom = "text", x = 25, y = -3.5,label = paste0("lLOA = ", round(mean(bandalt$Diff)+(-1.96*sd(bandalt$Diff)), 2)), color = "brown", size = 4) +
  labs(x = "Mean of Techniques (Yrs)", y = "(Thin Sectioning) - (Crack-and-Burn) (Yrs)") +
  scale_fill_manual(name = "Ager: ", values = c("grey100", "grey70", "grey10")) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.position = "bottom"
  )

bandaltplot #View plot

ggsave("bandaltman.jpg", bandaltplot, height = 6, width = 6, units = "in")

#If dont want all three regression lines on graph remove color=Ager from ggplot and if want CI for the one mean line then remove se=False


hist(bandalt$Diff)
sd(bandalt$Diff)
# should lie within d+-2sd
#1.4+2(2.975079)
# Upper is 7.35
# Lower is -4.55

age_boxplot <- #Make a boxplot of the age estimates
  ggplot(data = agedatatrim, aes(x = Ager, y = Age, fill = Method)) +
  geom_boxplot(show.legend = F) +
  labs(x = "Ager", y = "Age Estimate (Yrs)") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

age_boxplot

ggsave("age_boxplot.jpg", age_boxplot, height = 4, width = 3.5, units = "in")


perror_boxplot <- #Make a boxplot of the perror estimates
  ggplot(data = agestats, aes(x = Ager, y = perror, fill = Method)) +
  geom_boxplot(show.legend = F) +
  labs(x = "Ager", y = "Percent Error") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

perror_boxplot

ggsave("perror_boxplot.jpg", perror_boxplot, height = 4, width = 3.5, units = "in")


forlegendonly <- ggplot(data = agestats, aes(x = Ager, y = perror, fill = Method)) +
  geom_boxplot() +
  labs(y = "Percent Error") +
  scale_fill_discrete(name = "Method: ", labels = c("Crack and Burn", "Thin Sectioning"))+
  theme(
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.title = element_blank()
  )

library(cowplot)
boxplots <- plot_grid(age_boxplot, perror_boxplot, get_legend(forlegendonly), 
          nrow = 3, rel_heights = c(1,1,.15), align = "v", axis = "l",
          labels = c("A", "B", NA), label_x = 0.20, label_y = 0.95)

ggsave("boxplots.jpg", boxplots, height = 4, width = 3.5, units = "in")

##Scatterplot of percent error and mean age
meanerrorscatter <- ggplot(data = agestats, aes(x = mean, y = perror)) +
  geom_point(aes(fill = Ager), shape = 21) +
  scale_fill_manual(name = "Ager: ", values = c("grey90", "grey10")) +
  labs(x = "Mean Age Estimate (Years)", y = "Percent Error") +
  facet_grid(~Method)+
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(color = "black", fill = "grey80"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.position = "bottom"
  )

ggsave("meanerrorscatter.jpg", meanerrorscatter, height = 4, width = 3.5, units = "in")

# Percent Agreement Plot
ggplot(data = Percentagreement, aes(x = Age.Difference, y = Percent.Agreement)) + 
  geom_jitter(aes(color = Ager)) +
  geom_line(aes(color = Ager), size = 1)
labs(x = "Age Difference", y = "Percent Agreement") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

install.packages("tidyverse")
library("tidyverse")

Percentagreementplot <-
  ggplot(data = Percentagreement, aes(x = Age.Difference, y = Percent.Agreement, group = Ager)) +
  geom_line(aes(linetype = Ager), size = .5, col= "black") +
  scale_linetype_manual(values = c ("solid", "twodash", "dashed", "dotted")) +
  labs(x = "Age Difference (Yrs)", y = "Percent Agreement (%)") +
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
Percentagreementplot
Percentagreementplot + scale_x_discrete(name="Age Difference (Yrs)", limits=c(0,2,4,6,8,10,12))

ggsave("Percentagreement", height = 4, width = 3.5, units = "in")







##Experimenting...

experiment <- bandalt %>%
  group_by(Ager, round(Mean,0)) %>%
  summarise(ymin = min(Diff), ymax = max(Diff))
names(experiment)[2] <- "Mean"

ggplot(data = experiment) +
  geom_ribbon(aes(x = Mean, ymin = ymin, ymax = ymax, fill = Ager), alpha = .2) +
  labs(y = "Difference of Methods", x = "Mean Age Estimate (Years)")
          



## Trying to use Ogles code for Precision values
install.packages(FSA)
library(FSA)


# All Agers for Burn
ap.FinalAgesBurn <- agePrecision(~Burn_Ager1+Burn_Ager2+Burn_Ager3, data = FinalAges)
summary(ap.FinalAgesBurn, what = "precision")


# All Agers for Thin
ap.FinalAgesThin <- agePrecision(~Thin_Ager1+Thin_Ager2+Thin_Ager3, data = FinalAges)
summary(ap.FinalAgesThin, what = "precision")

library(plyr)
count(bandalt$Thin$Diff)

# Compare Novice to Expert for Thin
ap.FinalAgesNE <- agePrecision(~Thin_Ager1+Thin_Ager3, data = FinalAges)
summary(ap.FinalAgesNE, what = "precision")

# Compare Novice to Intermediate for Thin
ap.FinalAgesNI <- agePrecision(~Thin_Ager1+Thin_Ager2, data = FinalAges)
summary(ap.FinalAgesNI, what = "precision")

# Compare Intermediate to Expert for Thin
ap.FinalAgesIE <- agePrecision(~Thin_Ager2+Thin_Ager3, data = FinalAges)
summary(ap.FinalAgesIE, what = "precision")

# Compared Novice to Expert for Burn
ap.FinalAgesNEBurn <- agePrecision(~Burn_Ager1+Burn_Ager3, data = FinalAges)
summary(ap.FinalAgesNEBurn, what = "precision")

# Compare Novice to Intermediate for Burn
ap.FinalAgesNIBurn <- agePrecision(~Burn_Ager1+Burn_Ager2, data = FinalAges)
summary(ap.FinalAgesNIBurn, what = "precision")

# Compare Intermediate to Expert for Burn
ap.FinalAgesIEBurn <- agePrecision(~Burn_Ager2+Burn_Ager3, data = FinalAges)
summary(ap.FinalAgesIEBurn, what = "precision")

# Compare Thin to Burn for Novice
ap.FinalAgesTBNovice <- agePrecision(~Thin_Ager1+Burn_Ager1, data = FinalAges)
summary(ap.FinalAgesTBNovice, what = "precision")

# Compare Thin to Burn for Intermediate
ap.FinalAgesTBIntermediate <- agePrecision(~Thin_Ager2+Burn_Ager2, data = FinalAges)
summary(ap.FinalAgesTBIntermediate, what = "precision")

# Compare Thin to Burn for Expert
ap.FinalAgesTBExpert <- agePrecision(~Thin_Ager3+Burn_Ager3, data = FinalAges)
summary(ap.FinalAgesTBExpert, what = "precision")

# Need to combind agers for thin and burn to compare both methods from all agers 
ap.AllThinBurn <- agePrecision(~Thin+Burn, data = bandalt)
summary(ap.AllThinBurn, what = "precision")


# Need to combind agers for thin and burn to compare both methods between Novice and Expert
ap.AllThinBurn <- agePrecision(~Thin+Burn, data = Bandalt.N_E)
summary(ap.AllThinBurn, what = "precision")

# Seeing how many values for 0,1,2 year age difference between the methods from all agers (percent agreement)
library(plyr)
count(bandalt$Diff)

# Seeing how many values for 0,1,2 year age difference between the methods between Novice and Expert (percent agreement)
library(plyr)
count(Bandalt.N_E$Diff)

# Seeing how many values for 0,1,2 year age difference between Novice and Expert for Thin section (percent agreement)
library(plyr)
count(Diff_NE$Thin_Diff)

# Seeing how many values for 0,1,2 year age difference between Novice and Expert for burn (percent agreement)
library(plyr)
count(Diff_NE$Burn_Diff)


 




