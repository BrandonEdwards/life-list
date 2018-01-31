#####################################
# Brandon Edwards
# life-list
# yearly-percentage-of-available.R
# Created January 2018
# Last Updated January 2018
#####################################

#####################################
# Clear Memory
#####################################

remove(list = ls())

#####################################
# Import Libraries and Files
#####################################

# install.packages("ggplot2")

library(ggplot2)

#####################################
# Create Output Directories
#####################################

dir.create("output")
dir.create("output/percentage")

#####################################
# Simulate Data
#####################################

numAvailable <- 496 # Birds available to find in Ontario, technically
totalTicked <- 0

yearLifeListAdditions <- NULL
cumAvailableList <- NULL
cumLifeList <- NULL
yearList <- NULL

currentYear <- 1

while (numAvailable > 0)
{
  t <- rbinom(1, numAvailable, 0.10)
  totalTicked <- totalTicked + t
  
  yearLifeListAdditions <- c(yearLifeListAdditions, t)
  cumLifeList <- c(cumLifeList, totalTicked)
  cumAvailableList <- c(cumAvailableList, round((t/numAvailable)*100, digits = 2))
  yearList <- c(yearList, currentYear)
  
  numAvailable <- numAvailable - t
  currentYear <- currentYear + 1
}

cumulativeData <- data.frame(yearList, cumLifeList)
names(cumulativeData) <- c("Year", "Cumulative.Total")

yearData <- data.frame(yearList, yearLifeListAdditions)
names(yearData) <- c("Year", "Additions")

cumulativeAvailableData <- data.frame(yearList, cumAvailableList)
names(cumulativeAvailableData) <- c("Year", "Percentage")

totalYears <- length(yearList)

#####################################
# Plot Data
#####################################

p <- ggplot(cumulativeData,
       aes(Year, Cumulative.Total)) + 
  geom_line(size = 1, colour = "#56B4E9") +
  theme(plot.title = element_text(size = 20), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12), 
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = paste("Cumulative Life Species: ", totalYears, " Year Simulation", sep=""), x = "Year", y = "Number of Species") +
  scale_color_manual(name = "Checklist Type", 
                     values=c("#56B4E9"), 
                     labels = c("Total"))

png("output/percentage/simCumulativeLifeBirds.png",width = 10.5, height = 4, units = "in", res = 300)
print(p)
dev.off()

# Yearly life list additions
p <- ggplot(yearData, 
       aes(Year, Additions)) +
  geom_bar(stat = "identity", fill = "#56B4E9") +
  theme(plot.title = element_text(size = 20), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12), 
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = paste("Life Bird Additions by Year: ",totalYears, " Year Simulation", sep=""), x = "Year", y = "Number of Species")

png("output/percentage/yearlyLifeBirds.png",width = 10.5, height = 4, units = "in", res = 300)
print(p)
dev.off()

# Percentage available plot
p <- ggplot(cumulativeAvailableData, 
        aes(Year, Percentage)) +
   geom_bar(stat = "identity", fill = "#56B4E9") +
   theme(plot.title = element_text(size = 20), 
         axis.title = element_text(size = 15),
         axis.text = element_text(size = 12), 
         legend.title = element_text(size = 15), 
         legend.text = element_text(size = 12), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black")) +
   labs(title = paste("% Yearly Life Birds of Available Birds: ", totalYears, " Year Simulation", sep=""), x = "Year", y = "% Life Birds from Available")

png("output/percentage/percentAvailableLifeBirds.png",width = 10.5, height = 4, units = "in", res = 300)
print(p)
dev.off()