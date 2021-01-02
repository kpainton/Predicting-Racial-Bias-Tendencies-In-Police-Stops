library(ggplot2)
library(dplyr)
library(scales)

data <- read.csv("complete_clean_data.csv")

# Finds the ratio or republicans to democrats 
data$rToDRatio <- 2 * data$Republican / data$Democratic
data$stop_time_hour <- as.numeric(data$stop_time) - 1

# Creates new data frames for time, age, vote preferences, and violations from original dataframe
timeData <- data.frame(data$stop_time_hour, data$driver_race)
ageData <- data.frame(data$driver_age, data$driver_race)
voteData <- data.frame(data$rToDRatio, data$driver_race)
violationsData <- data.frame(data$TotalViolations, data$driver_race)

# Filters data based on race

# Time Data Set 
whiteTimeData <- timeData %>%
  filter(data.driver_race == 'White')
asianTimeData <- timeData %>%
  filter(data.driver_race == 'Asian')
hispanicTimeData <- timeData %>%
  filter(data.driver_race == 'Hispanic')
blackTimeData <- timeData %>%
  filter(data.driver_race == 'Black')
otherTimeData <- timeData %>%
  filter(data.driver_race == 'Other')

# Age Data Set
whiteAgeData <- ageData %>%
  filter(data.driver_race == 'White')
asianAgeData <- ageData %>%
  filter(data.driver_race == 'Asian')
hispanicAgeData <- ageData %>%
  filter(data.driver_race == 'Hispanic')
blackAgeData <- ageData %>%
  filter(data.driver_race == 'Black')
otherAgeData <- ageData %>%
  filter(data.driver_race == 'Other')

# Violations Data Set 
whiteViolationsData <- violationsData %>%
  filter(data.driver_race == 'White')
asianViolationsData <- violationsData %>%
  filter(data.driver_race == 'Asian')
hispanicViolationsData <- violationsData %>%
  filter(data.driver_race == 'Hispanic')
blackViolationsData <- violationsData %>%
  filter(data.driver_race == 'Black')
otherViolationsData <- violationsData %>%
  filter(data.driver_race == 'Other')

# Voting Data Set
whiteVoteData <- voteData %>%
  filter(data.driver_race == 'White')
asianVoteData <- voteData %>%
  filter(data.driver_race == 'Asian')
hispanicVoteData <- voteData %>%
  filter(data.driver_race == 'Hispanic')
blackVoteData <- voteData %>%
  filter(data.driver_race == 'Black')
otherVoteData <- voteData %>%
  filter(data.driver_race == 'Other')

par(mfrow = c(2, 3))

# Creates historgrams for time of day of stops for each race
hist(whiteTimeData$data.stop_time_hour, freq = FALSE, main = 'White', xlab = 'Hour of Day', 
     ylab = 'Percentage of Stops', xlim = c(0, 25), ylim = c(0, .10), breaks = 12)
hist(asianTimeData$data.stop_time_hour, freq = FALSE, main = 'Asian', xlab = 'Hour of Day', 
     ylab = 'Percentage of Stops', xlim = c(0, 25), ylim = c(0, .10), breaks = 12)
hist(hispanicTimeData$data.stop_time_hour, freq = FALSE, main = 'Hispanic', xlab = 'Hour of Day', 
     ylab = 'Percentage of Stops', xlim = c(0, 25), ylim = c(0, .10), breaks = 12)
hist(blackTimeData$data.stop_time_hour, freq = FALSE, main = 'Black', xlab = 'Hour of Day', 
     ylab = 'Percentage of Stops', xlim = c(0, 25), ylim = c(0, .10), breaks = 12)
hist(otherTimeData$data.stop_time_hour, freq = FALSE, main = 'Other', xlab = 'Hour of Day', 
     ylab = 'Percentage of Stops', xlim = c(0, 25), ylim = c(0, .10), breaks = 12)

# Completes ANOVA test to see if there is a mean difference between the stop times for each race
timeAnova <- aov(data = timeData, formula = data.stop_time_hour ~ data.driver_race)
summary(timeAnova)

# Completes TukeyPostHoc to idneitfy where the mean differences are between each group
timeTuk <- TukeyHSD(timeAnova)
timeTuk

par(mfrow = c(2, 3))

# Creates historgrams for age for each race
hist(whiteAgeData$data.driver_age, freq = FALSE, main = 'White', xlab = 'Age of Driver', 
     ylab = 'Percentage of Stops', xlim = c(15, 100), ylim = c(0, .05), breaks = 17)
hist(asianAgeData$data.driver_age, freq = FALSE, main = 'Asian', xlab = 'Age of Driver', 
     ylab = 'Percentage of Stops', xlim = c(15, 100), ylim = c(0, .05), breaks = 17)
hist(hispanicAgeData$data.driver_age, freq = FALSE, main = 'Hispanic', xlab = 'Age of Driver', 
     ylab = 'Percentage of Stops', xlim = c(15, 100), ylim = c(0, .05), breaks = 17)
hist(blackAgeData$data.driver_age, freq = FALSE, main = 'Black', xlab = 'Age of Driver', 
     ylab = 'Percentage of Stops', xlim = c(15, 100), ylim = c(0, .05), breaks = 17)
hist(otherAgeData$data.driver_age, freq = FALSE, main = 'Other', xlab = 'Age of Driver', 
     ylab = 'Percentage of Stops', xlim = c(15, 100), ylim = c(0, .05), breaks = 17)

# Completes ANOVA test to see if there is a mean difference between ages for each race
ageAnova <- aov(data = ageData, formula = data.driver_age ~ data.driver_race)
summary(ageAnova)

# Completes TukeyPostHoc to idneitfy where the mean differences are between each group
ageTuk <- TukeyHSD(ageAnova)
ageTuk

par(mfrow = c(2, 3))

# Creates historgrams for total violations for each race
hist(whiteViolationsData$data.TotalViolations, freq = FALSE, main = 'White', xlab = 'Total Violations', 
     ylab = 'Percentage of Stops', xlim = c(0, 10), ylim = c(0, 1), breaks = 8)
hist(asianViolationsData$data.TotalViolations, freq = FALSE, main = 'Asian', xlab = 'Total Violations', 
     ylab = 'Percentage of Stops', xlim = c(0, 10), ylim = c(0, 1), breaks = 8)
hist(hispanicViolationsData$data.TotalViolations, freq = FALSE, main = 'Hispanic', xlab = 'Total Violations', 
     ylab = 'Percentage of Stops', xlim = c(0, 10), ylim = c(0, 1), breaks = 8)
hist(blackViolationsData$data.TotalViolations, freq = FALSE, main = 'Black', xlab = 'Total Violations', 
     ylab = 'Percentage of Stops', xlim = c(0, 10), ylim = c(0, 1), breaks = 8)
hist(otherViolationsData$data.TotalViolations, freq = FALSE, main = 'Other', xlab = 'Total Violations', 
     ylab = 'Percentage of Stops', xlim = c(0, 10), ylim = c(0, 1), breaks = 8)

# Completes ANOVA test to see if there is a mean difference between the number of violations for each race
violationsAnova <- aov(data = violationsData, formula = data.TotalViolations ~ data.driver_race)
summary(violationsAnova)

# Completes TukeyPostHoc to idneitfy where the mean differences are between each group
violationsTuk <- TukeyHSD(violationsAnova)
violationsTuk

par(mfrow = c(2, 3))

# Creates historgrams for voting preferences of county for each race
hist(whiteVoteData$data.rToDRatio, freq = FALSE, main = 'White', xlab = 'Republican to Democrat', 
     ylab = 'Percentage of Stops', xlim = c(0, 7), ylim = c(0, .7), xaxt = 'n',
     breaks = c(.6, seq(1.4, 7, .8)))
axis(side = 1, at = seq(.6, 7, 1.6), labels = seq(.3, 3.5, .8))
hist(asianVoteData$data.rToDRatio, freq = FALSE, main = 'Asian', xlab = 'Republican to Democrat', 
     ylab = 'Percentage of Stops', xlim = c(0, 7), ylim = c(0, .7), xaxt = 'n',
     breaks = c(.6, seq(1.4, 7, .8)))
axis(side = 1, at = seq(.6, 7, 1.6), labels = seq(.3, 3.5, .8))
hist(hispanicVoteData$data.rToDRatio, freq = FALSE, main = 'Hispanic', xlab = 'Republican to Democrat', 
     ylab = 'Percentage of Stops', xlim = c(0, 7), ylim = c(0, .7), xaxt = 'n',
     breaks = c(.6, seq(1.4, 7, .8)))
axis(side = 1, at = seq(.6, 7, 1.6), labels = seq(.3, 3.5, .8))
hist(blackVoteData$data.rToDRatio, freq = FALSE, main = 'Black', xlab = 'Republican to Democrat', 
     ylab = 'Percentage of Stops', xlim = c(0, 7), ylim = c(0, .7), xaxt = 'n',
     breaks = c(.6, seq(1.4, 7, .8)))
axis(side = 1, at = seq(.6, 7, 1.6), labels = seq(.3, 3.5, .8))
hist(otherVoteData$data.rToDRatio, freq = FALSE, main = 'Other', xlab = 'Republican to Democrat', 
     ylab = 'Percentage of Stops', xlim = c(0, 7), ylim = c(0, .7), xaxt = 'n', 
     breaks = c(.6, seq(1.4, 7, .8)))
axis(side = 1, at = seq(.6, 7, 1.6), labels = seq(.3, 3.5, .8))

voteData$data.rToDRatio <- voteData$data.rToDRatio / 2

# Completes ANOVA test to see if there is a mean difference between the voting preferences of county for each race
voteAnova <- aov(data = voteData, formula = data.rToDRatio ~ data.driver_race)
summary(voteAnova)

# Completes TukeyPostHoc to idneitfy where the mean differences are between each group
voteTuk <- TukeyHSD(voteAnova)
voteTuk