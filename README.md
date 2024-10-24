
# Import the CSV file
data <- read.csv("C:/Users/Saman/Desktop/Project Portfolios/Time Management and Productivity Insights.csv")


# Install required packages if you don't have them
install.packages("ggplot2")
install.packages("dplyr")

# Load the packages
library(ggplot2)
library(dplyr)

#Time management and Productivity Insights 


# What are some basic summary stats in the dataset?
# Basic summary statistics for the entire dataset
summary(data)

# Calculate standard deviation for specific columns
sd(data$Daily.Work.Hours)
sd(data$Daily.Leisure.Hours)
sd(data$Daily.Exercise.Minutes)
sd(data$Productivity.Score)


# Is there any correleation between Work Hours, Exercise Minutes, and Product Score?

# Correlation matrix for selected variables
cor_matrix <- cor(data[, c("Daily.Work.Hours", "Daily.Exercise.Minutes", "Productivity.Score")])
print(cor_matrix)

# Visualizing the correlation using pairs plot
pairs(data[, c("Daily.Work.Hours", "Daily.Exercise.Minutes", "Productivity.Score")])


# How does Daily sleep hours affect productivity scores?

Sleepmodel <- lm(Productivity.Score ~ Daily.Sleep.Hours, data = data)

summary (Sleepmodel)

#Relationship plot

plot(data$Daily.Sleep.Hours, data$Productivity.Score,
     main = "Sleep Hours vs. Productivity Score",
     xlab = "Daily Sleep Hours", ylab = "Productivity Score",
     pch = 19, col = "blue")

#add the regression line to the plot

abline(Sleepmodel, col = "red")


#Do you think screen time impact productivity?

#scatter plot between time and productivity 

plot(data$Screen.Time..hours., data$Productivity.Score,
     main = "Screen Time vs. Productivity Score",
     xlab= "Screen Time(hours)", ylab= "Productivity Score",
     pch = 19, col = "green")

#linear line

screenmodel <- lm(Productivity.Score ~ Screen.Time..hours., data = data)
abline(screenmodel, col = "red")

summary(screenmodel)


#How do different age groups compare with daily hours and productivity score?

# Create age group categories
data$Age.Group <- cut(data$Age, breaks = c(20, 30, 40, 50, 60, 70), right = FALSE)

# Calculate average work hours and productivity by age group
agesummary <- aggregate(cbind(Daily.Work.Hours, Productivity.Score) ~ Age.Group, data = data, FUN = mean)
print(agesummary)

# Bar plot for average productivity score by age group
barplot(agesummary$Productivity.Score, names.arg = agesummary$Age.Group,
        main = "Average Productivity Score by Age Group",
        xlab = "Age Group", ylab = "Average Productivity Score", col = "pink")
        

#Visualize relationships between exercise and productivity using box plots

# Box plot for exercise minutes and productivity score
boxplot(Productivity.Score ~ Daily.Exercise.Minutes, data = data,
        main = "Productivity Score by Exercise Minutes",
        xlab = "Daily Exercise Minutes", ylab = "Productivity Score",
        col = "purple")

#the results are shown that the more exercise the higher the productivity :)




        




