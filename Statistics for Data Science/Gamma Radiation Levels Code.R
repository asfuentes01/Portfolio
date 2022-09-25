# DSC520
# Final Project Part II
# Astrid Fuentes
# 26-Feb-2021

## Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)


## Set the working directory to the root of your DSC 520 directory
setwd("/Users/astrid/Documents/GitHub/dsc520")

## Load the data file after converting to csv
data <- read.csv("week10/gamma_radiation_readings.csv")

## Convert certain variables to factor
data$Facility.Operator <- factor(data$Facility.Operator)
data$Location <- factor(data$Location)
data$Quarter <- factor(data$Quarter)

## Data cleaning: drop rows with missing values in key variables
data <- data %>% drop_na(Facility.Operator, Location, Year, Quarter, Gamma.Radiation)

## Data cleaning: fix typo in Location variable
data$Location <- replace(data$Location, data$Location == "Captial District", "Capital District")

## Check for outliers

summary(data$Gamma.Radiation)

hist(data$Gamma.Radiation,
     xlab = "Gamma Radiation Levels",
     main = "Histogram of Gamma Radiation Levels",
     breaks = sqrt(nrow(data))
) 

data %>%
  filter(Gamma.Radiation <= 50) %>%
  ggplot() + 
  geom_histogram(aes(x = Gamma.Radiation), fill = "grey", color = "black")

# Drop outliers Gamma.Radiation >=50

data2 <- subset(data, data$Gamma.Radiation <= 50)

# Repeat histogram with clean data

hist(data2$Gamma.Radiation,
     xlab = "Gamma Radiation Levels",
     main = "Histogram of Gamma Radiation Levels",
     breaks = sqrt(nrow(data2))
) 

## Descriptions of the cleaned data set
str(data2)
head(data2)
summary(data2)
nrow(data2)
ncol(data2)
head(data2)

## Correlation

cor(data2$Gamma.Radiation, data2$Year, method = c("pearson", "kendall", "spearman"))
cor.test(data2$Gamma.Radiation, data2$Year, method=c("pearson", "kendall", "spearman"))

## Scatter plot
plot(data2$Year, data2$Gamma.Radiation, main="Gamma Radiation per quarter", 
     xlab="Year", ylab="Gamma Radiation Level (mrem/quarter)", pch=19)

ggplot(data2, aes(Year, Gamma.Radiation, color=Quarter)) +
  geom_point(alpha=0.5, size=2) +
  labs(y="Gamma Radiation Level (mrem/quarter)", x="Year", subtitle="Gamma Radiation per quarter")

# Time plots
time.series <- ts(data2$Gamma.Radiation, start=1996, end=2018)

plot(time.series, main="Gamma Radiation Over Time")

# Correlation and covariance
cor(data[], method="pearson")
cov(data[])

#Regression

gamma.rad_lm <-  lm(Gamma.Radiation ~ Facility.Operator + Location + Year + Quarter, data=data)


summary(gamma.rad_lm)

ci <- confint(gamma.rad_lm)
ci

gamma.rad_lm2 <-  lm(Gamma.Radiation ~ Year , data=data)
summary(gamma.rad_lm2)

ci2 <- confint(gamma.rad_lm2)
ci2
