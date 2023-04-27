# Exercise 2: Working with vectors and functions

# 2.1

x <- runif(100, 1, 100)
mean_x <- round(mean(x), 2)
help(runif)
# runif(n, min, max) creates a vector of n element from a uniform 
# distribution with min and max values 

# a command that allows you to obtain the same value for mean_x every
# time you rerun the two code lines

# set a seed for the random number generator

for (i in 1:10) {
  set.seed(123)
  x <- runif(100, 1, 100)
  mean_x <- round(mean(x), 2)
  print(mean_x)
}

# 2.2

# Generate a vector x that entails all integers between 1 and 1000.

x <- 1:1000

# Find at least one more way to generate the same vector x.

x <- seq(1, 1000, by = 1)

# Generate a vector y such that the output of x + y returns a 
# vector that only contains 1000s.

y <- seq(999, 0, by = -1)

z = x + y

all(z == 1000)

# Assume x and y contain the amounts in dollars that 2000 
# different people possess. How much money do all people possess together?

sum(c(x, y))

# 1000000

# 2.3
# The following code draws 10,000 random values from a beta 
# distribution with the shape parameters a = 2 and b = 12 and
# multiplies them by a scale parameter scale = 15,000.

n <- 1e4
scale <- 1.5e4
income <- round( rbeta(n=n, shape1=2, shape2=12) * scale, 2)

# Let’s assume these simulated values represent the gross income
# in US$ of 10,000 people. The following lines of code plot a 
# histogram, which displays the counts of people that obtain an
# income in the respective range.

library(ggplot2) # only load (run) once

# Plot the resulting curve
ggplot(data.frame(x = income), aes(x=x)) +
  geom_histogram(color = "#0065BD", fill = "#0065BD", alpha=0.5, bins = 100) +
  scale_x_continuous(breaks = seq(0, scale, 1e3)) + 
  labs(x = "Gross income", 
       y = "Counts") + 
  theme_minimal()

# the simulated data is skewed towards higher incomes, which is pretty much 
# representative of the true distribution of income in a population

n <- 1e4
scale <- 5e4  # increase the scale parameter
income <- round(rbeta(n = n, shape1 = 5, shape2 = 2) * scale, 2)  # modify the shape parameters

# Plot the resulting curve
ggplot(data.frame(x = income), aes(x = x)) +
  geom_histogram(color = "#0065BD", fill = "#0065BD", alpha = 0.5, bins = 100) +
  scale_x_continuous(breaks = seq(0, scale, 1e3)) + 
  labs(x = "Gross income", y = "Counts") + 
  theme_minimal()

# Compute share of total income
total_income <- sum(income)
share <- income / total_income


income_s <- sort(income)
group <- c("Lower 1%", "Lower 50%", "Top 10%", "Top 1%")
p <- c(.1, .5, .9, .99)

boundary <- round(income_s[p*n], 0)

low10_m <- mean( income_s[c(1:(.1*n))] )
low50_m <- mean( income_s[c(1:(.5*n))] )
top10_m <- mean( income_s[c((.9*n):n)] )
top1_m <- mean( income_s[c((.99*n):n)] )

means <-  round( c(low10_m, low50_m, top10_m, top1_m) , 0)

income_summary <- data.frame(group, boundary, means)
income_summary

##       group boundary means
## 1  Lower 1%      618   398
## 2 Lower 50%     1865  1073
## 3   Top 10%     4014  4979
## 4    Top 1%     6125  6737


# Considering the above summary table and what you know or
# assume about how income is distributed in a population, 
# do you still think the simulated data is realistic? Change
# the scale and shape parameters in the function rbeta to simulate
# new data and generate new plots and summaries.

# Yes I think it is realistic

library(ggplot2)

# Set the parameters for the beta distribution
alpha <- 2
beta <- 5

# Generate a sample of 10,000 incomes from the beta distribution
incomes <- rbeta(10000, alpha, beta)

# Plot the distribution of incomes
ggplot(data.frame(x = incomes), aes(x = x)) +
  geom_histogram(aes(y = ..density..), bins = 50, color = "black", fill = "white") +
  stat_function(fun = dbeta, args = list(shape1 = alpha, shape2 = beta), color = "blue", size = 1) +
  labs(title = "Income Distribution", x = "Income", y = "Density")

# Exercise 3: Working with data frames

# Create a data frame with 5 variables and 5 rows.
# Computes the arithmetic mean of each variable in the data set 
# and store all 5 means in one vector.
# Bonus: If you copied and pasted the code for each variable, 
# try to come up with a solution, that omits copy and pasting 
# and does the entire operation in one line. Make sure that the
# output shows only the computed names and not also the respective
# variable names.


df <- data.frame(
  var1 = c(1, 2, 3, 4, 5),
  var2 = c(6, 7, 8, 9, 10),
  var3 = c(11, 12, 13, 14, 15),
  var4 = c(16, 17, 18, 19, 20),
  var5 = c(21, 22, 23, 24, 25)
)
library(dplyr)

df[,'var1'] %>% mean

means <- apply(df, 2, mean)
means


library(tidyverse)

names(diamonds) 

View(diamonds)
diamonds_ideal1 <- diamonds[which(diamonds$cut == "Ideal"),]
diamonds_ideal1

help(filter)

diamonds_ideal2 <- diamonds %>%
  filter(cut == "Ideal")
diamonds_ideal2

diamonds_ideal3 <- diamonds %>%
  filter(cut == "Ideal") %>%
  select(carat, cut, price)
diamonds_ideal3


