library(tidyverse)

# 2 Dice

# 1. Create 2 vectors representing the sample spaces of both dice throws.

vectore_1 <- 1:6
vectore_2 <- 1:6


# 2. Use the 2 vectors and the expand_grid() function from the tidyverse package to generate
# a data frame representing all 36 possible outcomes of throwing both dice. Each row should
# represent one of the possible combinations.

df <- expand.grid(d1 = vectore_1, d2 = vectore_1)
df


# 3. Add a column to the data frame that specifies the probability of each possible outcome
# (combination). 

df$Probability = 1/36


# 4. Add a column to the data frame that specifies the sum across both dice for each possible
# outcome (combination).

df$Sum = df$d1 + df$d2


# 5. Compute the probability that the sum is ≥ 7 , given the first dice shows 3.

# Filter the df based on the condition (d1 = 3)
subset_data <- subset(df, d1 == 3)

# Calculate the conditional probability
conditional_probability <- sum(subset_data[subset_data$Sum >= 7,]$Probability) / sum(subset_data$Probability)

# Print the conditional probability
print(conditional_probability)


# 6. Compute the probability that the sum lies between 4 and 9.

# Filter the df based on the condition (Sum between 4 and 9)
subset_data <- subset(df, Sum >= 4 & Sum <= 9)

# Calculate the probability
probability <- sum(subset_data$Probability)

# Print the probability
print(probability)


# 7. What is the probability of the most probable sum?

# Group the df data frame by sum and calculate the sum of probabilities for each sum value
grouped_data <- df %>% 
  group_by(Sum) %>% 
  summarize(Total_Prob = sum(Probability))

# Find the maximum probability and the corresponding sum
max_probability <- max(grouped_data$Total_Prob)
most_probable_sum <- grouped_data$Sum[grouped_data$Total_Prob == max_probability]


# Print the most probable sum and its probability
print(paste("Most Probable Sum:", most_probable_sum, " Probability:", max_probability))


# 8. For each possible probability of delay, compute the probability distribution over all
# possible numbers of delays using the dbinom() function.

# Set the probabilities of delay
probabilities <- seq(0, 1, 0.1)

# Number of trains
num_trains <- 10

# Calculate the probability distributions using outer()
probability_matrix <- outer(probabilities, 0:num_trains, function(p, d) dbinom(d, num_trains, p))

# Create the data frame with row and column names
df <- as.data.frame(probability_matrix)
rownames(df) <- probabilities
colnames(df) <- 0:num_trains


# 9. Use the code below to generate train ride data. Use the dbinom() function to compute
# the likelihood of the data (observed number of delays), given p=0, p=0.1, p=0.2, p=0.3,
# p=0.4, p=0.5, p=0.6, p=0.7, p=0.8, p=0.9, p=1.

sim_rides <- function(N, p){
  sample(c("L", "O"), size=N, replace=TRUE, prob=c(p, 1-p))
}
set.seed(1237)
obs <- sim_rides(10, .3)
obs

####


probabilities <- seq(0, 1, 0.1)
likelihoods <- dbinom(sum(obs == "L"), size = length(obs), prob = probabilities)

likelihoods


# 10. Assume the below prior probabilities. Use the Bayes’ rule to calculate the posterior
# probability using the prior probabilities and the likelihoods that you computed. (2 points)

prior <- c(0.000, 0.004, 0.041, 0.123, 0.209, 0.246, 0.209, 0.123, 0.041, 0.004, 0.000)

posterior <- prior * likelihoods
posterior <- posterior / sum(posterior)

posterior

# Plotting
plot(prior, type = "b", ylim = c(0, max(prior, posterior)), xlab = "Probabilities (0 - 1)", ylab = "Probability Density", 
     main = "Prior and Posterior Probabilities")
lines(posterior, type = "b", col = "red")
legend("topright", legend = c("Prior", "Posterior"), col = c("black", "red"), lty = 1, bty = "n")


