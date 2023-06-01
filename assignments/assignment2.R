library(tidyverse)
# Task 1
# Define the given probabilities
P_A <- 1/4
P_B <- 1/2
P_C <- 1/4
P_S_A <- 0.7
P_S_B <- 0.5
P_S_C <- 0.8

# Calculate the numerator of the posterior probability
numerator <- P_A * choose(10, 6) * (P_S_A^6) * ((1 - P_S_A)^4)

# Calculate the denominator of the posterior probability
denominator <- numerator + (P_B * choose(10, 6) * (P_S_B^6) * ((1 - P_S_B)^4)) + (P_C * choose(10, 6) * (P_S_C^6) * ((1 - P_S_C)^4))

# Calculate the posterior probability
posterior <- numerator / denominator

print(posterior)

######## Or

compute_post <- function(obs, poss) {
  N <- length(obs)
  L <- sum(obs == "P")
  
  likelihood <- dbinom(L, N, prob = poss$theta)
  
  posterior <- likelihood * poss$prior
  
  posterior_norm <- posterior / sum(posterior)
  
  tibble(poss, lh = round(likelihood, 3), post = round(posterior_norm, 3))
}

obs <- c(rep("P", 6), rep("N", 4))  # 6 positive and 4 negative reviews
poss <- tibble(
  company = c("A", "B", "C"),
  theta = c(0.70, 0.50, 0.80),
  prior = c(1/4, 1/2, 1/4)
)


compute_post(obs, poss)



# Task 2


additional_obs <- c(rep("P", 9), "N")  # 9 positive and 1 negative reviews

compute_post_updated <- function(obs, prev_post) {
  N <- length(obs)
  L <- sum(obs == "P")
  
  likelihood <- dbinom(L, N, prob = prev_post$theta)
  
  posterior <- likelihood * prev_post$post
  
  posterior_norm <- posterior / sum(posterior)
  
  tibble(company = prev_post$company, theta = prev_post$theta,
         prior = prev_post$post, lh = round(likelihood, 3), post = round(posterior_norm, 3))
}


prev_post <- compute_post(obs, poss)  # Previous posterior probabilities

new_post = compute_post_updated(additional_obs, prev_post)
print(new_post)


print(new_post[3,'post'] - new_post[3,'prior'])


# Task 3

# Define the probabilities and events
P_D1_given_A <- 0.10  # Probability of defective products given shipment from Factory A
P_D1_given_B <- 0.20  # Probability of defective products given shipment from Factory B
P_A <- 0.5  # Probability of shipment from Factory A
P_B <- 0.5  # Probability of shipment from Factory B

# Calculate the probability
P_D1 <- P_D1_given_A * P_A + P_D1_given_B * P_B

# P(D2|D1) = P(D2) = P(D1) since they are independent
P_D2_given_D1 <- P_D1

P_D2_given_D1

# Task 4

P_A <- 0.5  # Prior probability of the shipment being from Factory A
P_B <- 0.5  # Prior probability of the shipment being from Factory B
P_PosA_given_A <- 0.8  # Probability of a positive test given the shipment is from Factory A
P_PosA_given_B <- 0.65  # Probability of a positive test given the shipment is from Factory B
P_PosB_given_A <- 0.2
P_PosA_given_B <- 0.35

# P(A|Al_A) = (P(Al_A|A) * P(A)) / ((P(Al_A|A) * P(A)) + (P(Al_A|B) * P(B)))

P_PosA <- P_PosA_given_A * P_A + P_PosA_given_B * P_B

P_A_given_PosA <- (P_PosA_given_A * P_A) / P_PosA

P_A_given_PosA

# Task 4 - 5 Countinued 


# P(A) = 0.5 (Prior probability of product from Factory A)
# P(B) = 0.5 (Prior probability of product from Factory B)
# P(D|A) = 0.1 (Probability of defective product given it is from Factory A)
# P(D|B) = 0.2 (Probability of defective product given it is from Factory B)
# P(Al_A|A) = 0.8 (Probability algorithm classifies product from Factory A as A)
# P(Al_A|B) = 0.35 (Probability algorithm classifies product from Factory B as A)


# P(A|D, Al_A) = (P(D|A) * P(A) * P(Al_A|A)) / ((P(D|A) * P(A) * P(Al_A|A)) + (P(D|B) * P(B) * P(Al_A|B)))

P_A_given_PosA_D = (P_D1_given_A * P_A * P_PosA_given_A) / ((P_D1_given_A * P_A * P_PosA_given_A) + (P_D1_given_B*P_B*P_PosA_given_B))

P_A_given_PosA_D

# Task 6
# Assuming a prior belief that the proportion of land on Earth is normally distributed
# and centered around 0.3 with a sd 0.1

prior_mean <- 0.3
prior_sd <- 0.1  

# Define the prior distribution using the Beta distribution
prior_distribution <- function(x) dnorm(x, prior_mean, prior_sd)

# Plot the prior distribution
curve(prior_distribution, from = 0, to = 1, xlab = "Proportion of Land", ylab = "Density", main = "Prior Distribution")


# Task 7
# Generate 10,000 random samples from the prior distribution
set.seed(123)  # Set a seed for reproducibility
sample <- rnorm(10000, prior_mean, prior_sd)

head(sample)

# Task 8

# Generate 10,000 random samples from the prior distribution
set.seed(123)  # Set a seed for reproducibility
sample <- rnorm(10000, prior_mean, prior_sd)

# Compute prior probabilities
prop <- seq(0, 1, length.out = 12)
priors <- vector("numeric", length(prop))
for (i in seq_along(prop)) {
  priors[i] <- round(sum(sample >= prop[i] & sample < prop[i + 1]) / 10000, 2)
}

# Define the observed data
observed_land <- 26
total_tosses <- 100

# Compute posterior probabilities using Bayes' theorem

posteriors <- priors * dnorm(observed_land/total_tosses, prior_mean, prior_sd)

# Normalize the posterior probabilities to sum up to 1
posteriors <- posteriors / sum(posteriors)

# Create a data frame with proportion of land and corresponding prior and posterior probabilities
poss <- data.frame(prop_L = seq(0, 1, by = 0.1),
                   prior = priors[1:11],
                   posterior = round(posteriors[1:11], 2))

# Print the result
print(poss)


# Task 9

posterior_samples <- sample(prop, 1000, replace = TRUE, prob = posteriors)

# Task 10

# Predict outcomes of 100 globe tosses for each sample
predictions <- rbinom(1000, total_tosses, posterior_samples)

# Plot the posterior predictive distribution
hist(predictions, breaks = "FD", freq = FALSE, main = "Posterior Predictive Distribution",
     xlab = "Number of Lands", ylim = c(0, 0.03))


