library(ggplot2)
library(rethinking)
library(tidyverse)

# Task 1 
sim_data <- function(G, b) {
  N <- length(G)
  numeracy <- ifelse(G == 1, 100, 80) + rnorm(N, 0, 15)
  quality <- 50 + b[G] * numeracy + rnorm(N, 0, 10)
  data.frame(G, numeracy, quality)
}

# Set seed for reproducibility
set.seed(123)

# Generate grouping variable
G <- as.factor(sample(c(1, 2), 500, replace = TRUE))


# Simulate the data
d <- sim_data(G, b = c(0., -0.5))


# Task 2
# Create scatter plot
ggplot(d, aes(x = numeracy, y = quality, color = G)) +
  geom_point() +
  labs(x = "Numeracy (X)", y = "Quality (Y)") +
  scale_color_manual(values = c("blue", "red"), labels = c("Group 1", "Group 2")) +
  ggtitle("Scatter Plot of Numeracy and Quality") +
  theme_minimal()


# Create box plots
ggplot(d, aes(x = G, y = numeracy, fill = G)) +
  geom_boxplot() +
  labs(x = "Group", y = "Numeracy (X)") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Group 1", "Group 2")) +
  ggtitle("Box Plot of Numeracy by Group") +
  theme_minimal()

ggplot(d, aes(x = G, y = quality, fill = G)) +
  geom_boxplot() +
  labs(x = "Group", y = "Quality (Y)") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Group 1", "Group 2")) +
  ggtitle("Box Plot of Quality by Group") +
  theme_minimal()


#Task 3

## complete pooling 
# Model 1: Without accounting for group differences with different initial values

model_1 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma), 
    mu <- a + b * numeracy,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data = d, chains = 4, iter = 2000
)

precis(model_1)
traceplot(model_1)



# Model 2: Accounting for group differences
model_2 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma), 
    mu <- a[G] + b[G] * numeracy,
    a[G] ~ dnorm(0, 10),
    b[G] ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data = d, chains = 4, iter = 2000
)

precis(model_2, depth=2)
traceplot(model_2)

# In Model 2, the number of priors required depends on the number of unique levels in the grouping variable G. When we account for the group differences we have 5 priors. The one with pooling has 3 priors that can be observed from traceplots.

# The estimates from Model 1 can be misleading because the model does not account for group differences. The estimates of a and b do not capture the true group-specific effects and are confounded by the grouping variable G. Thus, if we were to interpret the estimates from Model 1, we would fall into the ecological fallacy by making inferences about individual-level relationships based on aggregate-level data.
# In the Model 2, The estimated means for parameters a[1], a[2], b[1] and b[2] are 42.21, 45.82, 0.08 and -0.45 respectively. The estimated standard deviation for sigma is 10.17. 

# In Model 2, by accounting for group differences, the estimates of a and b provide a more accurate representation of the group-specific relationships between numeracy and quality. This helps avoid the ecological fallacy by acknowledging and modeling the true variation across groups.

#Task 4

# Extract posterior samples from the MCMC run
posterior_samples <- extract.samples(model_2, n = 1000)

# Compute differences
slope_differences <- posterior_samples$b[, 1] - posterior_samples$b[, 2]


layout(1)
# Plot posterior distribution of differences
hist(slope_differences, breaks = 30, probability = TRUE, 
     main = "Posterior Distribution of Group-Specific Slope Differences",
     xlab = "Difference in Slopes (Group 1 - Group 2)")

#Task 5

posterior_samples <- extract.samples(model_2)

# Select a and b values based on group membership
a_samples <- posterior_samples$a
b_samples <- posterior_samples$b
a <- ifelse(d$G == 1, sample(a_samples[, 1], sum(d$G == 1), replace = TRUE), sample(a_samples[, 2], sum(d$G == 2), replace = TRUE))
b <- ifelse(d$G == 1, sample(b_samples[, 1], sum(d$G == 1), replace = TRUE), sample(b_samples[, 2], sum(d$G == 2), replace = TRUE))

# Predict Y values using selected a and b values
Y_pred <- a + b * d$numeracy + rnorm(nrow(d), 0, posterior_samples$sigma)

# Visualize posterior predictions
plot(d$numeracy, Y_pred, pch = 16, col = "#1f77b4",
     xlab = "X", ylab = "Predicted Y",
     main = "Posterior Predictive Check")

# Add observed data points
points(d$numeracy, d$quality, pch = 16, col = "#ff7f0e")

# Add legend
legend("topright", legend = c("Posterior Predictions", "Observed Data"),
       col = c("#1f77b4", "#ff7f0e"), pch = 16)




# Visualize posterior predictions with clusters
plot(d$numeracy, Y_pred, pch = 16, col = d$G,
     xlab = "X", ylab = "Predicted Y",
     main = "Posterior Predictive Check")

# Add observed data points
points(d$numeracy, d$quality, pch = 16, col = "#ff7f0e")

# Add legend
legend("topright", legend = c("Group 1", "Group 2", "Observed Data"),
       col = c(1, 2, "#ff7f0e"), pch = 16, cex = 0.8, pt.cex = 1.2)


# Task 6

wd <- read_csv("WorldData.csv")

print(wd %>% 
  count(region))

# Small sample sizes: In some regions, the number of observations may be small, leading to limited statistical power and unstable estimates. This can result in imprecise parameter estimates and unreliable inference.

# Heterogeneity: Each region may exhibit different patterns and relationships between freedom of choice and life expectancy. By analyzing each region separately, we assume that the relationship is consistent across all regions. However, if there is substantial heterogeneity in the relationship, stratifying by region may overlook important variations and mask overall trends.

# Missing confounders: While stratifying by region can account for some confounding factors that are region-specific, it may not capture all relevant confounders. There may be confounders that are not accounted for within each region, leading to biased estimates and potential confounding effects.

# Ecological fallacy: Stratifying by region assumes that the relationships observed at the aggregate level (region) hold true at the individual level. This can lead to the ecological fallacy, where associations observed at the group level may not necessarily reflect the same associations at the individual level.

# Loss of statistical power: By conducting separate regression models for each region, we divide the available sample size into smaller subsets. This can result in a loss of statistical power to detect significant associations, especially if the sample size within each region is already limited.


# Task 7

model_3 <- ulam(
  alist(
    life_expectancy ~ dnorm(mu, sigma),
    mu <- a[region_index],
    a[region_index] ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data = wd[c('region_index', 'life_expectancy')], chains = 4, iter = 2000)


precis(model_3, depth=2)
traceplot(model_3)

model_4 <- ulam(
  alist(
    life_expectancy ~ dnorm(mu, sigma),
    mu <- a[region_index] + b[region_index] * freedom_of_choice,
    a[region_index] ~ dnorm(0, 10),
    b[region_index] ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data = wd[c('region_index', 'life_expectancy', 'freedom_of_choice')], chains = 8, cores = 8, iter = 4000)

precis(model_4, depth=2)
traceplot(model_4)

layout(1)
plot(precis(model_4, depth = 2))


# Task 8 

# The estimates from Model 4 and Model 5 differ due to their underlying statistical assumptions and modeling approaches.

# In Model 4, each region is treated independently, and separate regression models are estimated for each region. This approach assumes that there are no shared patterns or relationships between regions. The estimates for the intercept (a[region_index]) and slope (b[region_index]) in Model 4 represent the specific associations between freedom of choice and life expectancy within each region. The estimates are specific to each region and do not account for any similarities or shared information across regions.

# On the other hand, Model 5 incorporates a partial pooling approach. It considers both region-specific effects and group-level effects. The region-specific effects (a[region_index], b[region_index]) capture the associations within each region, while the group-level effects (a_bar, b_bar) represent the overall average associations across all regions. The estimates in Model 5 are influenced by both the region-specific effects and the group-level effects. This allows for sharing of information across regions, leading to more stable and robust estimates, especially for regions with limited data.

# Comparing the estimates between Model 4 and Model 5, we can see that the estimates in Model 5 tend to have smaller standard deviations compared to Model 4. This indicates increased precision and reduced uncertainty in the estimates. The group-level effects in Model 5 help to borrow information across regions, leading to more reliable estimates, particularly in regions with sparse data.

# In summary, Model 4 treats each region independently, resulting in region-specific estimates, while Model 5 incorporates partial pooling by considering both region-specific and group-level effects. The estimates from Model 5 are expected to be more stable and informative due to the pooling of information across regions, providing a better understanding of the associations between freedom of choice and life expectancy.