pacman::p_load(tidyverse, rethinking)

# 1
# Load the data set Aging.csv and store it in an object.
aging = read_csv('Aging.csv')

# 2
# Delete all observations (rows) containing missing values, indicated by an NA.

aging = aging[complete.cases(aging),]
# or aging = na.omit(aging)

# 3
# Inspect the distributions of all variables. 
summary(aging)

plot(density(aging$Age), main = "Density Plot of Age")
plot(density(aging$RiskSeeking), main = "Density Plot of RiskSeeking")
plot(density(aging$DecisionQuality), main = "Density Plot of DecisionQuality")
plot(density(aging$Speed), main = "Density Plot of Speed")
plot(density(aging$NegAffect), main = "Density Plot of NegAffect")
plot(density(aging$Numeracy), main = "Density Plot of Numeracy")

aging$NegAffect

# Overall, based on the information provided, we see a mix of roughly symmetrical
# distributions (RiskSeeking, DecisionQuality, Speed) and slightly skewed distributions
# (Age, Numeracy NegAffect). 


plot(density(aging$Age), main = "Density Plot of Age")

mean_age <- mean(aging$Age)
median_age <- median(aging$Age)

rug(c(mean_age, median_age), side = 1, lwd = 2)
legend("topright", legend = c("Mean", "Median"), pch = 15, lwd = 2, col = "black")


# Task 3: Add a new variable AG to group people into 2 age groups. Mean or Medan either of them can be used
aging$AG <- ifelse(aging$Age <= median(aging$Age), 1, 2)

# 4
# Build and estimate a Gaussian model for the variables DecisionQuality and
# RiskSeeking (separately) using the quap() function from the rethinking package.

# Fit the model using quap()

model_dq <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- dnorm(0.6, 0.1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_dq)
# Psychological attributes usually follow a normal distributin.
# It makes sense to use normal priors for mean of the distribution.
# SD must be larger than 0 so using dexp(1) makes sense.

model_rs <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- dnorm(0.5, 0.1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_rs)

set.seed(123)
# Extract samples and calculate HPDI for Decision Quality
dq_samples <- extract.samples(model_dq, 1000)
dq_hpdi <- HPDI(dq_samples$mu, prob = 0.95)
dq_hpdi

# Extract samples and calculate HPDI for Risk Seeking
rs_samples <- extract.samples(model_rs, 1000)
rs_hpdi <- HPDI(rs_samples$mu, prob = 0.95)
rs_hpdi

# Decision Quality: The HPDI for the mean parameter (mu) is [0.634, 0.666] with a 95%confidence level.
# Risk Seeking: The HPDI for the mean parameter (mu) is [0.455, 0.487] with a 95% confidence level.
# For Decision Quality, the average falls between 0.634 and 0.666, while for Risk 
# Seeking, it ranges from 0.455 to 0.487.

# In summary, the analysis suggests that Decision Quality tends to be relatively high, 
#  Risk Seeking behavior being exhibited by individuals on average. 
# However, it's important to consider the specific details of the model and data used
# when interpreting these results.

# 5

model_young <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- dnorm(0.6, 0.1),
    sigma ~ dexp(1)
  ),
  data = subset(aging, AG == 1)
)

model_old <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- dnorm(0.6, 0.1),
    sigma ~ dexp(1)
  ),
  data = subset(aging, AG == 2)
)


samples_young <- extract.samples(model_young)$mu
samples_old <- extract.samples(model_old)$mu

difference <- samples_young - samples_old

hist(difference, breaks = 30, col = "lightblue", xlab = "Difference in Means",
     main = "Distribution of Difference in Means")


# 6

aging$Age <- standardize(aging$Age)
aging$RiskSeeking <- standardize(aging$RiskSeeking)
aging$DecisionQuality <- standardize(aging$DecisionQuality)
aging$Speed <- standardize(aging$Speed)
aging$NegAffect <- standardize(aging$NegAffect)
aging$Numeracy <- standardize(aging$Numeracy)

model_dq_num <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_num * Numeracy,
    a ~ dnorm(0, 1),
    b_num ~ dunif(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_dq_num)

model_dq_speed <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_speed * Speed,
    a ~ dnorm(0, 1),
    b_speed ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_dq_speed)

model_dq_neg <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_neg * NegAffect,
    a ~ dnorm(0, 1),
    b_neg ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_dq_neg)

model_rs_num <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a + b_num * Numeracy,
    a ~ dnorm(0, 1),
    b_num ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_rs_num)

model_rs_speed <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a + b_speed * Speed,
    a ~ dnorm(0, 1),
    b_speed ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_rs_speed)

model_rs_neg <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a + b_neg * NegAffect,
    a ~ dnorm(0, 1),
    b_neg ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_rs_neg)

# Using standardized variables in this task provides two key advantages:
  
#   Comparability: Standardizing variables places them on a common scale, with a mean of 0 and a standard deviation of 1. This enables direct comparison of the magnitude of coefficients across different predictors. It allows for a fair assessment of the relative importance of predictors in influencing the outcome variables.

#   Interpretability: Standardized coefficients offer a measure of effect size in terms of standard deviations. This facilitates the interpretation of the impact of predictors on the outcome variables. It helps identify which predictors have a stronger or weaker effect, irrespective of the original variables' scales or units.

# In relation to DecisionQuality, Numeracy demonstrates the strongest average total effect (b_num = 0.36). This implies that, on average, a one standard deviation increase in Numeracy is associated with a 0.36 standard deviation increase in DecisionQuality.

# For RiskSeeking, Negative Affect exhibits the strongest average total effect (b_neg = -0.28). This suggests that, on average, a one standard deviation increase in Negative Affect is associated with a 0.28 standard deviation decrease in RiskSeeking.

# Therefore, Numeracy exerts the strongest total effect on DecisionQuality, while Negative Affect exerts the strongest total effect on RiskSeeking.


# 8

model_speed_num_dq <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_speed * Speed + b_num * Numeracy,
    a ~ dnorm(0, 1),
    b_speed ~ dnorm(0, 1),
    b_num ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_speed_num_dq)

model_speed_dq <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_speed * Speed,
    a ~ dnorm(0, 1),
    b_speed ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_speed_dq)

model_num_dq <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_num * Numeracy,
    a ~ dnorm(0, 1),
    b_num ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_num_dq)

# The coefficient estimate for the b_speed parameter represents the direct effect of speed on decision quality, while the coefficient estimate for the b_num parameter represents the direct effect of numeracy on decision quality.

# If the coefficient estimate for b_speed remains significant and loses significance when numeracy (b_num) is added to the model, it suggests that numeracy may act as a mediator between speed and decision quality. This indicates that the effect of speed on decision quality is partially or fully explained by the mediating role of numeracy.

# Additionally, you can examine the confidence intervals and p-values associated with the coefficient estimates to assess the significance and strength of the mediation effect.



aging$Numeracy_fact = factor(aging$Numeracy, sort(unique(aging$Numeracy)))

ggplot(aging, aes(y = DecisionQuality, x = Speed, color = Numeracy_fact)) + 
  geom_point(alpha = .5) +
  theme_minimal()

p <- ggplot(aging, aes(y = Speed, x = Numeracy_fact)) +
  geom_point(alpha = 0.5) +
  xlab("Numeracy") +
  ylab("Speed")

# Tilt x-axis ticks by 45 degrees
p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
