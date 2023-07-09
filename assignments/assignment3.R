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
hist(aging$Age)
# By plotting the age data, we observe two distinct groups: one consisting of individuals under 30 and the other comprising individuals over 60.
plot(density(aging$RiskSeeking), main = "Density Plot of RiskSeeking")
hist(aging$RiskSeeking)
# By examining the plot, we can observe that all the values fall within the range of 0.25 and 0.75 potentially with normally or beta distributed
plot(density(aging$DecisionQuality), main = "Density Plot of DecisionQuality")
hist(aging$DecisionQuality)
# We can observe more a normal distribution
plot(density(aging$Speed), main = "Density Plot of Speed")
plot(aging$Speed)
hist(aging$Speed)
# From the plot, we can potentially identify two primary speed groups. On the other hand, the histogram suggests a distribution that resembles a normal distribution with a considerable variance.
plot(density(aging$NegAffect), main = "Density Plot of NegAffect")
hist(aging$NegAffect)
# The distribution of this variable appears to resemble that of a beta distribution.
plot(density(aging$Numeracy), main = "Density Plot of Numeracy")
hist(aging$Numeracy)
# The numeracy variable appears to follow a beta distribution, with a concentration of values skewed towards 1 (alpha > beta).


# Overall, based on the information provided, we see a mix of roughly symmetrical
# distributions (RiskSeeking, DecisionQuality, Speed) and slightly skewed distributions
# (Age, Numeracy NegAffect). 


plot(density(aging$Age), main = "Density Plot of Age")

mean_age <- mean(aging$Age)
median_age <- median(aging$Age)

rug(c(mean_age, median_age), side = 1, lwd = 2)

# Task 3: Add a new variable AG to group people into 2 age groups. Mean or Medan either of them can be used
aging$AG <- ifelse(aging$Age <= median(aging$Age), 1, 2)

# 4
# Build and estimate a Gaussian model for the variables DecisionQuality and
# RiskSeeking (separately) using the quap() function from the rethinking package.

# For both variables, normal distribution seems to be a good fit. Assuming we had a prior knowledge of how variables are distributed we can fit a gausian models with very low standard deviations.
# Fit the model using quap()
set.seed(1234)
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

# The findings suggest that, on average, approximately 65% of respondents in the survey opted for the choice that offered a higher expected payoff, as indicated by the mean (mu) of decision quality. The estimated standard deviation (sigma) of 0.09 for decision quality suggests that there was relatively little variation among participants, implying a high level of confidence in their tendency to select the option with the higher expected payoff.

# In contrast, the estimated mean coefficient (mu) of 0.47 for the variable "Risk Seeking" indicates that less than half of the observations displayed a preference for the less likely option, indicating a tendency towards risk aversion or, at the very least, a lack of risk-seeking behavior. The corresponding sigma value of 0.09 also suggests low variability, indicating a consistent level of risk-seeking behavior observed across the participants.

#Plotting the sample distribution to see if the model is a reasonable fit:

dq_samples %>%  ggplot(aes(x = mu)) +
  geom_density(color = "#552583", linewidth = 1, alpha = .1) +
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()

rs_samples %>%  ggplot(aes(x = mu)) +
  geom_density(color = "#552583", linewidth = 1, alpha = .1) +
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()


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
HPDI(difference, prob = 0.95)


# 6

aging$Age <- standardize(aging$Age)
aging$RiskSeeking <- standardize(aging$RiskSeeking)
aging$DecisionQuality <- standardize(aging$DecisionQuality)
aging$Speed <- standardize(aging$Speed)
aging$NegAffect <- standardize(aging$NegAffect)
aging$Numeracy <- standardize(aging$Numeracy)

# 7
plot(aging$Numeracy, aging$DecisionQuality)
# There is more or less a positive relationship that can be expected however with very high variance

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

plot(aging$Speed, aging$DecisionQuality)
# The relation seems to be very less significant this time, with very high variance

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

# There might be more or less a negative relationship

model_dq_neg <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_neg * NegAffect,
    a ~ dnorm(0, 1),
    b_neg ~ dnorm(-0.5,0.5),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_dq_neg)

# Numeracy (b = 0.36) shows a positive relationship with Decision Quality, indicating that better numeric abilities are associated with higher expected value in decision-making.

# Speed (b = 0.22) also exhibits a positive relationship with Decision Quality, suggesting that faster cognitive processing relates to better decision outcomes.

# NegAffect (b = -0.15) demonstrates a negative relationship with Decision Quality, indicating that higher levels of negative emotions are associated with lower decision quality.


# We might expect a slight negative relationship

model_rs_num <- quap(
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a + b_num * Numeracy,
    a ~ dnorm(0, 1),
    b_num ~ dnorm(-0.5,0.25),
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

# We can hypothesize that the Numeracy variable acts as a confounder for both Speed and DecisionQuality. Since Numeracy involves the ability to work with numeric information, it is expected to positively influence both processing speed and the quality of decisions. In task 7, we have already demonstrated the direct influence of Numeracy on DecisionQuality (Num --> DQ).

# Additionally, we anticipate that Speed serves as a mediator between Numeracy and DecisionQuality (Num --> Speed --> DQ). Speed is likely to play a role in influencing DecisionQuality, acting as an intermediate factor between Numeracy and the quality of decisions.

# To explore these relationships further, we will construct a model that estimates the mean of DecisionQuality using Speed and Numeracy as linear predictors.

model_speed_num_dq <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sigma),
    mu <- a + b_speed * Speed + b_num * Numeracy,
    a ~ dnorm(0, 1),
    b_speed ~ dnorm(0.5,0.5),
    b_num ~ dnorm(0.5,0.5),
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
    b_speed ~ dnorm(0.5,0.5),
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
    b_num ~ dnorm(0.5,0.5),
    sigma ~ dexp(1)
  ),
  data = aging
)

precis(model_num_dq)

# # We can notice how b_num hasn't much changed from the model with only Numeracy as predictor (b=0.36), while the b_spd has decreased (0.12 vs 0.22), probably stating the higher relevance of Numeracy when controlling for the Numeracy variable. 

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





dat <- list(
  Numeracy = aging$Numeracy,
  AG = aging$AG,
  DecisionQuality = aging$DecisionQuality,
  RiskSeeking= aging$RiskSeeking
  
)


model1 <- ulam(  
  alist(
  DecisionQuality ~ dnorm(mu, sigma),
  mu <- a[AG] + b_num[AG] * Numeracy,
  a[AG] ~ dnorm(0, 1),
  b_num[AG] ~ dnorm(0, 1),
  sigma ~ dexp(1)
),
data = dat, chains = 4, cores = 4)

precis(model1, depth=2)
traceplot(model1)



model2 <- ulam(  
  alist(
    RiskSeeking ~ dnorm(mu, sigma),
    mu <- a[AG] + b_num[AG] * Numeracy,
    a[AG] ~ dnorm(0, 1),
    b_num[AG] ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = dat, chains = 4, cores = 4)

precis(model2, depth=2)
traceplot(model2)


