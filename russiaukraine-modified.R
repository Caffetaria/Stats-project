#Sandra Zelen, Aria Vikram 
#Probability and Statistics Project
#December 2023

# Loading in the necessary libraries
library(dplyr)
library(ggplot2)
install.packages("qcc") # Only needed if not already installed
library(qcc)

# Importing the datasets
russian_losses <- read.csv("/Users/ariavikram/Downloads/losses_russia.csv")
ukrainian_losses <- read.csv("/Users/ariavikram/Downloads/losses_ukraine.csv")

# Examining file content
str(russian_losses)
str(ukrainian_losses)

# Summary statistics
summary(russian_losses)
summary(ukrainian_losses)

# Drop unnecessary columns for Russian Losses
drop <- c("sub_model","sunk","destroyed.in.a.non.combat.related.incident") 
russian_losses <- russian_losses[,!(names(russian_losses) %in% drop)] 

# Drop unnecessary columns for Ukrainian Losses
drop <- c("sub_model","sunk","destroyed.in.a.non.combat.related.incident") 
ukrainian_losses <- ukrainian_losses[,!(names(ukrainian_losses) %in% drop)] 


# Visualize distributions
# The x-axis = range of losses, y-axis = frequency of occurrences within each range
hist(russian_losses$losses_total, main = "Russian Losses Distribution", xlab = "Losses")
hist(ukrainian_losses$losses_total, main = "Ukrainian Losses Distribution", xlab = "Losses")

###############################

#PART 1: CALCULATES THE PROBABILITY OF A RUSSIAN TANK BEING LOST OUT OF THE RUSSIAN WEAPONS

################################

# Calculate the total number of tanks lost
total_russian_tanks_lost <- sum(russian_losses$losses_total[russian_losses$equipment == "Tanks"], na.rm = TRUE)

# Calculate the total number of Russian items lost
total_russian_items_lost <- sum(russian_losses$losses_total , na.rm = TRUE)

# Calculate the probability that a tank is lost out of all the weapons
prob_russian_tank_lost <- total_russian_tanks_lost / total_russian_items_lost

#The probability of a tank being lost
print(prob_russian_tank_lost)

###############################

#PART 2: CALCULATES THE PROBABILITY OF A RUSSIAN TANK BEING LOST OUT OF ALL THE WEAPONS ON BOTH SIDES

###############################

# Calculate the total number of Ukrainian items lost
total_ukrainian_items_lost <- sum(ukrainian_losses$losses_total , na.rm = TRUE)

#Calculate total items lost from both sides
total_loss_in_war <- sum(total_russian_items_lost, total_ukrainian_items_lost)

#Calculate the probability of a Russian tank being lost out of all the weapons from both sides
prob_russian_tank_lost_bothsides <- total_russian_tanks_lost / total_loss_in_war

print(prob_russian_tank_lost_bothsides)

###############################

#PART 3: EXAMPLE WITH BAYESIAN METHOD USING MONTE CARLO

###############################
# Assuming you have already defined total_russian_tanks_lost and total_russian_items_lost

# Prior belief (e.g., 50% chance of tank loss)
prior_prob <- 0.5

# Number of simulations
n_simulations <- 10000

# Initialize a vector to store the results of the simulations
posterior_probs <- numeric(n_simulations)

for (i in 1:n_simulations) {
  # Generate observational data (simulated)
  observed_data <- rbinom(1, total_russian_items_lost, prior_prob)
  
  # Update belief based on data (Bayesian update)
  success_count <- observed_data + total_russian_tanks_lost
  trial_count <- total_russian_items_lost + 1
  
  # Draw from the posterior distribution
  posterior_probs[i] <- rbeta(1, success_count, trial_count - success_count)
}

# Visualize the results of the Monte Carlo simulation
hist(posterior_probs, main = "Posterior Probability Distribution of Tank Losses", xlab = "Probability")


###############################

#PART 4: Calculating the expectations for Russian and Ukrainian losses

###############################

# Calculate the expectation (mean) for Russia
expectation_russia <- mean(russian_losses$losses_total, na.rm = TRUE)

# Calculate the expectation (mean) for Ukraine
expectation_ukraine <- mean(ukrainian_losses$losses_total, na.rm = TRUE)

# Print the results
cat("Expectation for Russia:", expectation_russia, "\n")
cat("Expectation for Ukraine:", expectation_ukraine, "\n")

###############################

# We have to account for overall losses per unit, since just expectation doesn't give us too much relevant info

###############################

# Assuming 'equipment_size' is the column representing the size of military equipment
losses_per_unit_russia <- expectation_russia / total_russian_items_lost
losses_per_unit_ukraine <- expectation_ukraine / total_ukrainian_items_lost

# Print the results
cat("Losses per unit for Russia:", losses_per_unit_russia, "\n")
cat("Losses per unit for Ukraine:", losses_per_unit_ukraine, "\n")

################################

# TO DO: Interpreting this with a t-test

################################
# Extract losses for tanks for both countries
# Identify unique equipment categories
equipment_categories <- unique(c(russian_losses$equipment, ukrainian_losses$equipment))

# Initialize an empty list to store t-test results
t_test_results <- list()

# Loop through each equipment category and perform a t-test
for (category in equipment_categories) {
  # Extract losses for the current category from both datasets
  russian_category_losses <- russian_losses$losses_total[russian_losses$equipment == category, drop = TRUE]
  ukrainian_category_losses <- ukrainian_losses$losses_total[ukrainian_losses$equipment == category, drop = TRUE]
  
  # Perform a t-test if both countries have losses for the current category
  if (length(russian_category_losses) > 0 && length(ukrainian_category_losses) > 0) {
    t_test_results[[category]] <- t.test(russian_category_losses, ukrainian_category_losses, na.rm = TRUE)
  } else {
    t_test_results[[category]] <- NA  # Indicate NA if one or both countries do not have losses for the category
  }
}

# Print the t-test results
print(t_test_results)
###############################

#PART 5: EXAMPLE WITH POISSON DISTRIBUTION

###############################

# Assumption: Military losses occur at a constant rate, 
# And the number of losses in a fixed period follows a Poisson distribution.

lambda_russia <- mean(russian_losses$losses_total)
lambda_ukraine <- mean(ukrainian_losses$losses_total)

# Generate Poisson random variables
simulated_losses_russia <- rpois(1000, lambda = lambda_russia)
simulated_losses_ukraine <- rpois(1000, lambda = lambda_ukraine)

# Visualize the simulated losses
hist(simulated_losses_russia, main = "Simulated Military Losses - Russia", xlab = "Losses", col = "lightblue")
hist(simulated_losses_ukraine, main = "Simulated Military Losses - Ukraine", xlab = "Losses", col = "lightcoral")

#???????????????????????????????
# Perform a t-test to compare mean losses between Russia and Ukraine
t_test_result <- t.test(russian_losses$losses_total, ukrainian_losses$losses_total)
print(t_test_result)
###############################

#PART 6: EXAMPLE WITH NORMAL DISTRIBUTION

###############################
# Additional library for normality test
library(nortest)

# Initialize an empty list to store normality test results and confidence intervals
normality_test_results <- list()
confidence_intervals <- list()

# Loop through each equipment category
for (category in equipment_categories) {
  # Extract losses for the current category from both datasets
  russian_category_losses <- russian_losses$losses_total[russian_losses$equipment == category, drop = TRUE]
  ukrainian_category_losses <- ukrainian_losses$losses_total[ukrainian_losses$equipment == category, drop = TRUE]
  
  # Combine losses for normality test
  combined_losses <- c(russian_category_losses, ukrainian_category_losses)
  
  # Normality test (e.g., Anderson-Darling test)
  if (length(combined_losses) > 7) {
    normality_test_results[[category]] <- ad.test(combined_losses)
    
    # Confidence Interval for mean losses, assuming normal distribution
    mean_losses <- mean(combined_losses, na.rm = TRUE)
    std_dev <- sd(combined_losses, na.rm = TRUE)
    n <- length(combined_losses)
    error_margin <- qt(0.975, df=n-1) * std_dev / sqrt(n)
    confidence_interval <- c(mean_losses - error_margin, mean_losses + error_margin)
    confidence_intervals[[category]] <- confidence_interval
  } else {
    normality_test_results[[category]] <- NA
    confidence_intervals[[category]] <- NA
  }
}

# Print the normality test results and confidence intervals
print("Normality Test Results:")
print(normality_test_results)
print("Confidence Intervals for Mean Losses:")
print(confidence_intervals)


###############################

# Performs linear regression, modeling 'losses_total' against 'destroyed', and plots the fitted line.

###############################

# Perform linear regression
model <- lm(losses_total ~ destroyed, data = ukrainian_losses)

# Display the summary of the model
summary(model)

# Create a scatter plot with the line of best fit
plot(ukrainian_losses$destroyed, ukrainian_losses$losses_total, 
     main = "Scatter Plot with Linear Regression Line",
     xlab = "Destroyed", ylab = "Losses Total", pch = 16, col = "blue")

# Add the regression line to the scatter plot
abline(model, col = "red")


################################

# Performs linear regression, modeling 'losses_total' against 'damaged', and plots the fitted line.

##################################

# Perform linear regression
model <- lm(losses_total ~ damaged, data = ukrainian_losses)

# Display the summary of the model
summary(model)

# Create a scatter plot with the line of best fit
plot(ukrainian_losses$damaged, ukrainian_losses$losses_total, 
     main = "Scatter Plot with Linear Regression Line",
     xlab = "Damaged", ylab = "Losses Total", pch = 16, col = "blue")

# Add the regression line to the scatter plot
abline(model, col = "red")

################################

# Performs linear regression, modeling 'losses_total' against 'captured', and plots the fitted line.

##################################

# Perform linear regression
model <- lm(losses_total ~ captured, data = ukrainian_losses)

# Display the summary of the model
summary(model)

# Create a scatter plot with the line of best fit
plot(ukrainian_losses$captured, ukrainian_losses$losses_total, 
     main = "Scatter Plot with Linear Regression Line",
     xlab = "Captured", ylab = "Losses Total", pch = 16, col = "blue")

# Add the regression line to the scatter plot
abline(model, col = "red")

###############################

# Conducts ANOVA to assess manufacturer effects on losses in Russian and Ukrainian datasets.

###############################
# Perform ANOVA for Russian dataset
anova_result_russia <- aov(losses_total ~ manufacturer, data = russian_losses)

# Display the ANOVA table
summary(anova_result_russia)

# Perform ANOVA for Ukrainian dataset
anova_result_ukraine <- aov(losses_total ~ manufacturer, data = ukrainian_losses)

# Display the ANOVA table
summary(anova_result_ukraine)

##############################

#ANOVA again

###############################

# Perform ANOVA
anova_result <- aov(losses_total ~ destroyed, data = ukrainian_losses)

# Display the ANOVA table
summary(anova_result)

#The ANOVA results strongly suggest that the 'destroyed' variable has
#a significant effect on the 'losses_total' variable.


###############################

#PART 12: SOME EXAMPLE WITH DOE

###############################
#Objective: To determine the impact of various factors (e.g., terrain type, time of day, weather conditions) on the likelihood of equipment loss in a simulated military exercise.

#Factors and Levels:
  
#Terrain Type: Urban, Rural, Forest
#Time of Day: Day, Night
#Weather Conditions: Clear, Rainy, Foggy
#Response Variable: Number of equipment losses.

#Experiment Design: A fractional factorial design can be used, where every possible combination of these factors is tested.
# Load necessary library
library(FrF2)

# Assuming original levels
factors <- data.frame(
  Terrain = factor(c("Urban", "Rural", "Forest")),
  TimeOfDay = factor(c("Day", "Night")),
  Weather = factor(c("Clear", "Rainy", "Foggy"))
)

# Create a fractional factorial design
design <- FrF2(nfactors = length(factors), resolution = 3, randomize = TRUE)  # Resolution 3 is commonly used
print(design)
###############################

#PART 13: SOME EXAMPLE WITH STATISTICAL QUALITY CONTROL
## Install and load the qcc package
install.packages("qcc")
library(qcc)

# Example data: Proportions of defective items in 20 samples
defect_proportions <- c(0.02, 0.025, 0.015, 0.03, 0.02, 0.025, 0.01, 0.015, 0.02, 0.03, 
                        0.025, 0.02, 0.018, 0.015, 0.02, 0.022, 0.027, 0.02, 0.023, 0.019)
sample_sizes <- rep(100, 20)  # Assuming eaq(ch sample has 100 items

# Create a p-chart
p_chart <- qcc(defect_proportions, sizes = sample_sizes, type = "p", 
               plot = TRUE, title = "p-chart for Equipment Defects")

# Add interpretation
interpretation <- qcc.groups(p_chart, cause.group = c(5, 10, 17))
###############################

