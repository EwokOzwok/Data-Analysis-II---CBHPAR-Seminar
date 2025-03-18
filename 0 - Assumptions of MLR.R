##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Assumptions of MLR  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1: Independence of observations (study design)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2: No measurement error in X (reliable / valid measures used given target pop)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3: Model is correctly specified  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 4: Linear Relationship between each X & Y  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The 4th assumption must be tested before running the analysis

plot(data$audit_total, data$phq9_total, xlab = "AUDIT", ylab = "Depression", main= "Plot of AUDIT and Depression")
fit<-lm(phq9_total~audit_total, data)
abline(fit, col="red")

plot(data$diener_mean, data$phq9_total, xlab = "Flourishing", ylab = "Depression", main= "Plot of Flourishing and Depression")
fit<-lm(phq9_total~diener_mean, data)
abline(fit, col="red")

# NOT LINEAR?: Look into exponential or quadradic regression (i.e., poloynomial regression)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 5: Homoskedasticity of residuals  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# You must fit the model to test the 5th assumption 
fit<-lm(phq9_total~audit_total+diener_mean+Gender_Male, data)


# Generate the diagnostic plots
par(mfrow=c(2,2))
plot(fit)


# Look at the plot(fit) matrix
# Uniform distribution of residuals around Y (0) in Resid. vs. Fitted?
# Scale-Location graph's line sticking close to center?  (some ups & down's are ok)

# Violated the 5th Assumption? Look into Weighted Least Sq. Regression (WLS)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 6: Independence of Residuals - related to 1st assumption  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To test for the 6th assumption, run a Durbin-Watson test (car package)
# The Durbin-Watson statistic tests for autocorrelation in the residuals (ERROR) of a regression model
# install.packages("car") # Uncomment and install if needed
library(car)
durbinWatsonTest(fit)

# Null Hypothesis is "There is no autocorrelation in the residuals of a regression model"
# Therefore, we MUST SEE NON-SIGNIFICANT RESULTS 
# This test is sensitive with very large sample sizes

# Violated the 6th Assumption?
# Check for model misspecification
# Consider nonlinear relationships
# Try Autoregressive (AR) or Generalized Least Squares (GLS) Models
# Log Transform data
# STILL SIGNIFICANT? Use HAC (Heteroskedasticity and Autocorrelation Consistent) standard errors (e.g., Newey-West estimator)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 7: Normally Distributed Residuals  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# To satisfy the 7th Assumption, the Q-Q plot from plot(fit) should not deviate much from the diagonal
# Often there is some deviation at the tails, this is mostly ok...

# DEVIATING TOO MUCH (i.e., violated the 7th assumption)? 
# Increase Sample size
# Transform the data and re-test
# Still violating this assumption? Try WLS (NOT IDEAL... to say the least)

plot(fit)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 8: No Multicollinearity  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variance Inflation Factor (VIF) should be less than 5 for all predictors
# VIF MUST BE less than 10
vif(fit)

# Violated the 8th assumption?
# Remove Highly Correlated Predictors
# Combine Highly Correlated Predictors (assumping they're measuring the same thing...)
# Increase Sample Size


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 9: No influential Outliers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Leverage: X-AXIS (i.e., Independent Variable) Outliers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get the hat values (leverage values)
leverage_values <- hatvalues(fit)

# Display leverage values
print(leverage_values)

# Calculate the average leverage value
avg_leverage <- mean(leverage_values)

# Set a threshold for high leverage (commonly 2 * p / n)
# p is the number of predictors, and n is the number of observations
threshold <- 2 * length(coef(fit)) / nrow(data)

# Find data points with leverage greater than the threshold
high_leverage_points <- which(leverage_values > threshold)

# Display high leverage points
cat("High leverage points are at indices:", high_leverage_points, "\n")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Discrepancy: Y-AXIS (i.e., Dependent Variable) Outliers  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages('olsrr') # Uncomment and install if needed
library(olsrr)


# olsrr package: Studentized deleted residuals
# Generate Studentized residual plot

ols_plot_resid_stud(fit)

# olsrr's default threshold is for large samples 
# The more conservative values are: 
#                                   - "2" (small n) 
#                                   - "3-4" (n>100)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Influence: Captures BOTH Leverage & Discrepancy  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# DFFIT (Standardized Difference in Fit): Measures the change in fitted values when an observation is removed. 
ols_plot_dffits(fit)
# Large values (typically >1) indicate influential points.


# Cook's Distance: Combines leverage and residual to detect influential points. 
ols_plot_cooksd_bar(fit)
# Values >1 (or 4/n) suggest strong influence.



# Standardized DFBETA: Measures the change in each coefficient when an observation is removed. 
ols_plot_dfbetas(fit)
# Large values (>1) indicate influential points.


# Influential Outliers?
# Remove them and re-run analysis
