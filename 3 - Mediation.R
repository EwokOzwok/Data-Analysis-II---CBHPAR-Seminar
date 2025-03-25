
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- INITIAL DATA CLEANING-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("1 - Data Cleaning.R")
par(mfrow=c(1,1))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------- SELECT VARIABLES OF INTEREST--------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


colnames(df)

data<-df[,c(4,5,75,99,283:291,295:304,347:354)]

head(data)
colnames(data)[4] <- "Alc30D"
colnames(data)
# Keep only rows where gender is either male or female

data$remove<-ifelse(data$Gender_Female == 0 & data$Gender_Male==0,1,0)

table(data$remove)

data <- data[data$remove == 0, ]

data<-dplyr::select(data,-remove)

str(data)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Compute Variables  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~

# AUDIT

# Select the AUDIT columns (AUDIT_1 to AUDIT_10)
audit_columns <- paste0("AUDIT_", 1:10)

# Create a new variable audit_total that sums the AUDIT scores for rows with no NAs
data$audit_total <- rowSums(data[, audit_columns], na.rm = FALSE)

# For rows with NAs in any of the AUDIT columns, set audit_total to NA
data$audit_total[!complete.cases(data[, audit_columns])] <- NA

describe(data$audit_total)
hist(data$audit_total)


# Depression

# Select the PHQ9 columns (PHQ9_1 to PHQ9_9)
phq9_columns <- paste0("PHQ9_", 1:9)

# Create a new variable phq9_total that sums the PHQ9 scores for rows with no NAs
data$phq9_total <- rowSums(data[, phq9_columns], na.rm = FALSE)

# For rows with NAs in any of the PHQ9 columns, set phq9_total to NA
data$phq9_total[!complete.cases(data[, phq9_columns])] <- NA

describe(data$phq9_total)
hist(data$phq9_total)


# Flourishing

# Select the Diener columns (Diener1 to Diener8)
diener_columns <- paste0("Diener", 1:8)

# Create a new variable diener_mean that calculates the mean of the Diener scores for rows with no NAs
data$diener_mean <- rowMeans(data[, diener_columns], na.rm = FALSE)

# For rows with NAs in any of the Diener columns, set diener_mean to NA
data$diener_mean[!complete.cases(data[, diener_columns])] <- NA


describe(data$diener_mean)
hist(data$diener_mean)


# IMPORTANT: MAKE A BACKUP OF THE DATA!
data_backup <- data


# Clean up data to remove individual items
data<-dplyr::select(data_backup, Gender_Male, GPA, Alc30D, audit_total, phq9_total, diener_mean)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- MISSING DATA ANALYSIS-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Visualize Missingness  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

finalfit::missing_plot(data)
finalfit::missing_pattern(data)

##~~~~~~~~~~~~~~~~~~~
##  ~ MCAR Test  ----
##~~~~~~~~~~~~~~~~~~~

mcar_test(data)

##~~~~~~~~~~~~~~~~~~~~~~
##  ~ MAR Analysis  ----
##~~~~~~~~~~~~~~~~~~~~~~

# START with most missingness (i.e., audit_total)
data$missing_audit_total <- ifelse(is.na(data$audit_total)==T, 1, 0)
data$missing_audit_total <- as.integer(data$missing_audit_total)

missing_audit_fit <- glm(missing_audit_total ~ Gender_Male + GPA + Alc30D + phq9_total + diener_mean, family = binomial, data = data)
summary(missing_audit_fit) # MAR
OR <- exp(-0.49818)
OR
1-OR
# Because missingness in AUDIT_total scores is predicted by past month alcohol use 
# (i.e., For every 1 additional day of past alcohol use the odds of missing AUDIT_total decrease by approximately 39.22%)
# We can impute using Multiple Imputation

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Impute ONLY AUDIT_total  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(mice)

# Restore Original data from backup
data<-dplyr::select(data_backup, Gender_Male, GPA, Alc30D, audit_total, phq9_total, diener_mean)

# Perform imputation on ALL variables with pmm
imputed_data <- mice(data, method = "pmm", m = 5, seed = 123)

# Extract the completed dataset
data <- complete(imputed_data)

# Restore original missingness for all variables except audit_total
non_impute_cols <- c("Gender_Male", "GPA", "Alc30D", "phq9_total", "diener_mean")

for(col in non_impute_cols){
  data[,col] <- data_backup[,col]
}

# Visualize Missingness again

finalfit::missing_plot(data)
finalfit::missing_pattern(data)

# Conduct MCAR test again

mcar_test(data) 

# Non-Significant MCAR Test -> Data is missing completely at random
# List-wise deletion or Multiple Imputation appropriate
data <- na.omit(data)

write.csv(data, "data/data_clean.csv", row.names = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ View Descriptives  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~
describe(data)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Examine Histograms  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfrow = c(2,3))
hist(data$audit_total, main = "AUDIT Scores")
hist(data$diener_mean, main = "Flourishing Scores")
hist(data$phq9_total, main = "PHQ-9 Scores")
hist(data$GPA, main = "GPA")
hist(data$Alc30D, main = "Alc30D")
hist(data$Gender_Male, main = "Gender\n(M = 1; F = 0)")




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- MEDIATION-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Install packages once (if not already installed)

# install.packages("car")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("MASS")
# install.packages("ltm")
# install.packages("lavaan")
# install.packages("semPlot")
# install.packages("semhelpinghands")
# install.packages("moments")

# Load libraries
library(car)
library(dplyr)
library(psych)
library(MASS)
library(ltm)
library(lavaan)
library(semPlot)
library(semhelpinghands)
library(moments)

# Mean-center all variables

data$diener_mean_mc <- scale(data$diener_mean, scale = F, center = T)
data$Alc30D_mc <- scale(data$Alc30D, scale = F, center = T)
data$audit_total_mc <- scale(data$audit_total, scale = F, center = T)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Structural Equation Modeling (SEM)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



model <- '
    ## Structural Model ##
    phq9_total ~ c_direct*Alc30D_mc
    phq9_total ~ b*audit_total_mc 
    audit_total_mc ~ a*Alc30D_mc

    ## Mediation Effects ##
    
    # Indirect Effect (a*b)
    indirect := a*b

    # Direct Effect
    direct := c_direct
    
    # Total Effect
    total := c_direct + (a*b)
'

fit <- sem(model,
           data = data,
           se = "bootstrap",
           bootstrap = 5000,
           parallel = "snow",
           ncpus = 23,
           iseed = 8888)

# Some errors will pop up, this is ok as long as the number of failed iterations is not very high (i.e., not > 1000)
# If errors exceed 500-1000, it suggests your model is not converging well and may need respecification or to examinet he data for errors in cleaning
# The models that failed to converge will not be used in computing the CIs

summary(fit, fit.measures=T, standardized = T) # We cannot interpret this!




# Examine Distribution of Bootstrap Results to determine which CI's to interpret


bootstrap_results <- fit@boot
bootstrap_results$coef
coef_vector <- as.vector(bootstrap_results$coef)
length(coef_vector)
coef_vector <- na.omit(coef_vector)

par(mfrow=c(1,1))
hist(coef_vector, main = "Bootstrap Estimates Distribution", xlab = "Estimate", col = "lightblue")

# QQ plot
qqnorm(coef_vector)
qqline(coef_vector, col = "red")

# Check for skewness and kurtosis of the bootstrap estimates
skewness(coef_vector)
kurtosis(coef_vector)

# Kolmogorov-Smirnov (KS) test for normality
# Null hypothesis: "The data follow a normal distribution"
# Alt. hypothesis: "The data do not follow a normal distribution"
ks_test_result <- ks.test(coef_vector, "pnorm", mean(coef_vector), sd(coef_vector))
print(ks_test_result)



# Generate the standardized solution with delta-method confidence intervals (Assumes Normality of bootstrapped estimates) 

# Delta-Method Confidence Intervals
# Based on Taylor series approximations, assuming the parameter estimates follow a normal distribution.
# 
# Uses the asymptotic variance-covariance matrix of the parameter estimates.
# 
# Often faster to compute but relies on assumptions about the underlying distribution.
# 
# Can be inaccurate if the sampling distribution is skewed or non-normal.

standardizedSolution(fit) # This is inaccurate because the sampling distribution of our bootstrap estimates is not normal

# Generate bootstrap percentile confidence intervals for the standardized solution (No normality assumption)

# Bootstrap Percentile Confidence Intervals
# A non-parametric approach that does not assume normality.
# 
# Resamples the data multiple times (e.g., 5,000 bootstrap samples) and computes the parameter of interest in each resampled dataset.
# 
# The percentile method takes the lower and upper percentiles (e.g., 2.5th and 97.5th) from the bootstrap distribution to form the confidence interval.
# 
# More robust, especially when the parameter estimates are non-normally distributed, but computationally intensive.

ci_boot <- standardizedSolution_boot_ci(fit)
print(ci_boot, output = "text")
summary(fit, fit.measures=T, standardized = T, rsquare=T) # We cannot interpret this!


semPaths(fit, "std", fade=F, residuals = F, layout = "tree", rotation =2, 
         edge.label.cex = 1.2, edge.label.position = .6, nCharNodes = 5)
