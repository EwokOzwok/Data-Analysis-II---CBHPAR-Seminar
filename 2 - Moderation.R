
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  ART of R - Organizational Headings Tool                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages("ARTofR") # Install only once
# library(ARTofR)
# ARTofR::xxx_title1('Mediation')
# ARTofR::xxx_title3('Structural Equation Modeling (SEM)')
# ARTofR::xxx_title3('MAR')
# ARTofR::xxx_title3('MNAR')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- INITIAL DATA CLEANING-----------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("1 - Data Cleaning.R")

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
hist(data$Gender_Male, main = "Gender\n(F = 0; M = 1)")    

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##-------------------------------- MODERATION-----------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# install.packages("emmeans")
library(emmeans)

# 1. nominal predictor and nominal moderator 
data$abstainer <- ifelse(data$Alc30D == 0, 1, -1)

# nested model comparisons
fit_rest<-lm(phq9_total ~ abstainer + Gender_Male, data=data)

# moderation with two nominal variables
fit_full <- lm(phq9_total ~ abstainer + Gender_Male + (abstainer*Gender_Male), data=data)


summary(fit_rest)
summary(fit_full)
anova(fit_rest, fit_full)
lm.beta::lm.beta(fit_full)


# install.packages("sjPlot")
library(sjPlot)
plot_model(fit_full, type = "pred", terms = c("abstainer", "Gender_Male"))
# plot_model(fit_full, type = "int")


# Compute simple slopes
simple_slopes <- emtrends(fit_full, 
                          specs = "Gender_Male", 
                          var = "abstainer")

# Print results
summary(simple_slopes, conf.int = TRUE)





# 2. continuous predictor and nominal moderator 
data$diener_mean_centered <- scale(data$diener_mean, scale = F, center = T)
data$diener_mean_centered <- as.numeric(data$diener_mean_centered)

fit_rest <- lm(phq9_total ~ diener_mean_centered + Gender_Male, data = data)
summary(fit_rest)

vif(fit_rest)


# moderator full - CENTERED and dummy coded



fit_full <- lm(phq9_total ~ diener_mean_centered + Gender_Male + (diener_mean_centered*Gender_Male), data=data)
vif(fit_full)


summary(fit_full)
anova(fit_rest, fit_full)
lm.beta::lm.beta(fit_full)

plot_model(fit_full, type = "int")

# Compute simple slopes
simple_slopes <- emtrends(fit_full, 
                          specs = "Gender_Male", 
                          var = "diener_mean_centered")

# Print results
summary(simple_slopes, conf.int = TRUE)


# 3 - moderation with continuous predictor and continuous moderator (both mean-centered)
data$Alc30D_centered <- scale(data$Alc30D, scale = F, center = T)
data$Alc30D_centered <- as.numeric(data$Alc30D_centered)
fit_rest <- lm(phq9_total ~ diener_mean_centered + Alc30D_centered, data=data )
vif(fit_rest)

summary(fit_rest)
  
fit_full1 <- lm(phq9_total ~ diener_mean_centered + Alc30D_centered + diener_mean_centered*Alc30D_centered, data=data )
vif(fit_full1)

summary(fit_full1)
summary.aov(fit_full1)

# model comparison
anova(fit_full1, fit_rest)
library(psych)
describe(data$Alc30D)
library(sjPlot)
plot_model(fit_full1, type = "pred", terms = c("diener_mean_centered", "Alc30D_centered"))






