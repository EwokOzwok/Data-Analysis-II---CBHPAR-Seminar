
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  ART of R - Organizational Headings Tool                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages("ARTofR") # Install only once
# library(ARTofR)
# ARTofR::xxx_title1('Missing Data Analysis')
# ARTofR::xxx_title3('MCAR')
# ARTofR::xxx_title3('MAR')
# ARTofR::xxx_title3('MNAR')

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Install Packages - Only done once                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# install.packages("haven") # Integrates SPSS and R
# install.packages("dplyr") # Great for data manipulation
# install.packages("psych") # Psych data package
# install.packages("car") # Companion to Applied Regression
# install.packages("caTools") # sample splits and other great data munging functions
# install.packages("finalfit") # for missing data
# install.packages("naniar") # More Missing data functions
# install.packages("rempsyc") # generates APA 7 tables very quickly in docx format
# install.packages("lubridate") # for handling dates

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               Load Libraries                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(haven)
library(dplyr)
library(psych)
library(lubridate)
library(car)
library(caTools)
library(finalfit)
library(rempsyc)
library(naniar)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  Load Data                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df <- read_sav("data/Fall24-Baseline_R.sav")

# df<-read_sav("data/Fall24-Baseline_R(FULL).sav")

# remove additional meta data from SPSS .sav file
df <- as.data.frame(as.matrix(df))
class(df)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Inspect Data Structure                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Print Column Names
colnames(df)


# Remove Header columns
df <- df[,-c(1:4, 6:7, 9:17, 399:408)]
colnames(df)

# Examine data-type for all variables
str(df)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             Data-type cleaning                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All variables are 'character' type,
# Therefore, we need to change all non-char variables to numeric (by column number)
colnames(df)
numeric_cols<-c(1, 3:5, 7:14, 16:31, 33:34, 36:44, 46:79, 81:383)

# Our first 'for-loop'

# Include Step by step of what a for loop is doing
for(col in numeric_cols){
  df[,col]<-as.numeric(df[,col])
}


# cleanup environment
rm(numeric_cols, col)


str(df[,1:10])

# Change RecordedDate to Date format (Column 2)
df$RecordedDate<-as.Date(df$RecordedDate)
df[,2]<-as.Date(df[,2])

str(df[,2])
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Variable Name cleaning                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Write column names to csv & manually clean in Excel

# write.csv(colnames(df), "col_names2.csv", row.names = F)


clean_columns<-read.csv("col_names.csv")

colnames(df)<-clean_columns$clean

colnames(df)

# cleanup environment
rm(clean_columns)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Cleaning Individual Variables                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~
##  ~ Age  ----
##~~~~~~~~~~~~~


df$age <- year(df$RecordedDate)- df$BirthYear


### Step 4: Examine the age column ###
describe(df$age)
hist(df$age)

### Step 5: Remove unneeded columns using dplyr
df<-dplyr::select(df, -BirthYear, -BirthMonth)

# Reorganize to make age the 3rd variable
df <- select(df, 1:2, age, everything())

##~~~~~~~~~~~~~~~~~~
##  ~ Hispanic  ----
##~~~~~~~~~~~~~~~~~~

# 95 corresponds to refusal to respond to the item, so we recode these as NA's
unique(df$Hispanic)

df$Hispanic <- ifelse(df$Hispanic == 95, NA, df$Hispanic)

table(df$Hispanic)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Race and Ethnicity  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Objectives: Replace NA's in Race/Ethnicity with 0's
#             Create new variable to denote missing Race/Ethnicity

# Steps: 1.) Use a for loop to replace all NA's with 0's
#        2.) Create new variable that adds up all the ethnicity columns
#        3.) If the sum of all ethnicity columns is 0, make a new missing_enthnicity column that is = 1
#        4.) Repeat this for Race


### Step 1: Use a for loop to replace all NA's with 0's ###

# Identify Ethnicity column numbers (7:13)
colnames(df[,7:13])

 
### Step 2: Use a for loop to replace NA's with 0 ###

for(i in 7:13){
  df[,i] <- ifelse(is.na(df[,i])==T, 0, df[,i])
}

### Step 3: Create missing_ethnicity variable ###

# Initialize a missing_ethnicity variable
df$missing_ethnicity = NA
for(i in 1:nrow(df)){
  df[i, "missing_ethnicity"]<-ifelse(sum(df[i, 7:13])==0, 1, 0)
}

table(df$missing_ethnicity)


# Reorganize to make the missing_ethnicity the 15rd variable
df <- select(df, 1:14, missing_ethnicity, everything())


### Step 4: Repeat for Race ###
colnames(df[,16:32])

for(i in 16:31){
  df[,i]<-ifelse(is.na(df[,i])==T, 0, df[,i])
}

df$missing_race = NA
for(i in 1:nrow(df)){
  df[i, "missing_race"]<-ifelse(sum(df[i, 16:31])==0, 1, 0)
}

table(df$missing_race)

df <- select(df, 1:32, missing_race, everything())

colnames(df)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Gender - Dummy coding  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Objectives: Turn the Gender column into dummy coded (0,1) variables for each gender

unique(df$Gender)
table(df$Gender)

# 1 - Male
# 2 - Female
# 5 - Transgender
# 6 - Nonbinary
# 7 - Other


# Initialize variables
df$Gender_Male <- NA
df$Gender_Female <- NA
df$Gender_Transgender <- NA
df$Gender_Nonbinary <- NA
df$Gender_Other <- NA



# Use ifelse functions to assign 0,1 values to each

df$Gender_Male <- ifelse(df$Gender==1, 1, 0)
df$Gender_Female <- ifelse(df$Gender==2, 1, 0)
df$Gender_Transgender <- ifelse(df$Gender==5, 1, 0)
df$Gender_Nonbinary <- ifelse(df$Gender==6, 1, 0)
df$Gender_Other <- ifelse(df$Gender==7, 1, 0)

# Add NA's where applicable using a for loop
gender_var_list<-c("Gender_Male", "Gender_Female", "Gender_Transgender", "Gender_Nonbinary", "Gender_Other")

for(gend in gender_var_list){
  df[, gend] <- ifelse(is.na(df$Gender)==T, NA, df[, gend])
}

rm(gender_var_list, i, gend)
colnames(df)
df<-dplyr::select(df, -Gender)

df <- select(df, 1:3, Gender_Male, Gender_Female, Gender_Transgender, Gender_Nonbinary, Gender_Other, everything())



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Transgender - Dummy coding -  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Objectives: Dummy code the response categories of Transgender

# 1 - Transgender_M2F
# 2 - Transgender_F2M
# 3 - Transgender_NonConforming
# 98 - NoneofThese (i.e., not transgender -> NA)


# Initialize the dummy coded variables
df$Transgender_M2F <- NA
df$Transgender_F2M <- NA
df$Transgender_NonConforming <- NA


# Assign values (0,1) using ifelse()
df$Transgender_M2F <- ifelse(df$Transgender == 1, 1, 0)
df$Transgender_F2M <- ifelse(df$Transgender == 2, 1, 0)
df$Transgender_NonConforming <- ifelse(df$Transgender == 3, 1, 0)

# Assign NAs where applicable
tg_var_list<-c("Transgender_M2F", "Transgender_F2M", "Transgender_NonConforming")

for(tg_col in tg_var_list){
  df[,tg_col]<-ifelse(df$Transgender == 98 | is.na(df$Transgender)==T, NA, df$Transgender)
}

rm(tg_var_list, tg_col)

# Clean up and Reorganize variable list
df<-dplyr::select(df, -Transgender)
colnames(df)
df <- select(df, 1:9, Transgender_M2F, Transgender_F2M, Transgender_NonConforming, everything())



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Sexual Orientation  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Objective: Ensure dummy coded sexual orientation variables are free of NA's except where appropriate

# Identify sexual orientation columns by number
sex_or_cols<-c(44:51)

for(col in sex_or_cols){
  df[,col]<- ifelse(is.na(df[,col]) == T, 0, 1)
}

# create missing_sexual_orientation variable
df$missing_sexual_orientation <- NA

for(i in 1:nrow(df)){
  df[i,"missing_sexual_orientation"]<-ifelse(sum(df[i,44:51])==0, 1, 0)
}

table(df$missing_sexual_orientation)


rm(sex_or_cols, col, i)

# reorganize columns
colnames(df)
df <- select(df, 1:52, missing_sexual_orientation, everything())



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------- CREATE DEMOGRAPHICS TABLE (APA 7 STYLE)---------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Identify Demographic Characteristic Columns by number
colnames(df)

# Age - 3
# Gender - 4:8
# Transgender - 10:12
# Ethnicity - 13:20
# Race - 23:38
# Sexual Orientation - 44:51

# Create a new dataframe with only dummy coded demographics (i.e., not age)
demo_df<-df[,c(4:8, 10:12, 13:20, 23:38, 44:51)]
colnames(demo_df)


# Initialize an empty list to store results
results_list <- list()

# Loop through each column in the dataset
for (col_name in colnames(demo_df)) {
  # Calculate count (N) of non-NA values in the column
  count <- sum(demo_df[[col_name]], na.rm = TRUE)
  
  # Calculate the percentage by dividing count by the total number of rows in the dataset
  percent <- (count / nrow(demo_df)) * 100
  
  # Store both count and percentage as a named vector in the results list
  results_list[[col_name]] <- c(N = count, Percent = percent)
}

# Combine the individual results from the list into a single data frame
combined_results <- do.call(rbind, results_list)

# Convert the combined results into a data frame
demo_table <- as.data.frame(combined_results)

# Format the 'Percent' column to show one decimal place followed by a percent sign
demo_table$Percent <- sprintf("%.1f%%", demo_table$Percent)

# Add a new column with the variable names as row labels
demo_table <- cbind(Variable = rownames(demo_table), demo_table)

# Reset row names to default numbers
rownames(demo_table) <- NULL

# Print the final results data frame
print(demo_table)


# Remove rows with N = 0
demo_table<-demo_table[demo_table$N>0,]

# Convert to nice_table using rempsyc
nice_demo_table<-nice_table(demo_table, title = "Demographic Characteristics")

# Print to Docx for further editing

print(nice_demo_table, preview="docx")


# Add mean and sd of Age manually in word
describe(df$age)
mean(na.omit(df$age))
sd(na.omit(df$age))


# Cleanup Environment
rm(combined_results, demo_df, demo_table, results_list, col_name, count, percent, nice_demo_table)


