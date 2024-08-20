# Load required libraries
library(tidyverse)      # Data manipulation and visualization
library(caret)          # Data partitioning and modeling
library(ggcorrplot)     # Correlation matrix visualization
library(rpart)          # Decision tree model
library(rpart.plot)     # Plotting decision trees
library(fastDummies)    # Create dummy variables

# Load the dataset
autoClaims <- read_csv("AutoClaims.csv")

# 1. Exploratory Data Analysis (EDA)

# 1.1 Descriptive Statistics
# Summary statistics for all variables
summary(autoClaims)

# 1.2 Handle Missing Values
# Check if there are any missing values
any_na <- any(is.na(autoClaims))
if(any_na) {
  # If there are missing values, remove them
  clean_autoClaims <- na.omit(autoClaims)
  print("Data contained missing values and they were removed.")
} else {
  # If no missing values, use the original dataset
  clean_autoClaims <- autoClaims
  print("Data is clean with no missing values.")
}

# 1.3 Distribution Plots
# Histogram for AGE
ggplot(autoClaims, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Histogram for PAID (Claim Amount)
ggplot(autoClaims, aes(x = PAID)) +
  geom_histogram(binwidth = 500, fill = "green", color = "black") +
  labs(title = "Distribution of Claim Amounts", x = "Claim Amount", y = "Frequency")

# 1.4 Correlation Analysis
# Correlation matrix for numerical variables
cor_matrix <- autoClaims %>% 
  dplyr::select(AGE, PAID) %>% 
  cor()

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
ggcorrplot(cor_matrix, lab = TRUE)

# 2. Feature Engineering

# 2.1 Subset Data by Age Groups
# Subsetting data into specific age ranges
fiftiesToSixties <- autoClaims %>%
  filter(AGE >= 50 & AGE < 70)

seventiesToEighties <- autoClaims %>%
  filter(AGE >= 70 & AGE < 90)

nineties <- autoClaims %>%
  filter(AGE >= 90)

# Graph each age group to claims
ggplot(fiftiesToSixties, aes(AGE, PAID)) +
  geom_point()

ggplot(seventiesToEighties, aes(AGE, PAID)) + 
  geom_point()

ggplot(nineties, aes(AGE, PAID)) + 
  geom_point()

# 2.2 Create Age Groups
# Create age group categories
autoClaims <- clean_autoClaims %>%
  mutate(age_group = case_when(
    AGE >= 50 & AGE < 60 ~ "Fifties",
    AGE >= 60 & AGE < 70 ~ "Sixties",
    AGE >= 70 & AGE < 80 ~ "Seventies",
    AGE >= 80 & AGE < 90 ~ "Eighties",
    AGE >= 90 ~ "Nineties"
  ))

# Ensure age groups are factors with correct order
myLevels <- c("Fifties", "Sixties", "Seventies", "Eighties", "Nineties")
autoClaims$age_group <- ordered(autoClaims$age_group, levels = myLevels)

# 2.3 Boxplots by Age Group
# Visualize claim amounts by age group
ggplot(autoClaims, aes(age_group, PAID)) +
  geom_boxplot() +
  labs(title = "Claims Paid by Age Group", x = "Age Groups", y = "Claims Paid")

# 2.4 Summary Statistics by Age Group
# Summary statistics for each age group
summary(autoClaims[autoClaims$age_group == "Fifties", "PAID"])
summary(autoClaims[autoClaims$age_group == "Sixties", "PAID"])
summary(autoClaims[autoClaims$age_group == "Seventies", "PAID"])
summary(autoClaims[autoClaims$age_group == "Eighties", "PAID"])
summary(autoClaims[autoClaims$age_group == "Nineties", "PAID"])

# 2.5 Dummification of Categorical Variables
# Convert categorical variables into dummy variables and remove original columns
autoClaims_dummies <- autoClaims %>%
  mutate(across(c(CLASS, STATE, GENDER, age_group), as.factor)) %>%
  fastDummies::dummy_cols(select_columns = c("CLASS", "STATE", "GENDER", "age_group"), 
                          remove_first_dummy = TRUE) %>%
  # Remove the original categorical columns
  dplyr::select(-Index,-STATE, -CLASS, -GENDER, -age_group)

# View the modified dataset with dummy variables
head(autoClaims_dummies)

# 2.6 Interaction and Polynomial Features
# Generate interaction terms and polynomial features
autoClaims_dummies <- autoClaims_dummies %>%
  mutate(
    AGE_CLASS = AGE * as.numeric(CLASS_C11),  # Example interaction with one CLASS variable
    AGE_GENDER = AGE * as.numeric(GENDER_M),
    AGE_STATE = AGE * as.numeric(`STATE_STATE 02`),  # Example with one STATE variable
    AGE_AGE = poly(AGE, 2, raw = TRUE)
  )

# 3. Advanced Modeling

# 3.1 Simple Linear Regression
# Linear regression model with AGE as the predictor
claimsModel <- lm(PAID ~ AGE, autoClaims)
summary(claimsModel)

# 3.2 Log Transformation of PAID
# Log transformation of PAID
autoClaims <- autoClaims %>%
  mutate(logPaid = log(PAID))

# Linear regression model with log-transformed PAID
logClaimsModel <- lm(logPaid ~ AGE, autoClaims)
summary(logClaimsModel)

# Boxplot of logClaims by age group
ggplot(autoClaims, aes(x = age_group, y = logPaid, color = age_group)) +
  geom_boxplot() +
  labs(title = "Claims Paid by Age Group (log)", x = "Age Group", y = "Claims Paid (log)")

# Scatter plot with regression line
ggplot(autoClaims, aes(x = AGE, y = logPaid, color = age_group)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot with Regression Line", x = "Age", y = "Claims Paid (log)")

# 3.3 Correlation Analysis
# Correlation between AGE and PAID
cor(autoClaims$AGE, autoClaims$PAID)
# Correlation between AGE and log(PAID)
cor(autoClaims$AGE, autoClaims$logPaid)

# 3.4 Multivariate Regression
# Multivariate regression model including age group and other variables
multivariateModel <- lm(PAID ~ AGE + GENDER + CLASS + STATE + age_group, data = autoClaims)
summary(multivariateModel)

# 3.5 Linear Model with Interaction Term
# Interaction model between AGE and CLASS
interactionModel <- lm(PAID ~ AGE * CLASS, data = autoClaims)
summary(interactionModel)

# 3.6 Non-Linear Models
# Polynomial regression model (for non-linear effects)
polyModel <- lm(PAID ~ poly(AGE, 2) + GENDER + CLASS + STATE + age_group, data = autoClaims)
summary(polyModel)

# 3.7 Decision Tree Model
# Decision tree model with adjusted parameters to find the best split
treeModel <- rpart(PAID ~ AGE + GENDER + CLASS + STATE + age_group, 
                   data = autoClaims, 
                   control = rpart.control(minsplit = 10, cp = 0.006))
rpart.plot(treeModel)

# 4. Model Evaluation

# 4.1 Train-Test Split
# Splitting the data into training and test sets for model evaluation
set.seed(123)
trainIndex <- createDataPartition(autoClaims_dummies$PAID, p = 0.8, list = FALSE)
trainData <- autoClaims_dummies[trainIndex, ]
testData <- autoClaims_dummies[-trainIndex, ]

# Train a multivariate model on the training data
trainModel <- lm(PAID ~ ., data = trainData)
summary(trainModel)

# Predict on the test data
predictions <- predict(trainModel, newdata = testData)

# Calculate RMSE on the test set
RMSE <- sqrt(mean((testData$PAID - predictions)^2))
print(paste("Root Mean Squared Error (Test Set):", RMSE))

# 5. Clustering Analysis

# 5.1 Perform K-Means Clustering
# K-Means clustering to identify groups of customers with similar claim amounts and demographics

set.seed(123)

# Perform K-means clustering using the cleaned dataset without the PAID column
numeric_columns <- autoClaims_dummies %>%
  select_if(is.numeric)%>%
  dplyr::select(-PAID) 

# Apply K-means clustering on the numeric columns
kmeans_result <- kmeans(scale(numeric_columns), centers = 3)

# Add the cluster assignments to the cleaned dataset
autoClaims_cleaned <- autoClaims_dummies
autoClaims_cleaned$cluster <- as.factor(kmeans_result$cluster)

# 5.2 Visualize Clustering Results
# Visualize the clustering results to see how different age groups fall into clusters

ggplot(autoClaims_cleaned, aes(x = AGE, y = PAID, color = cluster)) +
  geom_point() +
  labs(title = "Clustering Analysis of Claim Amounts", x = "Age", y = "Claim Amount")

# 7. Visualization

# 7.1 Heatmaps
# Visualize the correlation matrix with a heatmap

cor_matrix_cleaned <- cor(autoClaims_cleaned %>% 
                            dplyr::select(-PAID, -cluster))
ggcorrplot(cor_matrix_cleaned, lab = FALSE)

# 7.2 Bar Charts
# Bar chart for average PAID by STATE

state_avg <- autoClaims %>%
  group_by(STATE) %>%
  summarize(mean_paid = mean(PAID))

ggplot(state_avg, aes(x = STATE, y = mean_paid)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Claim Amounts by State", x = "State", y = "Average Claim Amount")

# 7.3 Interaction Plots
# Interaction plot between AGE, CLASS, and GENDER

ggplot(autoClaims_cleaned, aes(x = AGE, y = PAID, color = CLASS_C11)) +
  geom_point() +
  facet_wrap(~GENDER_M) +
  labs(title = "Interaction Effects between Age, Class, and Gender", x = "Age", y = "Claim Amount")

# 7.4 Age Group Comparison
# Bar chart for average PAID by age group

age_group_avg <- autoClaims %>%
  group_by(age_group) %>%
  summarize(mean_paid = mean(PAID))

ggplot(age_group_avg, aes(x = age_group, y = mean_paid)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Average Claim Amounts by Age Group", x = "Age Group", y = "Average Claim Amount")
