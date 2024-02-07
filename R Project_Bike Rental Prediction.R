library(readr)
library(dplyr)
library(readxl)

#### 1. Exploratory data analysis ####

## load dataset
df <- read_excel("1657875746_day.xlsx")


## Perform data type conversion of the attributes 
df$dteday <- as.Date(df$dteday)  # Convert 'dteday' to Date
df$season <- as.factor(df$season)  # Convert 'season' to factor

View(df)


## summary statistics
summary(df)  


## Missing value analysis
missing_values <- colSums(is.na(df))
print(missing_values)



#### 2. Attributes distributions and trends ####

## Monthly distribution of total number of bikes rented
monthly_rental <- df %>% group_by(mnth) %>% summarize(total_rentals = sum(cnt))

barplot(monthly_rental$total_rentals, names.arg = monthly_rental$mnth,
        xlab = "Month", ylab = "Total Rentals",
        main = "Monthly Distribution of Rentals")




## Yearly distribution of total number of bikes rented
yearly_rental <- df %>% group_by(yr) %>% summarize(total_rentals = sum(cnt))

# Create a bar chart
barplot(yearly_rental$total_rentals, names.arg = c("0" = "2011", "1" = "2012"),
        xlab = "Year", ylab = "Total Rentals",
        main = "Yearly Distribution of Rentals")



## Plot boxplot for outliers analysis
boxplot(df[, c("temp", "atemp", "hum", "windspeed")],
        horizontal = TRUE, main = "Boxplot for Temperature, Humidity, and Windspeed")




#### 3. Split the dataset into train and test dataset ####

## Load necessary libraries
library(caret)
library(caTools) # for splitting dataset into train (70%) and test (30%)
library(ggplot2) # For Visualization
library(caretForecast)


# Define your features and target variable
X <- df[, c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit', 'temp', 'atemp', 'hum', 'windspeed')]
y <- df$cnt

# Set the seed for reproducibility
set.seed(123)  # Seed initializes the randomness and every time you will get same data set


# Split the dataset into training and testing sets (e.g., 70% train, 30% test)
splitIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[splitIndex, ]
X_test <- X[-splitIndex, ]
y_train <- y[splitIndex]
y_test <- y[-splitIndex]

summary(X_train)
summary(y_train)
summary(X_test)
summary(y_test)


# Print the dimensions of the training and testing sets
cat("Training set dimensions: ", dim(X_train), "\n")
cat("Testing set dimensions: ", dim(X_test), "\n")





#### 4. Create a model using the random forest algorithm ####
library(randomForest)

## Create a Random Forest model
rf_model <- randomForest(y_train ~ ., data = cbind(X_train, y_train), ntree = 100)
varImpPlot(rf_model)




#### 5. Predict the performance of the model on the test dataset ####
## Make predictions on the test set
y_pred <- predict(rf_model, newdata = X_test)

# Evaluate the model's performance (you can use various metrics)
library(Metrics)

mse <- mse(y_test, y_pred)
r2 <- R2(y_test, y_pred)

# Metric display
cat("Mean Squared Error:", mse, "\n")
cat("R-squared (R2):", r2, "\n")


# Scatter Plot of Predicted vs. Actual Values:
ggplot(data.frame(Actual = y_test, Predicted = y_pred), aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Add a 45-degree line for reference
  labs(x = "Actual", y = "Predicted") +
  ggtitle("Scatter Plot of Predicted vs. Actual Values")

