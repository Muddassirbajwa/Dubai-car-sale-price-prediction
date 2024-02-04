# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Display the attributes/column names
cat("Attributes:\n")
print(names(dataset))

# Display the first five rows of the dataset
cat("\nFirst five rows of the dataset:\n")
print(head(dataset))



# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Check for null values in each attribute
null_values <- colSums(is.na(dataset))

# Display the null values for each attribute
cat("Null Values for Each Attribute:\n")
print(null_values)



# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Create box plots for selected attributes
par(mfrow=c(1, 3))  # Setting up a 1x3 grid for box plots

# Boxplot for "kilometers"
boxplot(dataset$kilometers, main="Kilometers Boxplot", ylab="Kilometers", col="lightblue", border="black")

# Boxplot for "no_of_cylinders"
boxplot(dataset$no_of_cylinders, main="Number of Cylinders Boxplot", ylab="Number of Cylinders", col="lightgreen", border="black")

# Boxplot for "year"
boxplot(dataset$year, main="Year Boxplot", ylab="Year", col="lightcoral", border="black")

# Reset the layout to default
par(mfrow=c(1, 1))



# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Create a bar plot for the "year" attribute
barplot(table(dataset$year), main="Distribution of Years", col="lightblue", border="black", ylim=c(0, max(table(dataset$year))*1.2))



# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Create a scatter plot for the "kilometers" attribute
plot(dataset$kilometers, main="Scatterplot: Kilometers", xlab="Index", ylab="Kilometers", col="blue", pch=16)






# Install and load the e1071 package if not already installed
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}
library(e1071)

# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Check skewness for the "kilometers" attribute
kilometers_skewness <- skewness(dataset$kilometers)
cat("Skewness for Kilometers:", kilometers_skewness, "\n")

# Check kurtosis for the "kilometers" attribute
kilometers_kurtosis <- kurtosis(dataset$kilometers)
cat("Kurtosis for Kilometers:", kilometers_kurtosis, "\n")


# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Check unique values of the "body_type" attribute
unique_body_types <- unique(dataset$body_type)
cat("Unique Body Types:", unique_body_types, "\n")

# Apply label encoding to the "body_type" attribute
dataset$body_type_encoded <- as.numeric(factor(dataset$body_type))

# Display the first few rows of the updated dataset
head(dataset)

# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Display the count of unique values in the "company" attribute
company_counts <- table(dataset$company)
cat("Count of Company Attribute Values:\n")
print(company_counts)



# Install and load the randomForest package if not already installed
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}
library(randomForest)

# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Set the seed for reproducibility
set.seed(123)

# Create an index for splitting the dataset
index <- createDataPartition(dataset$body_type_encoded, p = 0.8, list = FALSE)

# Split the dataset into training (80%) and testing (20%) sets
training_data <- dataset[index, ]
testing_data <- dataset[-index, ]

# Specify the predictor variables (features)
predictor_variables <- c("price_in_aed", "kilometers", "no_of_cylinders", "horsepower", "year")

# Train a Random Forest model
rf_model <- randomForest(body_type_encoded ~ ., data = training_data[, predictor_variables])

# Make predictions on the testing data
predictions <- predict(rf_model, newdata = testing_data[, predictor_variables])

# Evaluate the model (you can replace "accuracy" with other metrics based on your task)
accuracy <- sum(predictions == testing_data$body_type_encoded) / length(predictions)
cat("Accuracy on Testing Data:", accuracy, "\n")



# Install and load the required packages if not already installed
if (!requireNamespace("Metrics", quietly = TRUE)) {
  install.packages("Metrics")
}
library(Metrics)

# Calculate Mean Squared Error (MSE)
mse <- mse(predictions, testing_data$body_type_encoded)
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate Mean Absolute Error (MAE)
mae <- mae(predictions, testing_data$body_type_encoded)
cat("Mean Absolute Error (MAE):", mae, "\n")

# Calculate Root Mean Squared Error (RMSE)
rmse <- rmse(predictions, testing_data$body_type_encoded)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")






# Set the working directory to the folder containing your dataset
setwd("D:/Education/As a Student/NCI Data Analytics/Semester 1/Data Mining and Machine Learning I/Project/Dubizzle used car sales data/Project/")

# Read the CSV file into a data frame
dataset <- read.csv("data.csv")

# Set the seed for reproducibility
set.seed(123)

# Create an index for splitting the dataset
index <- createDataPartition(dataset$price_in_aed, p = 0.8, list = FALSE)

# Split the dataset into training (80%) and testing (20%) sets
training_data <- dataset[index, ]
testing_data <- dataset[-index, ]

# Perform linear regression with "price_in_aed" as the dependent variable and "year" as the independent variable
linear_model <- lm(price_in_aed ~ year, data = training_data)

# Make predictions on the testing data
predictions <- predict(linear_model, newdata = testing_data)

# Evaluate the model
mse <- mean((predictions - testing_data$price_in_aed)^2)
mae <- mean(abs(predictions - testing_data$price_in_aed))
rmse <- sqrt(mean((predictions - testing_data$price_in_aed)^2))

# Display evaluation metrics
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Display the coefficients of the linear regression model
cat("\nLinear Regression Coefficients:\n")
print(coef(linear_model))

