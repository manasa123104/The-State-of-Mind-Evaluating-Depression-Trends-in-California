# Load necessary libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(caret)
library(glmnet)
library(car)
library(gridExtra)

# Setting the working directory
setwd("C:/Users/NARESH KUMAR/OneDrive - George Mason University - O365 Production/GMU Spring 2024/1st Semester/Visualization for Analytics_STAT 515-DL1_Tokunbo Fadahunsi_Online/Project")

# Load the dataset
depression_data <- read.csv('adult-depression-lghc-indicator-24.csv', header = TRUE)

# Data preprocessing: remove NA values and convert necessary columns
depression_data <- na.omit(depression_data)
depression_data$Year <- as.numeric(as.character(depression_data$Year))
depression_data$Strata <- as.factor(depression_data$Strata)
depression_data$Strata.Name <- as.factor(depression_data$Strata.Name)
depression_data$Frequency <- as.numeric(depression_data$Frequency)
depression_data$Weighted.Frequency <- as.numeric(depression_data$Weighted.Frequency)
depression_data$Percent <- as.numeric(depression_data$Percent)
depression_data$Lower.95..CL <- as.numeric(depression_data$Lower.95..CL)
depression_data$Upper.95..CL <- as.numeric(depression_data$Upper.95..CL)

# Descriptive Analysis
# Scatterplot of Percent by Year colored by Strata Name
ggplot(depression_data, aes(x = Year, y = Percent, color = Strata.Name)) +
  geom_point() +
  labs(title = "Scatterplot of Depression Percent over Years", x = "Year", y = "Percent") +
  theme_minimal()

# Boxplot comparing Strata Name and Lower 95% CL
ggplot(depression_data, aes(x = Strata.Name, y = Lower.95..CL, fill = Strata.Name)) +
  geom_boxplot() +
  labs(title = "Boxplot of Lower 95% CL by Strata Name", x = "Strata Name", y = "Lower 95% CL") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot comparing Strata Name and Upper 95% CL
ggplot(depression_data, aes(x = Strata.Name, y = Upper.95..CL, fill = Strata.Name)) +
  geom_boxplot() +
  labs(title = "Boxplot of Upper 95% CL by Strata Name", x = "Strata Name", y = "Upper 95% CL") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for Year and Percent
ggplot(depression_data, aes(x = as.factor(Year), y = Percent)) +
  geom_boxplot(fill = "turquoise3", colour = "black") +
  labs(title = "Boxplot of Percent by Year", x = "Year", y = "Percent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(glmnet)

# Prepare the model matrix for the predictors
x_matrix <- model.matrix(~ . - Percent, data = depression_data)

# Perform cross-validation for lasso regression
# Note that glmnet standardizes the variables by default
cv_lasso <- cv.glmnet(x_matrix, depression_data$Percent, alpha = 1, nfolds = 10)

# Extract the cross-validated R-squared values
# Since glmnet doesn't directly provide R-squared, we can use the mean squared error
# R-squared is 1 - (MSE of model / MSE of null model)
mse_ratio <- cv_lasso$cvm / mean((depression_data$Percent - mean(depression_data$Percent))^2)
r_squared_cv <- 1 - mse_ratio

# Plot the cross-validated R-squared values against log(lambda)
plot(log(cv_lasso$lambda), r_squared_cv, type = 'b', pch = 19, col = 'blue',
     xlab = 'Log(Lambda)', ylab = 'Cross-Validated R-squared',
     main = 'Cross-Validation R-squared vs. Log(Lambda)')

# Add a vertical line for the optimal lambda value
abline(v = log(cv_lasso$lambda.min), col = "red", lwd = 2, lty = 2)
points(log(cv_lasso$lambda.min), 1 - (min(cv_lasso$cvm) / mean((depression_data$Percent - mean(depression_data$Percent))^2)), 
       col = "red", pch = 19)

# Add a legend to the plot
legend('bottomleft', legend = c('Cross-Validation R-squared', 'Optimal Lambda'),
       col = c('blue', 'red'), lty = c(1, 2), pch = c(19, NA), lwd = c(1, 2))


# Extract BIC for the lasso model
lasso_model <- glmnet(x_matrix, depression_data$Percent, alpha = 1)
lasso_pred <- predict(lasso_model, s = cv_lasso$lambda.min, newx = x_matrix)
rss <- sum((depression_data$Percent - lasso_pred)^2)
n <- nrow(x_matrix)
effective_df <- sum(coef(lasso_model, s = cv_lasso$lambda.min) != 0)
bic_lasso <- n * log(rss / n) + effective_df * log(n)
print(paste("BIC for Lasso Model: ", bic_lasso))

# BIC of linear model for comparison
model_linear <- lm(Percent ~ ., data = depression_data)
bic_linear <- BIC(model_linear)
print(paste("BIC for Linear Model: ", bic_linear))

# Logistic Regression Model Summary
# Assuming HighRisk is defined as Percent greater than median value of Percent
depression_data$HighRisk <- ifelse(depression_data$Percent > median(depression_data$Percent, na.rm = TRUE), 1, 0)

# Convert HighRisk to a factor for logistic regression
depression_data$HighRisk <- as.factor(depression_data$HighRisk)

# Logistic Regression Model
logistic_model <- glm(HighRisk ~ Year + Strata, family = binomial(), data = depression_data)

# Summary of Logistic Regression Model
summary(logistic_model)

#  10-fold Cross Validation Plot
# Set a seed for reproducibility
set.seed(123)

# Prepare a 10-fold cross-validation
cv_folds <- createFolds(depression_data$HighRisk, k = 10)

# Initialize a list to store model accuracy for each fold
accuracies <- vector(mode = "list", length = 10)

# Loop through each fold, training and testing the glm model
for(i in seq_along(cv_folds)) {
  # Split the data into training and test sets
  train_indices <- cv_folds[[i]]
  train_set <- depression_data[-train_indices, ]
  test_set <- depression_data[train_indices, ]
  
  # Fit the model on the training set
  glm_model <- glm(HighRisk ~ Year + Strata, family = binomial(), data = train_set)
  
  # Predict on the test set
  predictions <- predict(glm_model, newdata = test_set, type = "response")
  
  # Binarize predictions based on the threshold (0.5 by default)
  predictions_bin <- ifelse(predictions > 0.5, 1, 0)
  
  # Calculate accuracy
  actuals <- test_set$HighRisk
  accuracies[[i]] <- mean(predictions_bin == actuals)
}

# Calculate the average accuracy over all folds
average_accuracy <- mean(unlist(accuracies))
print(average_accuracy)

# Plot the accuracy of each fold
plot(unlist(accuracies), type = 'b', pch = 19, xlab = "Fold", ylab = "Accuracy", main = "10-fold Cross-Validation Accuracy")



# Extract coefficients at the optimal lambda value
lasso_coeffs <- coef(cv_lasso, s = "lambda.min")

# Convert the coefficients to a regular matrix for easier handling
lasso_coeffs_matrix <- as.matrix(lasso_coeffs)

# Print non-zero coefficients, filtering out the zero ones (first row is the intercept)
non_zero_coeffs <- lasso_coeffs_matrix[lasso_coeffs_matrix[,1] != 0, , drop = FALSE]
print(non_zero_coeffs)



# Check for aliased (perfectly collinear) coefficients in the model
aliased_coeffs <- alias(model_linear)$Complete
print(aliased_coeffs)

adjusted_model_linear <- lm(Percent ~ . -Weighted.Frequency, data = depression_data)

# Summary to check new model
summary(adjusted_model_linear)


# Summary of the linear regression model
summary_linear <- summary(model_linear)
print(summary_linear)


# Generate and print PCA loading plots
fviz_pca_var(pca_results, col.var = "contrib") + 
  labs(title = "PCA Loading Plot", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()




# Calculating mean percent by year
mean_percent_data <- depression_data %>%
  group_by(Year) %>%
  summarize(MeanPercent = mean(Percent, na.rm = TRUE))

# Scatter plot of mean percent over years
ggplot(mean_percent_data, aes(x = Year, y = MeanPercent)) +
  geom_point() +
  geom_line() +
  labs(title = "Mean Depression Percent Over Years", x = "Year", y = "Mean Percent") +
  theme_minimal()



# Calculating median percent by year
median_percent_data <- depression_data %>%
  group_by(Year) %>%
  summarize(MedianPercent = median(Percent, na.rm = TRUE))

# Scatter plot of median percent over years
ggplot(median_percent_data, aes(x = Year, y = MedianPercent)) +
  geom_point() +
  geom_line(color = "blue") +
  labs(title = "Median Depression Percent Over Years", x = "Year", y = "Median Percent") +
  theme_minimal()




# Hierarchical Clustering
# Data preprocessing: remove NA values and convert necessary columns
depression_data <- na.omit(depression_data)
# Assuming we are using numeric columns for clustering, identify numeric columns
numeric_columns <- depression_data %>% select_if(is.numeric)

# Standardize the numeric data for clustering
standardized_data <- scale(numeric_columns)

# Perform hierarchical clustering with different methods
clustering_methods <- c("single", "complete", "average")
colors_for_methods <- c("red", "blue", "green") # Specific colors for each method

# Initialize an empty list to save the hclust results for later use
hc_results <- list()

# Loop through each clustering method to perform and plot the hierarchical clustering
for (i in seq_along(clustering_methods)) {
  method <- clustering_methods[i]
  
  # Compute hierarchical clustering
  hc_result <- hclust(dist(standardized_data), method = method)
  hc_results[[method]] <- hc_result # Saving the result for possible future use
  
  # Plot dendrogram with specific color and main title
  plot(hc_result, main = paste("Dendrogram using", method, "linkage"), col = colors_for_methods[i])
  
  # Adding rectangles for 5 clusters with distinct borders
  rect.hclust(hc_result, k = 5, border = 5:9)
}

# Optionally, you can also visualize the dendrograms using 'fviz_dend' from factoextra for a more aesthetic plot:
for (method in names(hc_results)) {
  fviz_dend(hc_results[[method]], main = paste("Dendrogram using", method, "linkage"), rect = TRUE, k = 5,
            show_labels = FALSE, # Set TRUE to show labels
            color_labels_by_k = TRUE, # Color labels based on clusters
            k_colors = colors_for_methods) +
    labs(title = paste("Dendrogram using", method, "linkage"))
}

# PCA Analysis
# Preprocess data: removing NA values and selecting numeric columns
depression_data <- na.omit(depression_data)
numeric_columns <- depression_data %>% select_if(is.numeric)

# Perform PCA on the numeric columns
pca_results1 <- prcomp(numeric_columns, scale. = TRUE)

# Plotting biplot using base R
biplot(pca_results1, col = c("red", "orange"))





# Preprocess data: removing NA values and selecting numeric columns
depression_data <- na.omit(depression_data)
numeric_columns <- depression_data %>% select_if(is.numeric)

# Perform PCA on the numeric columns
pca_results1 <- prcomp(numeric_columns, scale. = TRUE)

# Assuming 'Strata.Name' or another column represents different regions or categories akin to "Country"
# Update the column name accordingly if different
scores_df1 <- data.frame(Strata = depression_data$Strata.Name, PC1_Score = pca_results1$x[, 1]) %>%
  arrange(desc(PC1_Score))

print(scores_df1)

# Create a bar plot for the PC1 scores
bar_plot <- ggplot(scores_df1, aes(x = reorder(Strata, PC1_Score), y = PC1_Score, fill = PC1_Score)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0) +
  labs(title = "Ranking of Strata Based on First Principal Component Score",
       x = "Strata",
       y = "First Principal Component Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        axis.title.x = element_text(margin = margin(t = 20, b = 20)),
        plot.margin = unit(c(1, 1, 1, 1), "lines")) +
  guides(fill=guide_legend(title="PC1 Score"))

print(bar_plot)


#Scree plot
# Preprocess data: removing NA values
depression_data <- na.omit(depression_data)

# Exclude non-numeric columns and retain only numeric data for PCA
# Adjust the exclusion list according to your dataset's specific non-numeric columns
numeric_data1 <- depression_data %>% select_if(is.numeric)

# Standardize the data
standardized_data1 <- scale(numeric_data1)

# Perform PCA
pca_results1 <- prcomp(standardized_data1, scale. = TRUE)

# Visualize the eigenvalues/variance explained by each principal component
scree_plot1 <- fviz_eig(pca_results1, addlabels = TRUE, ylim = c(0, 100), barfill = "turquoise", barcolour = "blue") +
  labs(title = "Scree Plot of PCA",
       x = "Principal Components",
       y = "Variance Explained (%)") +
  theme_minimal()
print(scree_plot1)

# Defining HighRisk based on Percent
depression_data$HighRisk <- ifelse(depression_data$Percent > median(depression_data$Percent, na.rm = TRUE), 10, 1)
depression_data$HighRisk <- as.factor(depression_data$HighRisk)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
training_rows <- createDataPartition(depression_data$HighRisk, p = 0.8, list = FALSE)
trainData <- depression_data[training_rows, ]
testData <- depression_data[-training_rows, ]

# Fit the logistic regression model
logistic_model <- glm(HighRisk ~ Year + Strata, family = binomial(), data = trainData)

# Generate Predictions on test data
predictions <- predict(logistic_model, newdata = testData, type = "response")
predictions_class <- ifelse(predictions > 0.5, 10, 1)  # Convert to class labels

# Print the predictions (probabilities and class labels)
print("Predictions (Probabilities):")
print(predictions)

print("Predictions (Class Labels):")
print(predictions_class)

# calculated accuracy
correct_predictions <- testData$HighRisk == predictions_class
accuracy <- mean(correct_predictions)
print(paste("Accuracy:", accuracy))




# Assuming 'HighRisk' as a binary factor based on 'Percent'
depression_data$HighRisk <- ifelse(depression_data$Percent > median(depression_data$Percent, na.rm = TRUE), 1, 0)
depression_data$HighRisk <- as.factor(depression_data$HighRisk)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
training_rows <- createDataPartition(depression_data$HighRisk, p = 0.8, list = FALSE)
trainData <- depression_data[training_rows, ]

# 10-fold Cross-validation setup
library(boot)
cv.error.10 <- rep(0, 10)
set.seed(1)

for (i in 1:10) {
  # Assuming using 'Percent' and other numeric predictors for the model
  glm.fit <- glm(HighRisk ~ Year + Frequency + Weighted.Frequency + Lower.95..CL + Upper.95..CL, 
                 data=trainData, family=binomial())
  cv.error.10[i] <- cv.glm(trainData, glm.fit, K=10)$delta[1]
}

# Print cross-validation errors
print("Cross-validation Errors:")
print(cv.error.10)