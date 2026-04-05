#
# Simple example of the use of lightgbm in R. 
#


# Load necessary libraries
library(lightgbm)
library(data.table)

# Set seed for reproducibility
set.seed(1)

# Generate some example data
n <- 1000  # number of samples
p <- 10    # number of features

# Create a data frame with random features and a target variable
data <- data.table(matrix(rnorm(n * p), nrow = n, ncol = p))
data[, target := rnorm(n)]

# Split the data into training and validation sets
train_indices <- sample(1:n, size = 0.8 * n)
train_data <- data[train_indices]
valid_data <- data[-train_indices]

# Convert the data to LightGBM datasets
dtrain <- lgb.Dataset(data = as.matrix(train_data[, -p, with = FALSE]), label = train_data$target)
dvalid <- lgb.Dataset(data = as.matrix(valid_data[, -p, with = FALSE]), label = valid_data$target)

# Define the parameters for the LightGBM model
params <- list(
  objective = "regression",
  metric = "rmse",
  num_leaves = 31,
  learning_rate = 0.05,
  feature_fraction = 0.9
)

# Train the model
model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  valids = list(valid = dvalid),
  early_stopping_rounds = 10,
  verbose = -1
)

# Make predictions on the validation set
preds <- predict(model, as.matrix(valid_data[, -p, with = FALSE]))

# Calculate the RMSE
rmse <- sqrt(mean((preds - valid_data$target)^2))
cat("RMSE on validation set:", rmse, "\n")