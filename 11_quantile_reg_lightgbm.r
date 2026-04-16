#
# Quantile regression in LightGBM
#
# Try first for p = 1, then p = 10. Can you explain why the fitted values in the plots look different? 



# Load necessary libraries
library(lightgbm)
library(data.table)

# Set seed for reproducibility
set.seed(123)

# Generate example data
n <- 1000  # number of samples
p <- 1    # number of features
stde <- 0.2 # standard deviation of errors

# Define the quantile (e.g., 0.5 for median regression)
quantile <- 0.1

# Generate data 
x      = matrix(rnorm(n*p),n,p)   # a (n,p) matrix of independent normal
x1     = x[,1]
logstd = 0.0 + bh*x1
stdtrue = exp(logstd)
Eyx     = bm*x1      # true expected value of y given x
y       = Eyx + stdtrue*rnorm(n)


# Create a data frame with random features and a target variable
data <- data.table(x)
data[, target := y]  # Linear relationship with noise

# Split the data into training and validation sets
train_indices <- sample(1:n, size = 0.8 * n)
train_data <- data[train_indices]
valid_data <- data[-train_indices]

# Convert the data to LightGBM datasets
dtrain <- lgb.Dataset(data = as.matrix(train_data[, -"target", with = FALSE]), label = train_data$target)
dvalid <- lgb.Dataset(data = as.matrix(valid_data[, -"target", with = FALSE]), label = valid_data$target)

# Define LightGBM parameters for quantile regression
params <- list(
  objective = "quantile",  # Quantile regression objective
  metric = "quantile",     # Evaluation metric
  alpha = quantile,        # Quantile to estimate
  boosting = "gbdt",       # Gradient boosting decision tree
  learning_rate = 0.1,
  max_depth = 6,
  num_leaves = 31
)

# Train the LightGBM model
model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  valids = list(valid = dvalid),
  early_stopping_rounds = 100,
  verbose = 0
)

# Make predictions on the validation set
pred1 <- predict(model,x)

# Repeat for alpha=1-quantile 

params <- list(
  objective = "quantile",  # Quantile regression objective
  metric = "quantile",     # Evaluation metric
  alpha = 1-quantile,        # Quantile to estimate
  boosting = "gbdt",       # Gradient boosting decision tree
  learning_rate = 0.1,
  max_depth = 6,
  num_leaves = 31
)

# Train the LightGBM model
model2 <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  valids = list(valid = dvalid),
  early_stopping_rounds = 100,
  verbose = 0
)

# Make predictions on the validation set
pred2 <- predict(model2,x)

# 
plot(x[,1],y, type = 'p',main = "heteroskedastic data",col = "black")
lines(x1,type = 'p',pred1, col = "blue")
lines(x1,type = 'p',pred2, col = "blue")
