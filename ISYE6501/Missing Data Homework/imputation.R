library("caret")
library("e1071")

# read in data
bcw <- read.table("breast-cancer-wisconsin.data.txt", stringsAsFactors = FALSE, header = FALSE, sep = ",")

# Inspect data to help us identify missing data
summary(bcw)
str(bcw)
# V7 is filled with characters, this row sticks out

# Isolate V7 to inspect data further
table(bcw$V7)
# 16 values are '?' 
# That is the missing data

# impute gives the indices of data values that are missing (?)
impute <- which(bcw$V7 == "?")
impute

# Let's see how much of our data is missing, as a general rule we don't want to impute more than 5% of data
missing_data <- length(impute)/nrow(bcw)
missing_data
# 2.29% is less than 5%, so imputing this data is okay

# Inspect the data, specifically the missing data, to see if there is a bias
# bcw_clean will be the data with the missing vales taken out
# bcw_missing will be the data with the missing values in V7
# I create 4 bcw_missing data sets to allow me to use 4 different methods of imputation later on
# without worrying about overlapping data sets
bcw_clean <- bcw[-impute,]
bcw_missing_mean <- bcw[impute,]
bcw_missing_mode <-bcw[impute,]
bcw_missing_reg <- bcw[impute,]

# compare the response variables, V11, for the original data set, bcw_clean, and bcw_missing
orig_benign <- sum(bcw$V11 == 2)/nrow(bcw)
clean_benign <- sum(bcw_clean$V11 == 2)/nrow(bcw_clean)
missing_benign <- sum(bcw_missing_mean$V11 == 2)/nrow(bcw_missing_mean)
orig_benign
clean_benign
missing_benign
# The percentage of benign responses in the original data and clean_bca data set are similar, and the the percentage of 
# benign responses in the missing data set is 87.5% which is not too far off. We would be more careful if this percentage was
# either all benign responses, or all malignant responses as that would be more indicative of a bias in missing data.

# Method 1a: We will calculate the mean of V7 in bcw_clean and substitute that value in for all missing values in V7
impute_mean <- mean(as.numeric(bcw_clean$V7))
impute_mean

# all missing values in bca_missing will be substituted with impute_mean
bcw_missing_mean$V7 <- impute_mean
# confirm the updated bcw_missing data set has no missing values(?) in V7
bcw_missing_mean

# Method 1b: We will calculate the mode of V7 in bcw_clean and substitute that value in for all missing values in V7
# First define function mode to identify mode of column
mode <- function(x) {
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}
impute_mode <- mode(as.numeric(bcw_clean$V7))
impute_mode

# all missing values in bcw_missing will be substituted with impute_mode
bcw_missing_mode$V7 <- impute_mode
# confirm the updated bcw_missing data set has no missing values (?) in V7
bcw_missing_mode

# Method 2: Use Regression to impute values for missing data
# I used symbolic number coding to visualize the correlation between predictors for the bcw_clean data set
bcw_clean$V7 <- as.numeric(bcw_clean$V7)
symnum(cor(bcw_clean))
str(bcw_clean)
# This tells me that V7 is most correlated with V8 (0.8), V9 (0.6), V10 (0.6), and V11 (0.95)
# I do not think using the response variable, V11, to predict V7 is a good idea because it seems recursive. 
# V11 is originally predicted using all predictors, including V7, so to use it to go back and predict V7 does not seem like a good idea to me
impute_regression <- lm(V7 ~ V8 + V9 + V10, data = bcw_clean)
impute_regression
# The model that we can use to predict V7 is:
# V7 = -0.05681 + V8 * 0.76848 + V9 * 0.25114 + V10 * 0.14550
# We will loop through all missing values in V7 to get predicted values from our regression model to use

for(i in 1:length(bcw_missing_reg$V7))
{
  bcw_missing_reg$V7[i] = as.integer(-0.05681 + (0.76848 * bcw_missing_reg$V8[i]) + (0.25114 * bcw_missing_reg$V9[i]) + (0.14550 * bcw_missing_reg$V10[i]))
  }
bcw_missing_reg


# Method 3: Use Regression with Perturbation to impute values for missing data

bcw_missing_reg_perturb <- bcw_missing_reg[,-7]
# Take away V7 from the data set, as we will be replacing it with new values

perturb <- rnorm(length(impute), mean(as.numeric(bcw_clean$V7)), sd(as.numeric(bcw_clean$V7)))
# perturb will add random values based on the normal distribution, using the mean and standard deviation from the V7 column of bcw_clean
bcw_missing_reg_perturb_v7 <- round(as.numeric(bcw_missing_reg$V7) + perturb)
# These are the new values of V7 after adding our perturbations
bcw_missing_reg_perturb <- cbind(bcw_missing_reg_perturb, bcw_missing_reg_perturb_v7)
# Use cbind to combine columns
names(bcw_missing_reg_perturb)[11] <- 'V7'
# Rename the perturbation column to V7
bcw_missing_reg_perturb <- bcw_missing_reg_perturb[,c(1,2,3,4,5,6,11,7,8,9,10)]
# Reorder the columns to match the original order of the data set 
bcw_missing_reg_perturb[bcw_missing_reg_perturb > 10] <- 10
bcw_missing_reg_perturb[bcw_missing_reg_perturb < 1] <- 1
# If our perturbations cause a value above 10, make it 10
# If our perturbations cause a value below 1, make it 1
bcw_missing_reg_perturb


# We now have four sets of complete data using four imputation methods:
# 1) Mean, 2) Mode, 3) Regression, 4) Regression with Perturbation
# Use bind to combine bcw_clean with the imputed bcw
bcw_imputed_mean <- rbind(bcw_clean, bcw_missing_mean)
bcw_imputed_mode <- rbind(bcw_clean, bcw_missing_mode)
bcw_imputed_reg <- rbind(bcw_clean, bcw_missing_reg)
bcw_imputed_reg_perturb <- rbind(bcw_clean, bcw_missing_reg_perturb)

# SVM model using data set using imputed mean
# I will use 80/20 training/test split for the comparison of model quality for all models
sample_size <- floor(0.8 * nrow(bcw))

set.seed(11)
train_ind <- sample(seq_len(nrow(bcw)), size = sample_size)
# I will use train_ind for all models tested

# SVM model using data set completed using mean imputation method
bcw_imputed_mean_training <- bcw_imputed_mean[train_ind, ]
bcw_imputed_mean_testing <- bcw_imputed_mean[-train_ind, ]

svm_bcw_mean_model <- svm(V11 ~. , data = bcw_imputed_mean_training)
svm_bcw_mean_model_pred <- predict(svm_bcw_mean_model, newdata = bcw_imputed_mean_testing)
svm_bcw_mean_model_pred[svm_bcw_mean_model_pred < 3] <- 2
svm_bcw_mean_model_pred[svm_bcw_mean_model_pred >= 3] <- 4
# Since the response variable is either 2 or 4 I made the cutoff at 3.
# 3 or more is a prediction of 4 (Malignant)
# Less than 3 is a prediction of 2 (Benign)
# Create a function that takes the predicted values from the SVM model and chages the responses to 2 or 4
ResponseFunction <- function(x) {
  x[x < 3] <- 2
  x[x >= 3] <- 4
  return(x)
}

svm_mean_ConfMatrix <- confusionMatrix(data=factor(svm_bcw_mean_model_pred), reference=factor(bcw_imputed_mean_testing$V11))

# SVM model using data set completed using mode imputation method
bcw_imputed_mode_training <- bcw_imputed_mode[train_ind, ]
bcw_imputed_mode_testing <- bcw_imputed_mode[-train_ind, ]

svm_bcw_mode_model <- svm(V11 ~., data = bcw_imputed_mode_training)
svm_bcw_mode_model_pred <- predict(svm_bcw_mode_model, newdata = bcw_imputed_mode_testing)
svm_bcw_mode_model_pred <- ResponseFunction(svm_bcw_mode_model_pred)

svm_mode_ConfMatrix <- confusionMatrix(data = factor(svm_bcw_mode_model_pred), reference = factor(bcw_imputed_mode_testing$V11))

# SVM model using data set completed using regression imputation method
bcw_imputed_reg_training <- bcw_imputed_reg[train_ind, ]
bcw_imputed_reg_testing <- bcw_imputed_reg[-train_ind, ]

svm_bcw_reg_model <- svm(V11 ~., data = bcw_imputed_reg_training)
svm_bcw_reg_model_pred <- predict(svm_bcw_reg_model, newdata = bcw_imputed_reg_testing)
svm_bcw_reg_model_pred <- ResponseFunction(svm_bcw_reg_model_pred)

svm_reg_ConfMatrix <- confusionMatrix(data = factor(svm_bcw_reg_model_pred), reference = factor(bcw_imputed_reg_testing$V11))

# SVM model using data set completed using regression with perturbation imputation method
bcw_imputed_reg_perturb_training <- bcw_imputed_reg_perturb[train_ind, ]
bcw_imputed_reg_perturb_testing <- bcw_imputed_reg_perturb[-train_ind, ]

svm_bcw_reg_perturb_model <- svm(V11 ~., data = bcw_imputed_reg_perturb_training)
svm_bcw_reg_perturb_model_pred <- predict(svm_bcw_reg_perturb_model, newdata = bcw_imputed_reg_perturb_testing)
svm_bcw_reg_perturb_model_pred <- ResponseFunction(svm_bcw_reg_perturb_model_pred)

svm_reg_perturb_ConfMatrix <- confusionMatrix(data = factor(svm_bcw_reg_perturb_model_pred), reference = factor(bcw_imputed_reg_perturb_testing$V11))

# SVM model using data set missing values removed
bcw_clean_training <- bcw_clean[train_ind, ]
bcw_clean_testing <- bcw_clean[-train_ind, ]

svm_bcw_clean_model <- svm(V11 ~., data = bcw_clean_training)
svm_bcw_clean_model_pred <- predict(svm_bcw_clean_model, newdata = bcw_clean_testing)
svm_bcw_clean_model_pred <- ResponseFunction(svm_bcw_clean_model_pred)

svm_clean_ConfMatrix <- confusionMatrix(data = factor(svm_bcw_clean_model_pred), reference = factor(bcw_clean_testing$V11))

# SVM model using data set with binary variable to indicate is values are missing
# Create column to indicate if value is missing
bcw_binary <- bcw
bcw_binary$V7.5 <- ""
bcw_binary <- bcw_binary[,c(1,2,3,4,5,6,7,12,8,9,10,11)]
for(i in 1:length(bcw_binary$V7))
{
  if(bcw_binary$V7[i] == "?"){
    bcw_binary$V7.5[i] <- "Missing"
  } else {
    bcw_binary$V7.5[i] <- "Not Missing"
  } 
}

bcw_binary_training <- bcw_binary[train_ind, ]
bcw_binary_testing <- bcw_binary[-train_ind, ]
bcw_binary

svm_bcw_binary_model <- svm(V11 ~., data = bcw_binary_training)
svm_bcw_binary_model_pred <- predict(svm_bcw_binary_model, newdata = bcw_binary_testing)
svm_bcw_binary_model_pred <- ResponseFunction(svm_bcw_binary_model_pred)

svm_binary_ConfMatrix <- confusionMatrix(data = factor(svm_bcw_binary_model_pred), reference = factor(bcw_binary_testing$V11))


# Comparing the Confusion Matrices of all SVM models to see which model had the highest quality of fit

svm_mean_ConfMatrix
svm_mode_ConfMatrix
svm_reg_ConfMatrix
svm_reg_perturb_ConfMatrix
svm_clean_ConfMatrix
svm_binary_ConfMatrix


# Export Script to HTML
library(knitr)
knitr::stitch_rhtml('imputation.R')
