#' @name 06_train_catboost.R
#' @date 13.02.2026
#' @author Marc Kevin Schneider & Lukas Esselmann
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Script for the training of a catboost model.
#' 
#' Original code for the model training by Hanna Meyer. Adjusted from ranger to 
#' catboost by Marc Kevin Schneider.
#' 

#library(remotes)
#remotes::install_github('catboost/catboost', subdir = 'catboost/R-package')

# ================================================================
# 1. Load packages
# ================================================================

library(caret)
library(ranger)
library(doParallel)
library(terra)
library(sf)
library(CAST)
library(pROC)
library(catboost)
library(PresenceAbsence)

set.seed(12345)


# ================================================================
# 2. Read data
# ================================================================

path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Daten/"
rawtrainDF <- readRDS(paste0(path, "Merged_Tornado_RandomPoints_Parameters_1995_2024.rds"))

# ================================================================
# 3. Downsampling (handle class imbalance)
# ================================================================

downTrainDF <- downSample(
  x = rawtrainDF[, -ncol(rawtrainDF)],
  y = factor(rawtrainDF$tornado_occurred,
             levels = c(1, 0),
             labels = c("Tornado", "NoTornado"))
)

df <- downTrainDF[, !(names(downTrainDF) %in% c("class", "OBJ_ID", "Class"))]
df$class <- downTrainDF$Class
df <- na.omit(df)

# ================================================================
# 4. 80/20 Stratified Train/Test Split
# ================================================================

train_index <- createDataPartition(df$class, p = 0.8, list = FALSE)

train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

cat("Training samples:", nrow(train_data), "\n")
cat("Testing samples :", nrow(test_data), "\n")


# ================================================================
# 5. Feature Filtering
# ================================================================

# Remove near zero variance
nzv <- nearZeroVar(train_data[, -which(names(train_data) == "class")])
if (length(nzv) > 0) {
  train_data <- train_data[, -nzv]
  test_data  <- test_data[, -nzv]
}

# Remove high correlations
cor_matrix <- cor(train_data[, -which(names(train_data) == "class")],
                  use = "complete.obs")

high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)

if (length(high_cor) > 0) {
  train_data <- train_data[, -high_cor]
  test_data  <- test_data[, -high_cor]
}

# Remove linear combinations
lin_combo <- findLinearCombos(train_data[, -which(names(train_data) == "class")])

if (!is.null(lin_combo$remove)) {
  train_data <- train_data[, -lin_combo$remove]
  test_data  <- test_data[, -lin_combo$remove]
}

# ================================================================
# 6. Preparation for catboost
# ================================================================

# Convert class to numeric 0/1
train_label <- ifelse(train_data$class == "Tornado", 1, 0)
test_label  <- ifelse(test_data$class  == "Tornado", 1, 0)

# train matrix
train_matrix <- as.matrix(train_data[, -which(names(train_data) == "class")])

# test matrix
test_matrix  <- as.matrix(test_data[,  -which(names(test_data) == "class")])


# load pool for the model
train_pool <- catboost.load_pool(data = train_matrix,
                                 label = train_label)

test_pool  <- catboost.load_pool(data = test_matrix,
                                 label = test_label)

# ================================================================
# 7. Train catboost model
# ================================================================

# hypertuning parameters
params <- list(
  loss_function = "Logloss",
  eval_metric = "AUC",
  iterations = 2000,
  depth = 6,
  learning_rate = 0.03,
  l2_leaf_reg = 3,
  random_seed = 12345,
  od_type = "Iter",
  od_wait = 50,
  use_best_model = TRUE,
  verbose = 100
)

# train model
cat_model <- catboost.train(
  learn_pool = train_pool,
  test_pool = test_pool,
  params = params
)

# ================================================================
# 8. Predicting for the test dataset
# ================================================================

test_pred_prob <- catboost.predict(
  cat_model,
  test_pool,
  prediction_type = "Probability"
)


# Convert observed class to numeric 0/1
obs_numeric <- ifelse(test_data$class == "Tornado", 1, 0)


# from https://gitup.uni-potsdam.de/macroecology/mecofun/-/blob/master/R/evalSDM.R?ref_type=heads
# uses the code from parts of the evalSDM function
# finds the optimal threshold for maximizing sensitivity and specificity

# Create threshold dataframe
thresh.dat <- data.frame(
  ID   = seq_along(test_pred_prob),
  obs  = obs_numeric,
  pred = test_pred_prob
)

# find threshold
thresh.mat <- optimal.thresholds(DATA = thresh.dat, opt.methods = "MaxSens+Spec")

# optimize for maximum sensitivity and specificity
thresh <- thresh.mat[thresh.mat$Method == "MaxSens+Spec", "pred"]

print(paste0("Optimal threshold:", thresh))

# use threshold
test_pred_class_opt <- ifelse(test_pred_prob >= thresh, "Tornado", "NoTornado")

test_pred_class_opt <- factor(test_pred_class_opt, levels = c("Tornado", "NoTornado"))

test_class_factor <- factor(test_data$class, levels = c("Tornado", "NoTornado"))

# confusion matrix for checking the model
confusionMatrix(
  test_pred_class_opt,
  test_class_factor,
  positive = "Tornado"
)

# roc
roc_obj <- roc(
  response = test_class_factor,
  predictor = test_pred_prob,
  levels = c("NoTornado", "Tornado")
)

print(paste0("AUC:", auc(roc_obj)))

plot(roc_obj, main = "CatBoost ROC Curve - Independent Test Set")


# ================================================================
# 9. Variable importance
# ================================================================

# variable importance
importance <- catboost.get_feature_importance(
  cat_model,
  pool = train_pool,
  type = "FeatureImportance"
)

importance_df <- data.frame(
  Feature = colnames(train_matrix),
  Importance = importance
)

# order from most to least important
importance_df <- importance_df[order(-importance_df$Importance), ]

print(head(importance_df, 20))


# ================================================================
# 10. Save catboost model
# ================================================================

save_path <- "C:/Users/kevis/OneDrive/Desktop/Unisachen/Master/Fernerkundung_GIS_WS2024/Models/"
dir.create(save_path, showWarnings = FALSE, recursive = TRUE)

catboost.save_model(cat_model, paste0(save_path, "catboost_model_80_20_split.cbm"))


# ================================================================
# 11. Save model parameters for convective parameters
# ================================================================

# get the feature names
feature_names <- catboost.get_feature_importance(
  cat_model,
  pool = train_pool,
  type = "FeatureImportance"
)

names(feature_names) <- colnames(train_matrix)
used_features <- names(feature_names)


write.csv(data.frame(Feature = used_features), 
          paste0(save_path, "catboost_used_predictors.csv"),row.names = FALSE)
