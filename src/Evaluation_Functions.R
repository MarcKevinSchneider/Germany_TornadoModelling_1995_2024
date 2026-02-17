#' @name Evaluation_Functions.R
#' @date 16.12.2025
#' @author Marc Kevin Schneider
#' @contact Schnei7q@students.uni-marburg.de
#' 
#' @description
#' Functions for evaluating the model using AUC, TSS, RMSE, MAE, 
#' Pearson's Correlation, Jaccard's Similarity Index and Sorensen's Similarity Index

# ================================================================
# 1. Evaluation functions
# ================================================================


# first few metrics from https://cran.r-project.org/web/packages/Metrics/Metrics.pdf

# 1 - AUC ####
#-----------------------------------------#

auc_eval <- function(df){
  '
  Purpose: Helper function for calculating the AUC
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  AUC
  '
  AUC <- Metrics::auc(actual = df$tornado_occurred, predicted = df$pred_class)
  #print("Successfully calculated AUC.")
  
  return(AUC)
}

# 2 - MAE ####
#-----------------------------------------#

mae_eval <- function(df){
  '
  Purpose: Helper function for calculating the MAE
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  MAE
  '
  MAE <- Metrics::mae(actual = df$tornado_occurred, predicted = df$pred_class)
  #print("Successfully calculated MAE.")
  return(MAE)
}


# 3 - MSE ####
#-----------------------------------------#

rmse_eval <- function(df){
  '
  Purpose: Helper function for calculating the RMSE
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  RMSE
  '
  RMSE <- Metrics::rmse(actual = df$tornado_occurred, predicted = df$pred_class)
  #print("Successfully calculated RMSE.")
  return(RMSE)
}

# 4 - TSS ###
#-----------------------------------------#

tss_eval <- function(df){
  '
  Purpose: Helper function for calculating the TSS
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  TSS
  '
  # true positives
  tp <- sum(df$pred_class == 1 & df$tornado_occurred == 1)
  # false negatives
  fn <- sum(df$pred_class == 0 & df$tornado_occurred == 1)
  # false positives
  fp <- sum(df$pred_class == 1 & df$tornado_occurred == 0)
  # true negatives
  tn <- sum(df$pred_class == 0 & df$tornado_occurred == 0)
  
  # formula for calculating TSS
  # have to do it this way since "Metrics" doesnt have a TSS function
  TSS <- (tp / (tp + fn)) - (fp / (fp + tn))
  #print("Successfully calculated TSS.")
  return(TSS)
}


# 5 - Pearson's Correlation Coefficient ###
#-----------------------------------------#

cor_eval <- function(df){
  '
  Purpose: Helper function for calculating Pearsons Correlation Coefficient
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  Pearsons R
  '
  pearson_r <- cor(
    as.numeric(df$pred_class),
    as.numeric(df$tornado_occurred),
    method = "pearson",
    use = "complete.obs"
  )
  #print("Successfully calculated Pearson's R.")
  return(pearson_r)
}

# 6 - Jaccard's Similarity Index ###
#-----------------------------------------#

# from https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r-2/

jaccard_eval <- function(df){
  '
  Purpose: Helper function for calculating Jaccards Similarity Index
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  Jaccards Similarity Index
  '
  
  # from https://www.davidzeleny.net/anadat-r/doku.php/en:similarity
  
  # true positive
  tp <- sum(df$pred_class == 1 & df$tornado_occurred == 1)

  # false positive
  fp <- sum(df$pred_class == 1 & df$tornado_occurred == 0)

  # false negative
  fn <- sum(df$pred_class == 0 & df$tornado_occurred == 1)
  
  JAC <- tp / (tp + fp + fn)
  #print("Successfully calculated Jaccards Similarity Index.")
  return(JAC)
  
}

# 7 - Jaccard's Dissimilarity Index ###
#-----------------------------------------#

# just to have it in case it is needed
# from https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r-2/

jaccard_distance <- function(df){
  '
  Purpose: Helper function for calculating Jaccards Dissimilarity Index
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  Jacards Dissimilarity Index
  '
  jacard <- jaccard_eval(df)
  JAC_DIS <- 1 - jacard
  #print("Successfully calculated Jaccards Distance Index.")
  return(JAC_DIS)
}

# 7 - Sorensen's Similarity Index ###
#-----------------------------------------#

sorensen_eval <- function(df){
  '
  Purpose: Helper function for calculating Sorensens Similarity Index
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  Sorensens Similarity Index
  '
  # from https://www.davidzeleny.net/anadat-r/doku.php/en:similarity
  
  # true positive
  tp <- sum(df$pred_class == 1 & df$tornado_occurred == 1)
  # false positive
  fp <- sum(df$pred_class == 1 & df$tornado_occurred == 0)
  # false negative
  fn <- sum(df$pred_class == 0 & df$tornado_occurred == 1)
  
  # calc sorensen's
  SOREN <- (2 * tp) / (2 * tp + fp + fn)
  #print("Successfully calculated Sorensen's Similarity Index.")
  return(SOREN)
}

# 8 - Function for executing all metrics ##
#-----------------------------------------#

eval_funcs <- function(df){
  '
  Purpose: Helper function for calculating all evaluation metrics
  
  Parameters:
  ---------------------------------
  
  df: dataframe
    Dataframe of the actual distribution and artificial distribution map sample data
    Must contain the columns "pred_class" and "tornado_occurred
    
  
  Returns:
  --------------------------
  List of all evaluation metrics
  '
  # AUC
  AUC <- auc_eval(df)
  # MAE
  MAE <- mae_eval(df)
  # RMSE
  RMSE <- rmse_eval(df)
  # TSS
  TSS <- tss_eval(df)
  # Pearson R
  COR <- cor_eval(df)
  # Jaccards Similarity index
  JAC <- jaccard_eval(df)
  # Jaccards Dissimilarity Index
  DIS <- jaccard_distance(df)
  # Sorensens Similarity Index
  SOR <- sorensen_eval(df)
  
  return(list(AUC=AUC, MAE=MAE, RMSE=RMSE, TSS=TSS, COR=COR,
              JAC=JAC, DIS=DIS, SOR=SOR))
}