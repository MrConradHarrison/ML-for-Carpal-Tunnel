
# Install and load packages
install.packages("rsample") 
install.packages("recipes") 
install.packages("parsnip") 
install.packages("workflows") 
install.packages("tune") 
install.packages("dials") 
install.packages("yardstick")
install.packages("glmnet") 
install.packages("keras") 
install.packages("kernlab")
install.packages("ggplot2")
install.packages("performanceEstimation")
install.packages("xgboost")
install.packages("SHAPforxgboost")
install.packages("kknn")
install.packages("CHAID", repos="http://R-Forge.R-project.org")


library(dplyr)
library(rsample) 
library(recipes) 
library(parsnip) 
library(workflows) 
library(tune) 
library(dials) 
library(yardstick)
library(glmnet) 
library(keras) 
library(kernlab)
library(ggplot2)
library(performanceEstimation)
library(xgboost)
library(SHAPforxgboost)
library(kknn)
library(CHAID)

RNGkind(kind = "L'Ecuyer-CMRG")

# Load data
master_data <- readRDS("clean_data.RDS")

# Exclude participants that we can't measure function change in
data <- master_data[-which(is.na(master_data$Improved_Function)),]

# Remove columns that are not needed for this classification task
data[,c(60:71,74,76,77)] <- NULL

# Switch oucome factor levels
data$Improved_Function <- factor(data$Improved_Function,
                                 levels = c("yes", "no"))


# Split data and remove ID column
set.seed(111)
split = initial_split(data, prop = 3 / 4) 
classifier_train_withID = training(split)
classifier_test_withID = testing(split)

classifier_train <- classifier_train_withID[,-1]
classifier_test <- classifier_test_withID[,-1]

# Check class balance in training data
table(classifier_train$Improved_Function)

# Create 10 bootstraps of training data
set.seed(111)
boots <- bootstraps(classifier_train, times = 10) 

# Write preprocessing recipe
recipe <- recipe(Improved_Function ~., data = classifier_train) %>% # Specify formula
  step_dummy(Gender, HandDominance, VibratingToolsYN,
             SmokingStatus, heart, hbp, lung, 
             diabetes, ulcer,
             kidney, liver, anaemia, cancer, depression, osteoarthritis,
             backpain, rheumatoid, Thyroid, KS1, KS2, KS3, KS4, KS5, KS6,
             KS7, KS8, KS9, Employment_Status, Diagnosis, laterality, 
             DominantSideSurgery) %>% # Level encoding of dummy variables
  step_scale(all_numeric_predictors(), factor = 2) %>% # Scale numeric predictors by 2 SDs
  step_impute_mode(all_nominal_predictors()) %>% # Mode imputation for categorical predictors
  step_impute_mean(all_numeric_predictors()) %>% # Mean imputation for continuous predictors
  step_nzv(all_predictors()) %>% # Remove predictors with near zero variance
  prep(training_data = classifier_train)

## Logistic regression with elastic net regularization

# Specify model
log_md <- logistic_reg(penalty = tune(), 
                       mixture = tune()) %>%
          set_engine("glmnet")

# Create a placeholder grid for grid search
penalty_mixture_grid <- grid_regular(penalty(), 
                                     mixture(),
                                     levels = 10)

# Specify the workflow
log_wfl <- workflow() %>% 
           add_recipe(recipe) %>%
           add_model(log_md)

# Tune model
penalty_mixture_fit <- tune_grid(log_wfl,
                                 resamples = boots,
                                 grid = penalty_mixture_grid,
                                 control = control_grid(save_pred = TRUE),
                                 custom_metrics = metric_set(roc_auc, accuracy))

# Show best performing model
show_best(penalty_mixture_fit, 
          metric = "accuracy") %>%
  select(penalty, mixture, .metric, mean)

# Select best performing model
best <- select_best(penalty_mixture_fit,
                    metric = "accuracy") 
best

# Finalize workflow
log_wfl <- finalize_workflow(log_wfl, best)


# Results function
performanceResults <- function(workflow, training_data, test_data){
  
  fitted_workflow <- workflow %>%
    fit(training_data) 
  # This fits the workflow to the training data.
  
  p <-  fitted_workflow %>%
    predict(new_data = test_data) %>%
    bind_cols(test_data %>% select(Improved_Function)) 
  # This uses the fitted workflow to make predictions on the test data.
  
  # The next part specifies a function for extracting accuracy, sensitivity and 
  # specificity.
  stats <- function(data, indicies) {
    
    preds <- data[indicies,]
    
    res <- c(
      preds %>%
        accuracy(as.factor(Improved_Function), .pred_class) %>%
        select(.estimate) %>% 
        as.numeric(),
      
      preds %>%
        sensitivity(as.factor(Improved_Function), .pred_class) %>%
        select(.estimate) %>% 
        as.numeric(),
      
      preds %>%
        specificity(as.factor(Improved_Function), .pred_class) %>%
        select(.estimate) %>% 
        as.numeric()
    )
  }
  
  # Now we calculate the statistics, based on 1000 bootstraps of the results.
  set.seed(111)
  bootstrap_est <- boot::boot(p, stats, R = 1000)
  
  # Here we generate confidence intervals, based on our bootstraps.
  acc <- bootstrap_est$t0[1]
  lower_acc <- boot::boot.ci(bootstrap_est, index = 1, type ="perc")$perc[[4]]
  upper_acc <- boot::boot.ci(bootstrap_est, index = 1, type ="perc")$perc[[5]]
  
  sens <- bootstrap_est$t0[2]
  lower_sens <- boot::boot.ci(bootstrap_est, index = 2, type ="perc")$perc[[4]]
  upper_sens <- boot::boot.ci(bootstrap_est, index = 2, type ="perc")$perc[[5]]
  
  spec <- bootstrap_est$t0[3]
  lower_spec <- boot::boot.ci(bootstrap_est, index = 3, type ="perc")$perc[[4]]
  upper_spec <- boot::boot.ci(bootstrap_est, index = 3, type ="perc")$perc[[5]]
  
  # Next, we calculate the probability of each patient belonging to the "yes" or "no" 
  # class - this is used to calculate the area under the receiver operating 
  # characteristic curve (AUC).
  auc_p <- fitted_workflow %>%
    predict(new_data = test_data, type = "prob") %>%
    bind_cols(test_data %>% select(Improved_Function)) 
  
  # We specify a function for extracting the AUC.
  auc_boots <- function(auc_data, auc_indicies) {
    
    auc_preds <- auc_data[auc_indicies,]
    
    auc_preds %>%
      roc_auc(as.factor(Improved_Function), .pred_yes) %>%
      select(.estimate) %>% 
      as.numeric()
  }
  
  # And here we repeat the bootstrap process for AUC.
  set.seed(111)
  bootstrap_auc_est <- boot::boot(auc_p, auc_boots, R = 1000)
  auc <- bootstrap_auc_est$t0[1]
  lower_auc <- boot::boot.ci(bootstrap_auc_est, index = 1, type ="perc")$perc[[4]]
  upper_auc <- boot::boot.ci(bootstrap_auc_est, index = 1, type ="perc")$perc[[5]]
  
  # Next, we combine our statistics and confidence intervals into a table, for reading 
  # ease.
  colnames <- c("Statistic", "Value", "Lower 95% CI", "Upper 95% CI")
  row1 <- c("Accuracy", acc, lower_acc, upper_acc)
  row2 <- c("Sensitivity", sens, lower_sens, upper_sens)
  row3 <- c("Specificity", spec, lower_spec, upper_spec)
  row4 <- c("AUC", auc, lower_auc, upper_auc)
  
  results <- rbind(row1, row2, row3, row4)
  colnames(results) <- colnames
  results[,2:4] <- round(as.numeric(results[,2:4]), digits = 3)
  results <- as_tibble(results)
  
  # We also create a confusion matrix
  p$Improved_Function <- as.factor(p$Improved_Function)
  
  conf_matrix <- p %>%
    conf_mat(truth = Improved_Function, .pred_class)
  
  # and record ROC curve data.
  ROC_data <- auc_p %>%
    roc_curve(Improved_Function, .pred_yes)
  
  # Finally, we combine our results table and confusion matrix as a list.
  list <- list(results, conf_matrix, ROC_data)
  
  return(list)
  
}

lr_results <- performanceResults(log_wfl, classifier_train, classifier_test)

lr_results

# ROC curve (to plot later)
lr_auc <- lr_results[[3]] %>%
  mutate(Model = "Elastic Net")



## XG Boost
# Specify model
xgb_pen <- boost_tree(trees = 1000, 
                      tree_depth = tune(), 
                      min_n = tune(), 
                      loss_reduction = tune(), 
                      sample_size = tune(), 
                      mtry = tune(), 
                      learn_rate = tune()) %>% 
  set_engine("xgboost", importance = "impurity") %>%
  set_mode("classification")

# Create grid
set.seed(111)
xgb_grid <- grid_latin_hypercube(tree_depth(),
                                 min_n(),
                                 loss_reduction(), 
                                 sample_size = sample_prop(), 
                                 mtry(range = c(1, 60)), 
                                 learn_rate(),
                                 size = 10)

# Set workflow
xgb_wf <- workflow() %>%
          add_recipe(recipe) %>% 
          add_model(xgb_pen)

# Tune model
xgb_tg <- tune_grid(xgb_wf, 
                    resamples = boots,
                    grid = xgb_grid,
                    control = control_grid(save_pred = TRUE),
                    custom_metrics = metric_set(roc_auc, accuracy))

# Show best performing model
show_best(xgb_tg, metric = "accuracy")

# Select best
best <- select_best(xgb_tg,
                    metric = "accuracy")
best

# Finalize workflow
xgb_wf <- finalize_workflow(xgb_wf, best)

# Results
xgb_results <- performanceResults(xgb_wf, classifier_train, classifier_test)

xgb_results

xgb_auc <- xgb_results[[3]] %>%
  mutate(Model = "XG Boost")


# SHAP
set.seed(111)
xg_mod_fit <- xgb_wf %>%
  fit(classifier_train) %>%
  pull_workflow_fit() # Define fitted model

X <- prep(recipe, classifier_test) %>%
  juice() %>%
  select(-Improved_Function) %>%
  as.matrix() # Create a matrix of predictors to evaluate

shap_long_function <- shap.prep(xgb_model = xg_mod_fit$fit, 
                                X_train = X) # Get SHAP values

shap.plot.summary(shap_long_function) # Plot SHAP values

# Plot SHAP value against predictor value for top 4 predictors
shap_values <- shap.values(xg_mod_fit$fit, X)
features_ranked <- names(shap_values$mean_shap_score)[1:2]
fig_list <- lapply(features_ranked, shap.plot.dependence, 
                   data_long = shap_long_symptoms)
gridExtra::grid.arrange(grobs = fig_list, ncol = 2)


## SVM
# Specify model
svm_md <- svm_poly(cost = tune(), 
                   degree = tune()) %>%
          set_engine("kernlab") %>% 
          set_mode("classification") %>% 
          translate()

# Set workflow
svm_wfl <- workflow() %>% 
           add_recipe(recipe) %>%
           add_model(svm_md)

# Create grid
grid <- grid_regular(cost(), 
                     degree(),
                     levels = 10)

# Tune model
svm_grid <- tune_grid(svm_wfl, resamples = boots, grid = grid,
                      control = control_grid(save_pred = TRUE),
                      custom_metrics = metric_set(roc_auc, accuracy))

# Select best
best <- select_best(svm_grid,
                    metric = "accuracy")
best

# Finalize workflow
svm_wfl <- finalize_workflow(svm_wfl, best)

# Results
svm_results <- performanceResults(svm_wfl, classifier_train, classifier_test)

svm_results

svm_auc <- svm_results[[3]] %>%
  mutate(Model = "Support Vector Machine")

## ANN

# Specify model
nnet_md <- mlp(epochs = 10, 
               hidden_units = tune(),
               dropout = tune()) %>% 
  set_engine("keras") %>% 
  set_mode("classification") %>% 
  translate()

# Create grid
grid <- grid_regular(hidden_units(),
                     dropout(),
                     levels = 10)

# Set workflow
nnet_wfl <- workflow() %>% 
            add_recipe(recipe) %>%
            add_model(nnet_md)

# Tune model
nnet_grid <-  tune_grid(nnet_wfl,
                        resamples = boots,
                        grid = grid,
                        control = control_grid(save_pred = TRUE),
                        custom_metrics = metric_set(roc_auc, accuracy))

# Select best
best <- select_best(nnet_grid,
                    metric = "accuracy") # try changing this to accuracy/roc_auc
best

# Finalise workflow
nnet_wfl <- finalize_workflow(nnet_wfl, best)

# Results
nnet_results <- performanceResults(nnet_wfl, classifier_train, classifier_test)

nnet_results

nnet_auc <- nnet_results[[3]] %>%
  mutate(Model = "Neural Network")

### KNN
# Specify model
knn_md <- nearest_neighbor(neighbors = tune(),
                           weight_func = tune()) %>%
          set_engine("kknn") %>% 
          set_mode("classification") %>% 
          translate()

# Set workflow
knn_wfl <- workflow() %>% 
           add_recipe(recipe) %>%
           add_model(knn_md)

# Create grid
grid <- grid_regular(neighbors(), 
                     weight_func(),
                     levels = 10)

# Tune model
knn_grid <- tune_grid(knn_wfl, resamples = boots, grid = grid,
                      control = control_grid(save_pred = TRUE),
                      custom_metrics = metric_set(roc_auc, accuracy))

# Select best
best <- select_best(knn_grid,
                    metric = "accuracy")
best

# Finalise workflow
knn_wfl <- finalize_workflow(knn_wfl, best)

# Results
knn_results <- performanceResults(knn_wfl, classifier_train, classifier_test)

knn_results

knn_auc <- knn_results[[3]] %>%
  mutate(Model = "K Nearest Neighbours")

## Combine ROC curves
bind_rows(svm_auc, lr_auc, xgb_auc, nnet_auc, knn_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = Model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  theme_minimal() +
  xlab("1 - specificity") +
  ylab("Sensitivity") +
  scale_fill_brewer("Accent")

## CHAID with DASH 1 and EQ5D Mobility, based on full dataset

# Create CHAID dataset
CHAID_dataset <- data %>% select(Improved_Function, DASH1,EQ5D_Mobility)

placeholder <- rep(NA, 1045)
placeholder[which(CHAID_dataset$Improved_Function == "yes")] <- "Improved"
placeholder[which(CHAID_dataset$Improved_Function == "no")] <- "Not improved"
placeholder <- factor(placeholder,
                      levels = c("Not improved", "Improved"))
CHAID_dataset <- CHAID_dataset %>%
  mutate(Improvement = placeholder)

CHAID_dataset$Improved_Function <- NULL

CHAID_dataset$DASH1 <- as.factor(CHAID_dataset$DASH1)
CHAID_dataset$EQ5D_Mobility <- as.factor(CHAID_dataset$EQ5D_Mobility)

# Create CHAID tree
ctrl <- chaid_control(maxheight = 2)
tree <- chaid(Improvement ~., data = CHAID_dataset, control = ctrl)
tree
plot(tree, gp = gpar(lty = "solid", lwd = 1, fontsize = 10))


































