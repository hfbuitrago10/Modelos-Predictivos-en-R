# Regularized models

# Loading libraries -------------------------------------------------------

library(glmnet)
library(glmnetUtils)

# Reading data ------------------------------------------------------------

data_train <- readRDS("processed_data/core_2_train.rds")

# Setting parameters ------------------------------------------------------

set.seed(607)

lambda_grid <- 10^seq(-2, 10, length = 200)

# Classical ---------------------------------------------------------------

classical_ridge_cv <- cv.glmnet(formula = like_listen_classical ~ ., 
            data = data_train, 
            alpha = 0, 
            nfolds = 5,
            lambda = lambda_grid,
            family = "binomial"
  )

plot(classical_ridge_cv)

classical_lasso_cv <- cv.glmnet(
    formula = like_listen_classical ~ ., 
    data = data_train, 
    alpha = 1, 
    nfolds = 5,
    family = "binomial"
  )

plot(classical_lasso_cv)

classical_ridge_lambda_min <- classical_ridge_cv$lambda.min # 0.03036771
classical_lasso_lambda_min <- classical_lasso_cv$lambda.min # 0.003813685

classical_ridge <- glmnet(like_listen_classical ~ ., data = data_train, alpha = 0, 
                               lambda = classical_ridge_lambda_min, family = "binomial")

classical_lasso <- glmnet(like_listen_classical ~ ., data = data_train, alpha = 1, 
                               lambda = classical_lasso_lambda_min, family = "binomial")

# Classical rock and oldies -----------------------------------------------

rock_ridge_cv <- cv.glmnet(formula = like_listen_classic_rock_oldies ~ ., 
            data = data_train, 
            alpha = 0, 
            nfolds = 5,
            lambda = lambda_grid,
            family = "binomial"
  )

plot(rock_ridge_cv)

rock_lasso_cv <- cv.glmnet(
    formula = like_listen_classic_rock_oldies ~ ., 
    data = data_train, 
    alpha = 1, 
    nfolds = 5,
    family = "binomial"
  )

plot(rock_lasso_cv)

rock_ridge_lambda_min <- rock_ridge_cv$lambda.min # 0.02643081
rock_lasso_lambda_min <- rock_lasso_cv$lambda.min # 0.003850818

rock_ridge <- glmnet(like_listen_classic_rock_oldies ~ ., data = data_train, alpha = 0, 
                          lambda = rock_ridge_lambda_min, family = "binomial")

rock_lasso <- glmnet(like_listen_classic_rock_oldies ~ ., data = data_train, alpha = 1, 
                          lambda = rock_lasso_lambda_min, family = "binomial")

# Country -----------------------------------------------------------------

country_ridge_cv <- cv.glmnet(formula = like_listen_country ~ ., 
            data = data_train, 
            alpha = 0, 
            nfolds = 5,
            lambda = lambda_grid,
            family = "binomial"
  )

plot(country_ridge_cv)

country_lasso_cv <- cv.glmnet(
    formula = like_listen_country ~ ., 
    data = data_train, 
    alpha = 1, 
    nfolds = 5,
    family = "binomial"
  )

plot(country_lasso_cv)

country_ridge_lambda_min <- country_ridge_cv$lambda.min # 0.01
country_lasso_lambda_min <- country_lasso_cv$lambda.min # 0.003135717

country_ridge <- glmnet(like_listen_country ~ ., data = data_train, alpha = 0, 
                          lambda = country_ridge_lambda_min, family = "binomial")

country_lasso <- glmnet(like_listen_country ~ ., data = data_train, alpha = 1, 
                          lambda = country_lasso_lambda_min, family = "binomial")
