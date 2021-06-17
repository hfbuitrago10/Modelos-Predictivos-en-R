# Model selection
# Emilio Lehoucq

# Setting parameters ------------------------------------------------------

options(java.parameters = "-Xmx54g")

# Loading packages --------------------------------------------------------

library(tidyverse)
library(glmnet) # masks 'expand', 'pack', and 'unpack' from tidyr
library(glmnetUtils)
library(randomForest) # masks 'combine' from 'dplyr' and 'margin' from ggplot2
library(caret) # masks 'lift' from purrr
library(gbm)
library(bartMachine)

# Reading data ------------------------------------------------------------

data_train <- readRDS("processed_data/core_2_train.rds")
data_test <- readRDS("processed_data/core_2_test.rds")

# Setting parameters ------------------------------------------------------

set.seed(607) 

fit_control <- trainControl(method = "none", classProbs = TRUE)

# Helper function ---------------------------------------------------------

#' @name misclass_rate
misclass_rate <- function(outcome, model){
  
  p_hat <- predict(model, newdata = data_test, type = "response")
  class_hat <- if_else(p_hat > 0.5, "(2) No", "(1) Yes")
  misclass_rate <- mean(class_hat != outcome)
  misclass_rate
  
}

#' @name misclass_rate_rf
misclass_rate_rf <- function(outcome, model){
  
  class_hat <- predict(model, newdata = data_test, type = "response")
  misclass_rate <- mean(class_hat != outcome)
  misclass_rate
  
}

#' @name misclass_rate_bt
misclass_rate_bt <- function(outcome, model){
  
  p_hat <- predict(model, newdata = data_test, type = "prob")
  class_hat <- if_else(p_hat[1] > 0.5, "(1) Yes", "(2) No")
  misclass_rate <- mean(class_hat != outcome)
  misclass_rate
  
}

#' @name misclass_rate_bart
misclass_rate_bart <- function(outcome, model, x_test, y_test){
  
  predictions <- bart_predict_for_test_data(model, x_test, y_test)
  class_hat <- predictions$y_hat
  misclass_rate <- mean(class_hat != y_test)
  misclass_rate
  
}

# Logistic regressions ----------------------------------------------------

classical_logit <- glm(like_listen_classical ~ ., data = data_train, family = binomial(link = "logit")) # didn't converge

classical_logit_mr <- misclass_rate(data_test$like_listen_classical, classical_logit)

rock_logit <- glm(like_listen_classic_rock_oldies ~ ., data = data_train, family = binomial(link = "logit"))

rock_logit_mr <- misclass_rate(data_test$like_listen_classic_rock_oldies, rock_logit)

country_logit <- glm(like_listen_country ~ ., data = data_train, family = binomial(link = "logit"))

country_logit_mr <- misclass_rate(data_test$like_listen_country, country_logit)

# Regularized regressions -------------------------------------------------

classical_ridge <- glmnet(like_listen_classical ~ ., data = data_train, alpha = 0, lambda = 0.03036771, family = "binomial")

classical_ridge_mr <- misclass_rate(data_test$like_listen_classical, classical_ridge)

classical_lasso <- glmnet(like_listen_classical ~ ., data = data_train, alpha = 1, lambda = 0.003813685, family = "binomial")

classical_lasso_mr <- misclass_rate(data_test$like_listen_classical, classical_lasso)

rock_ridge <- glmnet(like_listen_classic_rock_oldies ~ ., data = data_train, alpha = 0, lambda = 0.02643081, family = "binomial")

rock_ridge_mr <- misclass_rate(data_test$like_listen_classic_rock_oldies, rock_ridge)

rock_lasso <- glmnet(like_listen_classic_rock_oldies ~ ., data = data_train, alpha = 1, lambda = 0.003850818, family = "binomial")

rock_lasso_mr <- misclass_rate(data_test$like_listen_classic_rock_oldies, rock_lasso)

country_ridge <- glmnet(like_listen_country ~ ., data = data_train, alpha = 0, lambda = 0.01, family = "binomial")

country_ridge_mr <- misclass_rate(data_test$like_listen_country, country_ridge)

country_lasso <- glmnet(like_listen_country ~ ., data = data_train, alpha = 1, lambda = 0.003135717, family = "binomial")

country_lasso_mr <- misclass_rate(data_test$like_listen_country, country_lasso)

# Random forests ----------------------------------------------------------

classical_rf <- randomForest(like_listen_classical ~ ., data = data_train, nodesize = 10, mtry = 15)

classical_rf_mr <- misclass_rate_rf(data_test$like_listen_classical, classical_rf)

rock_rf <- randomForest(like_listen_classic_rock_oldies ~ ., data = data_train, nodesize = 10, mtry = 28)

rock_rf_mr <- misclass_rate_rf(data_test$like_listen_classic_rock_oldies, rock_rf)

country_rf <- randomForest(like_listen_country ~ ., data = data_train, nodesize = 10, mtry = 30)

country_rf_mr <- misclass_rate_rf(data_test$like_listen_country, country_rf)

# Boosted trees -----------------------------------------------------------

classical_bt <- train(like_listen_classical ~ ., data = data_train %>% mutate(like_listen_classical = factor(like_listen_classical, 
                                                                                             labels = make.names(levels(like_listen_classical)))), 
                 method = "gbm", 
                 trControl = fit_control, 
                 verbose = FALSE, 
                 tuneGrid = data.frame(interaction.depth = 1,
                                       n.trees = 800,
                                       shrinkage = 0.089,
                                       n.minobsinnode = 10),
                 metric = "Accuracy")

classical_bt_mr <- misclass_rate_bt(data_test$like_listen_classical, classical_bt)

rock_bt <- train(like_listen_classic_rock_oldies ~ ., data = data_train %>% mutate(like_listen_classic_rock_oldies = factor(like_listen_classic_rock_oldies, 
                                                                                                             labels = make.names(levels(like_listen_classic_rock_oldies)))), 
                      method = "gbm", 
                      trControl = fit_control, 
                      verbose = FALSE, 
                      tuneGrid = data.frame(interaction.depth = 1,
                                            n.trees = 800,
                                            shrinkage = 0.012,
                                            n.minobsinnode = 10),
                      metric = "Accuracy")

rock_bt_mr <- misclass_rate_bt(data_test$like_listen_classic_rock_oldies, rock_bt)

country_bt <- train(like_listen_country ~ ., data = data_train %>% mutate(like_listen_country = factor(like_listen_country, 
                                                                                                             labels = make.names(levels(like_listen_country)))), 
                      method = "gbm", 
                      trControl = fit_control, 
                      verbose = FALSE, 
                      tuneGrid = data.frame(interaction.depth = 1,
                                            n.trees = 1100,
                                            shrinkage = 0.089,
                                            n.minobsinnode = 10),
                      metric = "Accuracy")

country_bt_mr <- misclass_rate_bt(data_test$like_listen_country, country_bt)

# BART --------------------------------------------------------------------

classical_y <- data_train$like_listen_classical

classical_x <- data_train

classical_x$like_listen_classical <- NULL

classical_bart <- bartMachine(data.frame(classical_x), classical_y, k = 3, num_trees = 50)

classical_y_test <- data_test$like_listen_classical

classical_x_test <- data_test

classical_x_test$like_listen_classical <- NULL

classical_bart_mr <- misclass_rate_bart(data_test$like_listen_classical, classical_bart, classical_x_test, classical_y_test)

rock_y <- data_train$like_listen_classic_rock_oldies

rock_x <- data_train

rock_x$like_listen_classic_rock_oldies <- NULL

rock_bart <- bartMachine(data.frame(rock_x), rock_y, k = 5, num_trees = 50)

rock_y_test <- data_test$like_listen_classic_rock_oldies

rock_x_test <- data_test

rock_x_test$like_listen_classic_rock_oldies <- NULL

rock_bart_mr <- misclass_rate_bart(data_test$like_listen_classic_rock_oldies, rock_bart, rock_x_test, rock_y_test)

country_y <- data_train$like_listen_country

country_x <- data_train

country_x$like_listen_country <- NULL

country_bart <- bartMachine(data.frame(country_x), country_y, k = 2, num_trees = 50)

country_y_test <- data_test$like_listen_country

country_x_test <- data_test

country_x_test$like_listen_country <- NULL

country_bart_mr <- misclass_rate_bart(data_test$like_listen_country, country_bart, country_x_test, country_y_test)

# Visual model comparison -------------------------------------------------

misclass_rates <- tibble(models = rep(c("Logistic", "Ridge", "Lasso", "Random forest", "Boosted trees", "BART"), times=3),
                   response = c(rep("Classical", times=6), rep("Rock and oldies", times=6), rep("Country", times=6)),
                   misclass_rate = c(classical_logit_mr, classical_ridge_mr, classical_lasso_mr, classical_rf_mr, classical_bt_mr, classical_bart_mr,
                                     rock_logit_mr, rock_ridge_mr, rock_lasso_mr, rock_rf_mr, rock_bt_mr, rock_bart_mr,
                                     country_logit_mr, country_ridge_mr, country_lasso_mr, country_rf_mr, country_bt_mr, country_bart_mr))

saveRDS(misclass_rates, file = "misclass_rates.rds")

ggplot(misclass_rates) +
  aes(x=misclass_rate, y=models) +
  geom_point() +
  facet_wrap(~response) +
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Test misclassification rates across models and responses") +
  xlab("Misclassification rate") +
  ylab("Model") # 678 x 410

table(data_test$like_listen_classical)/nrow(data_test)
table(data_test$like_listen_classic_rock_oldies)/nrow(data_test)
table(data_test$like_listen_country)/nrow(data_test)
