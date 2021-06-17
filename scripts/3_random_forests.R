# Random forests

# Loading packages --------------------------------------------------------

library(randomForest)
library(caret) # masks 'margin' from randomForest

# Reading data ------------------------------------------------------------

data_train <- readRDS("processed_data/core_2_train.rds")

# Setting parameters ------------------------------------------------------

set.seed(607)

train_control <- trainControl(method = "cv",
                              number = 5,
                              search = "grid")

tune_grid <- expand.grid(.mtry = c(1:40))
# For other parameters: https://www.guru99.com/r-random-forest-tutorial.html#2

# Classical ---------------------------------------------------------------

classical_rf_mtry <- train(like_listen_classical ~ .,
                           data = data_train,
                           method = "rf",
                           metric = "Accuracy",
                           tuneGrid = tune_grid,
                           trControl = train_control,
                           importance = TRUE,
                           nodesize = 10) # makes computing faster and not super important to tune

print(classical_rf_mtry) # 28 is the best. after 15 it's pretty stable.

# Classical rock and oldies -----------------------------------------------

rock_rf_mtry <- train(like_listen_classic_rock_oldies ~ .,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tune_grid,
                      trControl = train_control,
                      nodesize = 10) # makes computing faster and not super important to tune

print(rock_rf_mtry) # best is 34. stable after 28.

# Country -----------------------------------------------------------------

country_rf_mtry <- train(like_listen_country ~ .,
                         data = data_train,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = tune_grid,
                         trControl = train_control,
                         nodesize = 10) # makes computing faster and not super important to tune

print(country_rf_mtry) # best is 34. stable after 30.
