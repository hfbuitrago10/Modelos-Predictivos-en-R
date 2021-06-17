# Boosted trees
# Emilio Lehoucq

# Loading packages --------------------------------------------------------

library(gbm)
library(caret)

# Reading data ------------------------------------------------------------

data_train <- readRDS("processed_data/core_2_train.rds")

# Setting parameters ------------------------------------------------------

set.seed(607) 

train_control <- trainControl(method = "cv",
                              number = 5)

gbm_grid <-  expand.grid(n.trees = (1:30)*50, 
                        shrinkage = seq(0.001, 0.1, length.out = 10),
                        n.minobsinnode = 20, # increases stability of trees
                        interaction.depth = 1) 
# For other parameters: https://topepo.github.io/caret/model-training-and-tuning.html#basic-parameter-tuning

# Classical ---------------------------------------------------------------

classical_gbm <- train(like_listen_classical ~ ., data = data_train, 
                 method = "gbm", 
                 trControl = train_control,
                 verbose = FALSE,
                 tuneGrid = gbm_grid) # best n.trees = 1250, shrinkage = 0.089.
# after shrinkage = 0.089 and n.trees = 200 it stabilizes.
# definitely more shrinkage is better, I'd say 800 trees are fine.

ggplot(classical_gbm)  


# Classical rock and oldies -----------------------------------------------

rock_gbm <- train(like_listen_classic_rock_oldies ~ ., data = data_train, 
                       method = "gbm", 
                       trControl = train_control,
                       verbose = FALSE,
                       tuneGrid = gbm_grid) # best n.trees = 300, shrinkage = 0.056
# after shrinkage = 0.012 and n.trees = 800 it's actually more stable.
# visually, shrinkage = 0.012 is also better with more trees. I'd say after 800.

ggplot(rock_gbm)  

# Country -----------------------------------------------------------------

country_gbm <- train(like_listen_country ~ ., data = data_train, 
                       method = "gbm", 
                       trControl = train_control,
                       verbose = FALSE,
                       tuneGrid = gbm_grid) # best n.trees = 1100, shrinkage = 0.089
# in this case that seems fine.

ggplot(country_gbm)  
