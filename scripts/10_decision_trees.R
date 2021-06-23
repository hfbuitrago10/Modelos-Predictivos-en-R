# Decision trees
# Emilio Lehoucq

# Loading packages --------------------------------------------------------

library(tree)

# Reading data ------------------------------------------------------------

data_train <- readRDS("processed_data/core_2_train.rds")
data_test <- readRDS("processed_data/core_2_test.rds")

# Setting parameters ------------------------------------------------------

set.seed(607)

# Fitting a classification tree and extracting information ----------------

cart <- tree(data_train$like_listen_edm ~ . , data = data_train)

cart
?tree

summary(cart)

# Visualizing a tree ------------------------------------------------------

plot(cart)
text(cart, pretty = 0)

# Obtaining predictions ---------------------------------------------------

cart_pred_class <- predict(cart, data_test, type = "class")
table(cart_pred_class, data_test$like_listen_edm)
(131+374)/3699 # 0.14 misclassification rate 

# Pruning the tree --------------------------------------------------------

cart_cv <- cv.tree(cart, FUN = prune.misclass)
cart_cv

par(mfrow = c(1, 2))
plot(cart_cv$size ,cart_cv$dev ,type="b") # best three terminal nodes
plot(cart_cv$k ,cart_cv$dev ,type="b") # best 0 alpha

cart_prune <- prune.misclass(cart, best = 3)

# Assessing the pruned tree -----------------------------------------------

plot(cart_prune)
text(cart_prune, pretty = 0)

cart_prune_pred_class <- predict(cart_prune, data_test, type = "class")
table(cart_prune_pred_class, data_test$like_listen_edm)
(131+374)/3699 # 0.14 misclassification rate 
