# BART

# Setting parameters ------------------------------------------------------

options(java.parameters = "-Xmx54g")

# Loading packages --------------------------------------------------------

library(bartMachine)

# Reading data ------------------------------------------------------------

data_train <- readRDS("processed_data/core_2_train.rds")

# Setting parameters ------------------------------------------------------

set_bart_machine_num_cores(4)

set.seed(2620)

# Training on full data ---------------------------------------------------

# Preparing data ----------------------------------------------------------

classical_y <- data_train$like_listen_classical

classical_x <- data_train

classical_x$like_listen_classical <- NULL

rock_y <- data_train$like_listen_classic_rock_oldies

rock_x <- data_train

rock_x$like_listen_classic_rock_oldies <- NULL

country_y <- data_train$like_listen_country

country_x <- data_train

country_x$like_listen_country <- NULL

# Tuning through 5-fold CV ---------------------------------------------

classical_bart_cv <- bartMachineCV(data.frame(classical_x), classical_y) # best k=3 m=50

rock_bart_cv <- bartMachineCV(data.frame(rock_x), rock_y) # best k=5 m=50

country_bart_cv <- bartMachineCV(data.frame(country_x), country_y) # best k=2 m=50

# Classical --------------------------------------------------------------

classical_bart <- bartMachine(data.frame(classical_x), classical_y, k = 3, num_trees = 50)

plot_convergence_diagnostics(classical_bart) # looks fine

# Rock and oldies ---------------------------------------------------------

rock_bart <- bartMachine(data.frame(rock_x), rock_y, k = 5, num_trees = 50)

plot_convergence_diagnostics(rock_bart) # looks fine

# Country -----------------------------------------------------------------

country_bart <- bartMachine(data.frame(country_x), country_y, k = 2, num_trees = 50)

plot_convergence_diagnostics(country_bart) # looks fine
