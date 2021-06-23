# Logistic regression
# Emilio Lehoucq

# Loading packages --------------------------------------------------------

library(ggplot2)
library(car)

# Reading data ------------------------------------------------------------

data_train <- readRDS("processed_data/core_2_train.rds")

# Fitting models and extracting information -------------------------------

logit <- glm(like_listen_edm ~ age, data = data_train, family = "binomial")

logit
?glm

logit$coefficients
logit$residuals
logit_res <- logit$residuals
logit_fitted <- logit$fitted.values
logit_pred_probabilities <- predict(logit, type = "response")
cor(logit_fitted, logit_pred_probabilities)
logit_pred_classes <- ifelse(logit_pred_probabilities > 0.5, "(1) Yes", "(2) No")

summary(logit) # for every year, the log odds of liking listening to EDM increase by 0.03
confint(logit)
exp(coef(logit)) # for every year, the odds of liking listening to EDM increase by 1.03

logit_2 <- glm(like_listen_edm ~ age + race, data = data_train, family = "binomial")

summary(logit_2) # as compared to white people, the log odds of black people liking EDM are 0.37 times higher controlling for age
exp(coef(logit_2)) # as compared to white people, the odds of black people liking EDM are 1.45 higher controlling for age

# Diagnostics -------------------------------------------------------------
# far from exhaustive

# Linearity ---------------------------------------------------------------

ggplot(data_train) +
  aes(age, log(logit_pred_probabilities / (1-logit_pred_probabilities))) + # logit = log(p / 1-p)
  geom_point(size = 0.5, alpha = 0.5)

# Influential values ------------------------------------------------------

plot(logit, which = 4) # Cook's distance for influence (influence of removing one observation on the model)

# Multicollinearity -------------------------------------------------------

vif(logit_2)

# Comparing models --------------------------------------------------------

# AIC and BIC -------------------------------------------------------------

cbind(AIC(logit), AIC(logit_2)) # prefers logit_2
cbind(BIC(logit), BIC(logit_2)) # prefers logit (BIC penalizes more for additional parameters)

# Likelihood ration test --------------------------------------------------

anova(logit, logit_2, test ="Chisq") # we reject the null hypothesis that the reduced model contains the same information (prefers logit_2)
