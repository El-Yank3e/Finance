install.packages(c("forecast",
                   "leaps"))

library(forecast)
library(leaps)

# Table 6.3
toyota.df <- read.csv("ToyotaCorolla.csv")[1:1000,]
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
# List selected variables
names(toyota.df)[selected.var]
# Partition the dataset into training and validation sets
set.seed(1)
train.index <- sample(c(1:1000), 600)
train.df <- toyota.df[train.index, selected.var]
valid.df <- toyota.df[-train.index, selected.var]
# Multiple linear regression model estimation
toyota.lm <- lm(Price ~ ., data = train.df)
options(scipen = 999)
summary(toyota.lm)

# Table 6.4
# Prediction
valid.pred <- predict(toyota.lm, valid.df)
# Residuals (errors = actual value - prediction)
options(scipen = 999, digits = 0)
valid.res <- valid.df$Price - valid.pred
data.frame("Predicted" = valid.pred, "Actual" = valid.df$Price, "Residual" = valid.res)
# Compute accuracy measures
options(scipen = 999, digits = 3)
accuracy(valid.pred, valid.df$Price)

# Figure 6.1
# Draw histogram of residuals
hist(valid.res, breaks = 25, xlab = "Residuals", main = "")

# Table 6.5
exhaustive_search <- regsubsets(Price ~ ., data = train.df, nbest = 1,
                     nvmax = dim(train.df)[2], method = "exhaustive")
sum_exhaustive <- summary(exhaustive_search)
# Print selected models given the number of predictors
sum_exhaustive$which
# Print the R-squared of selected models
sum_exhaustive$rsq
# Print the adjusted R-squared of selected models
sum_exhaustive$adjr2
# Print the Mallow's Cp of selected models
sum_exhaustive$cp
# Print BIC of selected models
sum_exhaustive$bic

# Table 6.6
# Backward elimination
toyota.lm.all <- lm(Price ~ ., data = train.df)
toyota.lm.backward <- step(toyota.lm.all, direction = "backward")
summary(toyota.lm)
summary(toyota.lm.backward)
valid.pred.backward <- predict(toyota.lm.backward, valid.df)
accuracy(valid.pred.backward, valid.df$Price)

# Table 6.7
# Forward selection
toyota.lm.null <- lm(Price ~ 1, data = train.df)
toyota.lm.all <- lm(Price ~ ., data = train.df)
toyota.lm.forward <- step(toyota.lm.null,
                          scope = list(lower = toyota.lm.null, upper = toyota.lm.all),
                          direction = "forward")
summary(toyota.lm)
summary(toyota.lm.forward)
valid.pred.forward <- predict(toyota.lm.forward, valid.df)
accuracy(valid.pred.forward, valid.df$Price)

# Table 6.8
toyota.lm.null <- lm(Price ~ 1, data = train.df)
toyota.lm.all <- lm(Price ~ ., data = train.df)
toyota.lm.stepwise <- step(toyota.lm.null,
                           scope = list(lower = toyota.lm.null, upper = toyota.lm.all),
                           direction = "both")
summary(toyota.lm)
summary(toyota.lm.stepwise)
valid.pred.stepwise <- predict(toyota.lm.stepwise, valid.df)
accuracy(valid.pred.stepwise, valid.df$Price)
