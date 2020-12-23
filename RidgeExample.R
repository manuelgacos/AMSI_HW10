library(MASS)
#Data: data.frame of interest, where Data$y is response and Data$x1, Data$x2 are predictors. There is no need to standardize or mean center your data using the following code.
#seq_of_lambda: a vector of possible values of lambda. They must be >= 0, where lambda=0 corresponds to the OLS estimate.
#X: The design matrix, which includes the intercept. This does NOT need to be standardized or mean centered using the following code.

fit.ridge <- lm.ridge(y~x1 + x2, data=Data, lambda = seq_of_lambda)  #fit.ridge$GCV contains the generalized cross validation results for each value of lambda
all.coefficients <- coef(fit.ridge)  #A matrix containing the fitted coefficients for each value of lambda (this includes the intercept)
Y.hat.lambda1 <- X%*%all.coefficients[1,]   #The fitted values for lambda = seq_of_lambda[1].