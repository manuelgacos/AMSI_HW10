library(MASS)

setwd('/home/noble_mannu/Documents/PhD/First/STAT_2131_Applied_Statistical_Methods_I/HW10')

Data <- read.table('Fat.txt', header = TRUE)
# Remove every tenth observation
tenths <- seq(from = 10, to = 252, by =10)
# Create data frame removing the tenths observations
Data_1 <- Data[-tenths, ]
# Create data frame removing with only the tenths observations
Data_2 <- Data[tenths, ]

# Create X matrix including intercept for the training data
intercept <- rep(1, length(Data_1$siri))
X <- cbind(intercept, data.matrix(subset(Data_1, select = -c(siri))))
Y <- Data_1$siri

# Create the linear model
linMod <- lm(siri ~ ., data = Data_1)
# Display the summary of the linear model
summary(linMod)

# Ridge regression

# Create vector of lambdas
lambda_seq <- 10^seq(-1, -4, by = -.05)

fit.ridge <- lm.ridge(siri~ ., data=Data_1, lambda = lambda_seq)  #fit.ridge$GCV contains the generalized cross validation results for each value of lambda
all.coefficients <- data.matrix(coef(fit.ridge))  #A matrix containing the fitted coefficients for each value of lambda (this includes the intercept)

# Here I plot GCV as a function of lambda
plot( lambda_seq, fit.ridge$GCV, xlab = 'Lambda', ylab = 'GCV', 
      main = 'GCV as a function of lambda')

# The next step will be choosing the best lambda
index <- which.min(fit.ridge$GCV)
min_gcv <- fit.ridge$GCV[index]
best_lambda <- as.numeric(names(fit.ridge$GCV[index]))

# Next we compute the fitted values for the best lambda using ridge regression
Y.hat.lambda <- as.vector(X%*%all.coefficients[index,])   #The fitted values for lambda = seq_of_lambda[1].

# Compute the training error for the linear model
y.lin <- Data_1$siri - linMod$fitted.values
n <- length(Data_1$siri)
err.lin <- 1/n * y.lin%*%y.lin
# Compute the training error for the ridge regression model
y.ridge <- Data_1$siri - Y.hat.lambda
err.ridge <- 1/n * y.ridge%*%y.ridge

# We get that the error from the linear model is smaller than the error using ridge
err.lin < err.ridge

# Predicting the held out data
# Create X matrix including intercept for the test data
intercept.test <- rep(1, length(Data_2$siri))
X.test <- cbind(intercept.test, data.matrix(subset(Data_2, select = -c(siri))))
Y.test <- Data_2$siri
n.test <- length(Data_2$siri)

# Using the linear model to predict the held out data
lin.coef <- coef(linMod)
fitted.Y.linear <- as.vector(X.test%*%lin.coef)
y.lin.test <- Data_2$siri - fitted.Y.linear
loss.lin <- 1/n.test * y.lin.test%*%y.lin.test

# Using the ridge model to predict the held out data
fitted.Y.ridge <- as.vector(X.test%*%all.coefficients[index,])
y.ridge.test <- Data_2$siri - fitted.Y.ridge
loss.ridge <- 1/n.test * y.ridge.test%*%y.ridge.test

# Now ridge does a better job at predicting the data
loss.ridge < loss.lin