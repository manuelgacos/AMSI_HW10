library(MASS)
library(glmnet)

setwd('/home/noble_mannu/Documents/PhD/First/STAT_2131_Applied_Statistical_Methods_I/HW10')
set.seed(489)

Data <- read.table('Fat.txt', header = TRUE)
# Remove every tenth observation
tenths <- seq(from = 10, to = 252, by =10)
Data_1 <- Data[-tenths, ]

# Create vector of lambdas
lambda_seq <- 10^seq(3, -3, by = -.1) # 0.5011872
# lambda_seq <- 10^seq(3, -3, by = -.01) # 0.5011872
# lambda_seq <- 10^seq(3, -3, by = -.001) # 0.5861382 0.519996
# lambda_seq <- 10^seq(3, -3, by = -.005) # 0.5688529
# lambda_seq <- 10^seq(3, -3, by = -.0005) # 0.5364139
# lambda_seq <- 10^seq(3, -3, by = -.0001) # 0.5514422

intercept <- rep(1, length(Data$siri))
X <- cbind(intercept, data.matrix(subset(Data, select = -c(siri))))
Y <- Data$siri

fit <- glmnet(X, Y, alpha = 0, lambda  = lambda_seq)

ridge_cv <- cv.glmnet(X, Y, alpha = 0, lambda = lambda_seq)

plot(ridge_cv)

best_lambda <- ridge_cv$lambda.min

linMod <- lm(siri ~ ., data = Data)
summary(linMod)

fit.ridge <- lm.ridge(siri~ ., data=Data, lambda = lambda_seq)  #fit.ridge$GCV contains the generalized cross validation results for each value of lambda
all.coefficients <- data.matrix(coef(fit.ridge))  #A matrix containing the fitted coefficients for each value of lambda (this includes the intercept)
Y.hat.lambda1 <- as.vector(X%*%all.coefficients[1,])   #The fitted values for lambda = seq_of_lambda[1].

y <- Y.hat.lambda1 - Data$siri
SSE <- y%*%y
MSE <- SSE/(length(Data$siri)-16)

MSE_vect <- rep(0, length(lambda_seq))

for (i in 1:length(lambda_seq)){
  Y.hat.lambda <- as.vector(X%*%all.coefficients[i,])
  y_test <- Y.hat.lambda - Data$siri
  SSE_test <- y_test%*%y_test
  MSE_test <- SSE_test/(length(Data$siri)-16)
  MSE_vect[i] <- MSE_test
}

min <- which.min(MSE_vect)
lambda_seq[min]
