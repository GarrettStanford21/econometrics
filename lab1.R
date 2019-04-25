#### Part 1

x <- rnorm(50, 3, sqrt(7)) # Generates a random variable of 50 observations distributed N(3,7)
y <- 12 + 1.5*x + rnorm(50, 0 , 1)  # Data generating process

#### Part 2

X <- matrix(data = c(replicate(50, 1) , x) , ncol = 2)
Y <- matrix(data = y , ncol = 1)

b_m <- solve( t(X) %*% X ) %*% ( t(X) %*% Y ) 
t(b_m)

#### Part 3

## Function that tells R to solve the OLS estimator matrix algebra for any matrices a and b I put into it

regression <- function(a , b) {
  b_mf <- solve( t(a) %*% a ) %*% t(a) %*% b
  return(b_mf)
}

reg <- lm(y ~ x)
t(regression(X, Y))
summary(reg)$coefficients[ , 1]


#### Part 4

library(ggplot2)

regression_rep <- function(a) {
  X_i <- rnorm(a,3,sqrt(7)) # i for "initial"
  y_x <- 12 + 1.5*X_i + rnorm(a,0,1)
  X_d <- matrix(data = c(replicate(a,1) , X_i) , ncol = 2) # d for "Design"
  b_rep <- solve( t(X_d) %*% X_d ) %*% t(X_d) %*% y_x
  return(b_rep)
}

b_test <- replicate(10000 , regression_rep(50) )
bb <- t(matrix(data = b_test, nrow = 2 , ncol = 10000))

B <- as.data.frame(bb)
ggplot(data = B , aes(B$V1)) + geom_histogram(binwidth=0.06 , color='gray69') + scale_x_continuous(name = "Intercept Coefficient Estimates") + scale_y_continuous("Observations") + ggtitle("Distribution of 10,000 Sample Intercept Coefficient Estimates")
ggplot(data = B , aes(B$V2)) + geom_histogram(binwidth=0.015 , color='gray69') + scale_x_continuous(name = "Slope Coefficient Estimates") + scale_y_continuous("Observations") + ggtitle("Distribution of 10,000 Sample Slope Coefficient Estimates")
