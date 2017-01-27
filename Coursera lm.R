# Quiz 2
# 1
# Give a P-value for the two sided hypothesis test of whether beta1 from a 
# linear regression model is 0 or not.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
summary(fit)
# 0.053

# 2
# Consider the previous problem, 
# give the estimate of the residual standard deviation.
# 0.223

# 3
# In the mtcars data set, fit a linear regression model of weight (predictor) 
# on mpg (outcome). Get a 95% confidence interval for the expected mpg 
# at the average weight. What is the lower endpoint?
fit <- lm(mpg ~ wt, mtcars)
# confint(fit)
mean_weight <- data.frame(wt = c(mean(mtcars$wt)))
predict(fit, mean_weight, interval='confidence')
# 18.99098


# 4
# Refer to the previous question. Read the help file for mtcars. 
# What is the weight coefficient interpreted as?
head(mtcars)
# The estimated expected change in mpg per 1,000 lb increase in weight.

# 5
# Consider again the mtcars data set and a linear regression model 
# with mpg as predicted by weight (1,000 lbs). 
# A new car is coming weighing 3000 pounds. 
# Construct a 95% prediction interval for its mpg. What is the upper endpoint?
fit <- lm(mpg ~ wt, mtcars)
summary(fit)
fit$coefficients
37.285126 + 3 * -5.344472
new_weight <- data.frame(wt = 3)
predict(fit, new_weight, interval='prediction')
# 27.57355


# 6
# Consider again the mtcars data set and a linear regression model with mpg 
# as predicted by weight (in 1,000 lbs). A “short” ton is defined as 2,000 lbs. 
# Construct a 95% confidence interval for the expected change in mpg 
# per 1 short ton increase in weight. Give the lower endpoint.
fit <- lm(mpg ~ wt, mtcars)
confint(fit)*2
# -6.486308
# -12.973


# 7
# If my X from a linear regression is measured in centimeters and 
# I convert it to meters what would happen to the slope coefficient?
fit <- lm(mpg ~ I(wt/100), mtcars)
summary(fit)
# It would get multiplied by 100.

# 8
# I have an outcome, Y, and a predictor, X and fit a linear regression model 
# with Y=beta0+beta1X+e to obtain beta0 and beta1. 
# What would be the consequence to the subsequent slope and intercept 
# if I were to refit the model with a new regressor, X+c for some constant, c?
# The new intercept would be beta0-c*beta1


# 9
# Refer back to the mtcars data set with mpg as an outcome and weight (wt) 
# as the predictor. About what is the ratio of the the sum of the squared 
# errors, sum(ni)=1(Yi???Y^i)2 when comparing a model with just an 
# intercept (denominator) to the model with the intercept and slope (numerator)?
fit <- lm(mpg ~ wt, mtcars)
summary(fit)
fit1 <- lm(mpg ~ 1, mtcars)
summary(fit1)

sum(fit$residuals^2) / sum(fit1$residuals^2)
1 - summary(fit)$r.squared
# 0.25

# 10
# Do the residuals always have to sum to 0 in linear regression?
# If an intercept is included, then they will sum to 0.


################################################################################
# Quiz 3

# 1
# Consider the mtcars data set. Fit a model with mpg as the outcome that 
# includes number of cylinders as a factor variable and weight as confounder. 
# Give the adjusted estimate for the expected change in mpg 
# comparing 8 cylinders to 4.
head(mtcars)
summary(lm(mpg ~ wt + as.factor(cyl), mtcars))
# -6.0709

# 2
summary(lm(mpg ~ wt + as.factor(cyl), mtcars))
summary(lm(mpg ~ as.factor(cyl), mtcars))
33.9908-6.0709
# Holding weight constant, cylinder appears to have less of an impact on mpg 
# than if weight is disregarded.

# 3
fit1 <- lm(mpg ~ wt + as.factor(cyl), mtcars)
fit2 <- lm(mpg ~ wt * as.factor(cyl), mtcars)
fit2 <- update(fit1, mpg ~ wt * as.factor(cyl))
# anova(fit1, fit2)
summary(fit1)
summary(fit2)
# The P-value is larger than 0.05. 
# So, according to our criterion, we would fail to reject, 
# which suggests that the interaction terms may not be necessary.

# 4
summary(lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars))
# The estimated expected change in MPG per half ton increase in weight for 
# for a specific number of cylinders (4, 6, 8).

# The estimated expected change in MPG per one ton increase in weight for 
# a specific number of cylinders (4, 6, 8).

# 5
# Give the hat diagonal for the most influential point
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

fit <- lm(y ~ x)
summary(fit)
fit$fitted.values

influence.measures(fit)
# 0.9946

# 6
# Give the slope dfbeta for the point with the highest hat value.
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
influence.measures(fit)
# -1.34*10^2 = -134

# 7
# It is possible for the coefficient to reverse sign after adjustment. 
# For example, it can be strongly significant and positive before adjustment 
# and strongly significant and negative after adjustment.




























# Workspace
library(ggplot2)
fit <- lm(mpg ~ wt, mtcars)
fit <- lm(mpg ~ I(wt-mean(wt)), mtcars)
fit <- lm(I(mpg-mean(mpg)) ~ I(wt-mean(wt))-1, mtcars)
fit <- lm(mpg ~ I(wt-mean(wt))-1, mtcars)
fit <- lm(mpg ~ wt, mtcars)
summary(fit)

y <- mtcars$mpg
x <- mtcars$wt
yc <- y - mean(y)
xc <- x - mean(x)

beta1 <- sum((x-mean(x))*(y - mean(y))) / sum((x - mean(x))^2); beta1
beta1 <- cov(x, y) / var(x); beta1
beta1 <- cor(x, y) * sd(y) / sd(x); beta1
beta0 <- mean(y) - beta1 * mean(x)
beta0

coef(fit)

ggplot(mtcars, aes(x = wt, y = mpg)) + 
        geom_point(aes(color = as.factor(cyl))) +
        geom_smooth(method = "lm", formula = y ~ x + I(x^2))

ggplot(data.frame(xc, yc), aes(x = xc, y = yc)) + 
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x)


fit_full <- lm(mpg ~., mtcars)
fit_null <- lm(mpg ~1, mtcars)
scope <- list(lower = fit_null, upper = fit_full)
library(psych)
fit <- ?lm()
data(mtcars); fit <- lm(mpg ~ ., data = mtcars)
# step-wise search using BIC
a <- step(fit, k = log(nrow(mtcars)))
b <- step(fit, direction = 'backward')
summary(a)
summary(b)

ideal_model <- step(fit_full, scope, direction = 'backward')
ideal_model
summary(ideal_model)
mean(ideal_model$residuals)
sd(ideal_model$residuals)
sd(mtcars$mpg) == 0
round(cov(ideal_model$residuals, mtcars$am)) == 0
deviance(ideal_model) == sum(resid(ideal_model)^2)
summary(ideal_model)$sigma == sqrt(sum(ideal_model$residuals^2)/28)


fit_summary <- summary(ideal_model)
fit_summary$r.squared
fit_summary$r.squared == 1 - var(ideal_model$residuals) / var(mtcars$mpg) 
fit_summary$r.squared == sum((ideal_model$fitted.values-mean(mtcars$mpg))^2) / 
                         sum((mtcars$mpg - mean(mtcars$mpg))^2)

fit <- lm(mpg ~ wt, mtcars)
summary(fit)
cor(mtcars$mpg, mtcars$wt)

data <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt',
                   header = FALSE)

fit <- lm(V1 ~ . - 1, data = data)
summary(fit)$coef
plot(predict(fit), resid(fit), pch = '.')




















