#
# multiple linear regression
#

?swiss


# numeric predictors

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)

confint(fit)

fit <- lm(Fertility ~ Catholic, data = swiss)
summary(fit)

confint(fit)

ggplot(swiss, aes(x = Catholic, y = Fertility)) + 
        geom_point() + geom_smooth(method = 'lm')

fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)

confint(fit2)

# Quiz 1
test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")

fill_na <- function(df) {
        
        complete_df <- df[complete.cases(df),]
        
        fit <- lm(y ~., complete_df)
        df$y_full <- predict(fit, df)
        
        na_rows <- which(!is.na(df$y))
        df$y_full[na_rows] <- df$y[na_rows]
        
        return(df)
}

fill_na(test_data)

# Quiz 2
names(mtcars)
# mpg, disp, drat, hp
fit <- lm(wt ~ mpg + disp + hp, mtcars)
summary(fit)
confint(fit)

fit_full <- lm(wt ~ ., data = mtcars)
optimal_fit <-  step(fit_full, direction = 'backward')
opt_summary <- summary(optimal_fit)
attr(as.formula(opt_summary), "term.labels")

# Quiz 3
head(attitude)
fit <- lm(rating ~ complaints*critical, attitude)
summary(fit)





# categorical predictors

hist(swiss$Catholic, col = 'red')

swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)
levels(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)

# plots

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')


#

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)

# Quiz 4
# Теперь постройте линейную модель, в которой в качестве зависимой переменной 
# выступает расход топлива (mpg), а в качестве независимых - вес машины (wt) 
# и коробка передач (модифицированная am), а также их взаимодействие. 
# Выведите summary этой модели.
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
levels(mtcars$am)
head(mtcars)
model <- lm(mpg ~ am*wt, mtcars)
summary(model)

# Предсказанное значение DV = Intercept + 10 * IV_numeric
# Предсказанное значение DV = Intercept + IV_categoricalLevel2 + 
#                       6 * (IV_numeric + IV_numeric:IV_categoricalLevel2)
x <- 2
31.4161 + x * -3.7859 # manual
31.4161 + 14.8784 + x * (-3.7859 + -5.2984) # automat

ggplot(mtcars, aes(x = wt, y = mpg, color = am)) +
        geom_point() +
        geom_smooth(method = 'lm')


# model comparison

rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)


# model selection

optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)


# Quiz 4
head(attitude)

model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
scope <- list(lower = model_null, upper = model_full)
ideal_model <- step(model_full, scope, direction = 'backward')
ideal_model

# Quiz 5
# Сравните полную модель из предыдущего степа и оптимальную модель 
# с помощью функции anova. Введите получившееся F-значение.
# Разделителем дробной и целой части в ответе должна быть запятая. 
anova(model_full, ideal_model)

# Quiz 6
head(LifeCycleSavings)
lm(sr ~ .^2, LifeCycleSavings)














