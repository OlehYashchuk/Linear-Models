#
# regression diagnostics
# 

library(ggplot2)

data(swiss)
str(swiss)
head(swiss)


# relationships between all variables
pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()


# Outliers

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


# Normality of variables distributions

ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + 
  geom_histogram()

ggplot(swiss, aes(x = log(Education))) + 
        geom_histogram()

# Quiz 1
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 
               0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 
               0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 
               0.255, 0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 
               0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 0.071, 0.213, 0.232, 
               0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(1/(my_vector))

# Quiz 2
# Функция scale() позволяет совершить стандартизацию вектора, 
# то есть делает его среднее значение равным нулю, 
# а стандартное отклонение - единице (Z-преобразование). 

# Стандартизованный коэффициент регрессии (????) можно получить, 
# если предикторы и зависимая переменная стандартизованы.

# Напишите функцию, которая на вход получает dataframe с двумя количественными 
# переменными, а возвращает стандартизованные коэффициенты для 
# регрессионной модели, в которой первая переменная датафрейма 
# выступает в качестве зависимой, а вторая в качестве независимой.

# beta.coef(mtcars[,c(1,3)])
# -7.036582e-17 -8.475514e-01
# beta.coef(swiss[,c(1,4)])
# 3.603749e-16 -6.637889e-01

df <- mtcars[,c(1,3)]

beta.coef <- function(df) {
        df <- scale(df) # now df is matrix
        fit <- lm(df[,1] ~ df[,2])
        return(fit$coefficients)
}
c <- beta.coef(df)
c
install.packages("QuantPsyc")
library(QuantPsyc)

# Quiz 3
dft <- mtcars[,1:6]
sapply(dft, function(x) shapiro.test(x)$p.value)

normality.test <- function(df, alpha = .05) {
        return(sapply(df, function(x) shapiro.test(x)$p.value))
}

normality.test(iris[,-5])


# linearity 

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)


swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)


anova(lm2, lm1)


swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) + 
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd=1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_hline(yintercept =  0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(yintercept = 0, col = 'red', lwd = 1)


# independence of errors

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()



# Homoscedasticity

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)

# Quiz 1
install.packages("gvlma")
library(gvlma)
getwd()
setwd("c:/Users/dsolegya/Downloads")
data <- read.csv("homosc.csv")
head(data)
fit <- lm(DV ~ IV, data)
x <- gvlma(fit)
y <- gvlma(DV ~ IV, data)
summary(x)



# Errors Normally distributed

ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)


ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)


# Quiz 2
fit <- lm(mpg ~ disp, mtcars)
fit <- lm(mpg ~ wt, mtcars)

resid.norm <- function(model, alpha = .05) {
        
        H0 <- shapiro.test(model$residuals)$p.value < alpha
        brush <- ifelse(H0, "red", "green")
        pict <- ggplot(as.data.frame(model$residuals), 
                       aes(x = model$residuals)) + 
                geom_histogram(fill = brush)
        
        return(pict)
}

my_plot <- resid.norm(fit)
my_plot

# Quiz 3
high.corr <- function(df) {
        cor_mat <- abs(cor(df))  # корреляционная матрица
        diag(cor_mat) <- 0
        
        t <- rep(1 : nrow(cor_mat), each = ncol(cor_mat)) # индикатор для применения функции tapply
        colmax <- data.frame(tapply(cor_mat, t, max))     # находим максимум в каждом столбце корреляционной матрицы
        
        colnames(df)[colmax == max(colmax)]    # возвращаем названия тех столбцов (переменных) у которых наибольшая корреляция
}

# Проверка
test_data <- as.data.frame(list(V1 = c(-0.8, -2.1, -1, 2.8, -1.8), V2 = c(1, 0.3, -1.5, -0.4, 1.5), V3 = c(-0.6, -1.9, -1, -0.9, 0.5), V4 = c(0.2, -1.4, 0.6, -0.9, 0.6), V5 = c(-1.3, -0.9, 1.3, -0.5, -0.3), V6 = c(1, -1.5, -0.6, -0.4, 1.6), V7 = c(0.4, -1.9, -0.9, -0.3, -0.7), V8 = c(-1.3, 0.2, -0.1, -0.3, 0.3), V9 = c(0.2, -0.1, 1.3, -0.5, 0.2), V10 = c(-0.7, 1.8, -0.2, -0.4, 0.8), V11 = c(-0.2, 1.4, -0.6, 0.9, -0.6)))
high.corr(test_data)

x1 <- rnorm(30) # создадим случайную выборку
x2 <- rnorm(30) # создадим случайную выборку
x3 <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
x4 <- x3 - 2
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3, var4 = x4)
high.corr(my_df)








