library(ggplot2)

df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit  <- lm(mpg ~ hp, df)
summary(fit)

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)

predict(fit, new_hp)

rmse <- sqrt(sum((fitted_values_mpg$mpg - fitted_values_mpg$fitted)^2))
rmse

##################################

my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)

summary(fit)
aggregate(mpg ~ cyl, my_df, mean)

getwd()
setwd("c:/Users/dsolegya/Downloads")
df <- read.table("dataset_11508_12 (4).txt",  header = FALSE, sep = " ", dec = ".")
str(df)
fit <- lm(V1 ~ V2, df)
summary(fit)
fit$coefficients

diamonds

ideal046 <- subset(diamonds, cut == 'Ideal' & carat == .46)
fit <- lm(price ~ depth, ideal046)
fit_coef <- fit$coefficients


corr.calc <- function(df) {
        fit <- cor.test(~., df)
        return(c(fit$estimate, fit$p.value))
}

cor.test(d$Sepal.Length, d$Sepal.Width)

smart_cor <- function(df, alpha = .05) {
        
        p.vals <- sapply(df, 
                         function(x) {stat <- shapiro.test(x); stat$p.value})
        
        correlation <- ifelse(any(p.vals < alpha), 
                              cor(df[1], df[2], method = "spearman"),
                              cor(df[1], df[2], method = "pearson"))
        
        return(correlation)
}

smart_cor(iris[,1:2])

regr.calc <- function(df, alpha = .05) {
         
        cor_p.val <- cor.test(df[,1], df[,2])$p.value
        
        if (cor_p.val < alpha) {
                
                model <- lm(df[,1] ~ df[,2], df)
                df$fit <- model$fitted.values
                return(df)
                
        } else {
                return("There is no sense in prediction")
        }
}

regr.calc(iris[,c(1,4)])

# Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
#         Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений 
# по переменной Species.
# 
# Если Вы все сделали правильно должен получиться следующий график:

my_plot
my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, col = Species)) +
        geom_point() +
        geom_smooth(method = 'lm')

ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
        geom_point()+
        geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
        geom_point(aes(col = factor(am)))+
        geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
        geom_point()+
        geom_smooth(aes(col = factor(am)))






