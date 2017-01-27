df  <- mtcars

cor.test(x = df$mpg, y = df$hp)

ggplot(df, aes(mpg, hp)) + 
        geom_point()

fit  <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)+
  facet_grid(. ~ am)


###########################################

df  <- mtcars
df_numeric  <- df[, c(1,3:7)]

pairs(df_numeric)

cor(df_numeric)

fit  <- corr.test(df_numeric)
fit$r
fit$p
fit$adjust


# Ќапишите функцию corr.calc, котора€ на вход получает data.frame 
# с двум€ количественными переменными, рассчитывает 
# коэффициент коррел€ции ѕирсона и возвращает вектор из двух значений: 
# коэффициент коррел€ции и p - уровень значимости.

corr.calc <- function(df) {
        fit <- cor.test(~., df)
        return(c(fit$estimate, fit$p.value))
}

a <- mtcars[,c(1,5)]
b <- cor.test(~., a)
b$p.value
b$estimate

b <- corr.calc(a)

step6 <-  read.table("step6.csv",  header=TRUE, sep=',' )
a <- which(sapply(step6, is.numeric))
b <- step6[,a]
cor(b, use = "pairwise")
nrow(cor(b))
cor(b, use = "pairwise") == cor(b)
max(c)

matrix(0, 13, 13)

diag(1, 13, 13)

pairs(step6)

filtered.cor <- function(df) {
        num_feature <- which(sapply(df, is.numeric))
        num_df <- df[, num_feature]
        cor_df <- cor(num_df, method = c("pearson"))
        diag(cor_df) <- 0
        return(cor_df[which.max(abs(cor_df))])
}

filtered.cor(step6)


test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
smart_cor(test_data)

c <- sapply(test_data, function(x) {stat <- shapiro.test(x); stat$p.value})
ifelse(c < .05, cor())
cor(test_data[1], test_data[2], method = "spearman")
any(c < .05)

smart_cor <- function(df, alpha = .05) {
        
        p.vals <- sapply(df, 
                         function(x) {stat <- shapiro.test(x); stat$p.value})

        correlation <- ifelse(any(p.vals < alpha), 
               cor(df[1], df[2], method = "spearman"),
               cor(df[1], df[2], method = "pearson"))
        
        return(correlation)
}
smart_cor(test_data)

