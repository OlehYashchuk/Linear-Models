#Step 2: Data preprocessing

?mtcars

df  <- mtcars

str(df)

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


#Step 3: Descriptive statistics

median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp  <- mean(df$disp)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

#Step 5: Aggregation

?aggregate

mean_hp_vs  <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs)  <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)


#Step 8, 9: Library "psych"

install.packages("psych")
library(psych)

?describe

describe(x = df)

descr  <- describe(x = df[,-c(8,9)])

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs)

descr2$V
descr2$S

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)

descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T, mat = T)


#Step 10: NA values

sum(is.na(df))

df$mpg[1:10]  <- NA

mean(df$mpg, na.rm = T)

aggregate(mpg ~am, df, sd)

describe(na.rm = )




mtcars
aggregate(mtcars$gear, by= list(mtcars$carb), mean)

aggregate(gear ~ carb + am, mtcars, mean)


aggregate(cbind(hp, disp)  ~ am, mtcars, sd)
aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)

airquality
sub <- airquality[airquality$Month %in% c(7, 8, 9),]
aggregate(Ozone ~ Month, sub, length)

sapply(subset, function(x) length(x, na.rm=TRUE))
sapply(subset, complete.cases)

describeBy(x = airquality[,c(1,2,3,4)], group = airquality$Month, mat = T, digits = 1)
describeBy(x = airquality$Wind, group = airquality$Month, digits = 1, mat = T)


sapply(iris[,-5], sd)

describe(iris)

# В данных iris расположите по убыванию значения медиан 
# количественных переменных в группе virginica.
describeBy(iris[,-5], group = iris$Species)$virginica


my_vector <- rnorm(30)

my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA


fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))

fixed_vector[is.na(my_vector)] <- mean(fixed_vector, na.rm = T)




