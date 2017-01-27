# Categorical data

df <- read.csv("grants.csv")

str(df)


df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))


# 1d Table 
t1 <- table(df$status)
t1

dim(t1)


# 2d Table
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)

dim(t2)

prop.table(t2)

prop.table(t2, 1)
prop.table(t2, 2)



# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)



# plots

barplot(t1)
barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

##########################

# Binomial Test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)


# Chi-Square
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs


t2
chisq.test(t2)



# Fisher's Exact Test

fisher.test(t2)

dimnames(HairEyeColor)

red_men <- 
HairEyeColor[]

HairEyeColor['Red', 'Blue','Male'] / sum(HairEyeColor[ , 'Blue','Male'])


red_men <- prop.table(HairEyeColor[, 'Blue','Male'])[3]

prop.table(HairEyeColor[ , ,'Male'],2)['Red','Blue']
prop.table(HairEyeColor[,"Blue" ,"Male" ] )["Red"]

# Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) 
# от общего числа голубоглазых мужчин.


sum(HairEyeColor[,'Green','Female'])

tapply(HairEyeColor[,,'Female'], rep(1:4, each = 4), sum)

mydata <- as.data.frame(HairEyeColor)

# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос 
# только у женщин из таблицы HairEyeColor. 
# По оси X должен идти цвет волос, цвет столбиков должен отражать цвет глаз. 
# По оси Y - количество наблюдений.

# ggplot(mydata[mydata$Sex=='Female',], 
#        aes(x = as.factor(Hair), , y = Freq)) + 
#         geom_bar()

# color = as.factor(Eye)


library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(mydata[mydata$Sex=='Female',], aes(x = Hair, y = Freq, fill = Eye)) + 
        geom_bar(stat="identity", position = 'dodge') + 
        scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))









a <- HairEyeColor['Brown',,'Female']
?chisq.test(a)


attach(diamonds)
main_stat <- chisq.test(cut, color)$statistic
detach(diamonds)

diamods_table <- table(diamonds$cut, diamonds$color)    
chi_result <- chisq.test(diamods_table )    
main_stat <- chi_result$statistic

xtabs(~cut+color, data=diamonds)

x <- c(1, 2, 3, 5, 6, 7) # mean(x) = 4
factor_x <- c(0, 0, 0, 1, 1, 1)

diamonds$factor_price <- ifelse(diamonds$price >= mean(diamonds$price), 1, 0)
diamonds$factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)
diamods_table <- table(diamonds$factor_price, diamonds$factor_carat)
main_stat <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic




mtcars
t <- table(mtcars$am, mtcars$vs)
ft <- fisher.test(t)
fisher_test <- ft$p.value
