#Step 1: Base graphs

df  <- mtcars
df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

hist(df$mpg, breaks = 20, xlab = "MPG", main ="Histogram of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

plot(density(df$mpg), xlab = "MPG", main ="Density of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(mpg ~ am, df, ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(df$mpg[df$am == "Auto"], df$mpg[df$am == "Manual"], ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)


plot(df$mpg, df$hp, xlab = "MPG", ylab ="HP" , main ="MPG and HP", pch = 22)

plot(~ mpg + hp, df) 


#Step 2, 3: Library ggplot2

library(ggplot2)

ggplot(df, aes(x = mpg))+
  geom_histogram(aes(fill = cyl, col = wt), binwidth = 2)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  ggtitle("MPG histogram")

ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")


ggplot(df, aes(x = mpg))+
  geom_density(fill = "red")

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG density plot")


ggplot(df, aes(x = am, y = hp, fill = vs))+
  geom_boxplot()+
  xlab("Transmission type")+
  ylab("Gross horsepower")+
  scale_fill_discrete(name="Engine type")+
  ggtitle("Gross horsepower and engine type")


ggplot(df, aes(x = mpg, y = hp, size = qsec, fill = wt))+
  geom_point()+
  xlab("Miles/(US) gallon")+
  ylab("Gross horsepower")+
  scale_size_continuous(name="1/4 mile time")+
  ggtitle("Miles/(US) gallon and Gross horsepower")


my_plot  <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot2  <- ggplot(df, aes(x = am, y = hp, fill = vs))

my_plot2 + geom_boxplot()


boxplot(Ozone ~ Month, data = airquality)
ggplot(airquality, aes(x = as.factor(Month), y = Ozone, fill = as.factor(Month))) + 
        geom_boxplot()
# + facet_grid(aes(airquality$Month))

# ����� ��������� scatterplot � ������� ggplot �� ggplot2, �� ��� x 
# �������� ����� mpg, �� ��� y - disp, � ������ ���������� ���������� (hp).

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, color = hp)) +
        geom_point()

ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()


ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = Species)
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))


ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
        geom_point(aes(size = Petal.Length))

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species, size = Petal.Length)) +
        geom_point(alpha = .5)
