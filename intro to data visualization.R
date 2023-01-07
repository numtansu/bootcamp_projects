getwd()
library(tidyverse)
df <- mtcars %>% rownames_to_column() %>% tibble()
df <- mtcars

#1 histrogram - one quantitative variable
hist(df$mpg)
hist(df$hp)

# hp stat - one quantitative variable
mean(df$hp)
median(df$hp)
quantile(df$hp, probs = c(0.25,0.5,0.75))

#review structure
str(df)

#am to factor
df$am <- factor(df$am,
                levels = c(0,1),
                labels = c("auto","Manual"))
#2 Bar plot - one quantitative variable
table(df$am)  #check frequency           
barplot(table(df$am))

#3 Box plot - one quantitative variable
boxplot(df$hp)
fivenum(df$hp) #min,Q1,Q2,Q3,max

#whisker cal
q3 <- quantile(df$hp, probs = 0.75)
q1 <- quantile(df$hp, probs = 0.25)
(iqr_hp <- q3-q1)

q3 +1.5*iqr_hp
q1 - 1.5*iqr_hp #or
boxplot.stats(df$hp, coef = 1.5) #find outliner

#filter outlier
(df_noout <- df %>% filter(df$hp < 335))
boxplot.stats(df_noout$hp, coef = 1.5) #outliner=0
boxplot(df_noout$hp)

#Boxplot 2 variable
boxplot(mpg ~ am, data = df, col = c("gold","salmon"))


#4 Scatter plot - two quantitative variable
plot(df$hp,df$mpg, pch = 16 , 
     col = "blue",
     main = "hp vs mpg",
     xlab = "horse power",
     ylab = "milea per gal") #pch plot R


















































