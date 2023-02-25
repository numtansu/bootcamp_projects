library(tidyverse)

#DF1 air quality
airquality <- drop_na(airquality)
head(airquality)

boxplot(Temp ~ Month, data = airquality)

ggplot(airquality, aes(Temp,Ozone, col=Temp)) + geom_point()+theme_minimal()+ facet_wrap(~ Month, ncol=2)

#DF2 Diamond
(diamond <- tibble(diamonds))

boxplot(price~cut, data = sample_n(diamonds,size = 1000))

ggplot(sample_n(diamonds,size = 1000), aes(carat, fill=color))+geom_histogram(bins = 10)+theme_minimal()

ggplot(sample_n(diamonds,size = 1000),aes(carat,price,col=color)) + geom_point() +geom_smooth(method = "lm") + facet_wrap(~color,ncol=3)+theme_minimal()

