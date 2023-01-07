#load library
library(dplyr)
getwd()
library(readr)

#read + review data
imdb <- read.csv("imdb.csv", stringsAsFactors = FALSE)
glimpse(imdb)
head(imdb)
tail(imdb)

#select data
select(imdb, MOVIE_NAME, RATING)
select(imdb,1 ,5)

#pipe operation
#filter(imdb, SCORE >=9)
imdb %>% filter(SCORE >=9)
imdb %>% select(MOVIE_NAME, YEAR, SCORE) %>% filter(SCORE >=9)
imdb %>% select(MOVIE_NAME, YEAR, SCORE) %>% filter(SCORE >=9 & YEAR > 2000)
imdb %>% select(MOVIE_NAME, LENGTH, SCORE) %>% filter(SCORE == 8.8 | SCORE == 8.3 | SCORE == 9)
imdb %>% select(MOVIE_NAME, LENGTH, SCORE) %>% filter( SCORE %in% c(8.3,8.8,9.0))

names(imdb) <- tolower(names(imdb))
names(imdb)

imdb %>% select(movie_name,genre)%>% filter(genre =="Drama")
imdb %>% select(movie_name,genre)%>% filter(grepl("Drama",imdb$genre))

imdb %>% select(movie_name) %>% filter(grepl("The",imdb$movie_name))
imdb %>% 
  select(movie_name,length,score)%>% 
  mutate(score_group = if_else(score>=9,"high","low"),
         length_group = if_else(length>=120,"long","short"))

imdb %>% select(movie_name,score) %>% mutate(score_up = score +1)
imdb %>% select(movie_name,score) %>% mutate(score_up = score -1.1)

imdb %>% arrange(length) %>% head(10)
imdb %>% arrange(desc(length)) %>% head(10)

imdb %>% arrange(rating, desc(length)) %>% head(10)
imdb %>% arrange(desc(length)) %>% head(10)

imdb %>% 
  filter(rating != "") %>%
  group_by(rating,score) %>%
  summarise(length_mean= mean(length),
                   lenght_max = max(length),
                   length_min = min(length))

film_fav <- data.frame(id = c(2,7,45,67,87))

imdb %>% inner_join(film_fav, by = c("no" = "id"))

film_fav %>% inner_join(imdb, by = c("id" = "no"))

imdb %>% group_by(rating,score)

imdb_prep <- imdb %>%
  select(movie_name,released_year = year,rating,length, score) %>%
  filter(rating == "R" & released_year > 2000)

write.csv(imdb_prep, "imdb_prep.csv",row.names = FALSE)

#tibble
df_tibble <- tibble(id = 1:3, name = c("lisa","jisoo","jenny"))
df <- data.frame(id = 1:3, name = c("lisa","jisoo","jenny"))

#mtcars
mtcars

mtcars_tibble <- tibble(mtcars)
mtcars_tibble

sample_n(mtcars,size=3)

sample_frac(mtcars, size = 0.4, replace = T)
sample_frac(mtcars, size = 0.4)

slice(mtcars, 1:5)
mtcars %>% slice(1:5)

mtcars %>% slice(1,3,5)

mtcars %>% slice( sample(nrow(mtcars),5))

sample(40,2)

