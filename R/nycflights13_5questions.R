library(nycflights13)
library(dplyr)

#review all tables
flights %>% head(50)
airlines %>% head()
weather %>% head()
airports %>% head() %>% print(n=100)
planes %>% head()

#Q1:find the five shortest minimum distances
flights %>% select(origin,dest,distance)%>% distinct %>% arrange(distance) %>% head(5)

#Q2:Compute the average delay by destination
flights %>% group_by(tailnum) %>% summarise(AVG_delay_time = mean(dep_delay+arr_delay, na.rm = T),n_time=n()) %>% print(n=50)

#Q3:how many flight go to MIA in May 2013 catagorise by origin
flights %>% filter(dest=="MIA" & month==5) %>% group_by(origin) %>% summarise(n=n())

#Q4:Is there some particular airport with the highest delay in the 365 operation that needs to be avaoided?
flights %>% mutate(n_delay = if_else(arr_delay > 0 | dep_delay > 0 ,1,0)) %>%  group_by(origin) %>% summarise(total_flight=n(),total_delay=sum(n_delay, na.rm = T ),percent_flight_delay = mean(n_delay, na.rm = T)*100)

#Q5:Statistics of wind speed for each area
weather %>% group_by(origin) %>% summarise(max_wind_speed=max(wind_speed, na.rm = T),min_wind_speed = min(wind_speed, na.rm = T), AVG_wind_speed = mean(wind_speed, na.rm = T), SD_wind_speed = sd(wind_speed, na.rm = T))

summary(weather)
