install.packages("tidyverse")
install.packages("nycflights13")
install.packages("Lahman")
library(tidyverse)
library(nycflights13)
library(Lahman)

#####
#The goal of this R session is to perform more advanced data analysis.
###

flights


#means of departure delays for each day; most days have an average delay of 0. Not so enlightening. 
#piping syntax

flights %>%
  group_by(year,month,day) %>%
  summarise(mean=mean(dep_delay, na.rm=TRUE))

#fliter out all cancelled flights


not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled


#grouping data by tailnumber of aircraft (this distinguishes between different aircraft types) 
#and taking mean


delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay=mean(arr_delay)
  )

delays

####The code above summerizes those flights which are not cancelled and their delays
#We plot this to see a Poisson Distribution:

ggplot(data=delays, mapping = aes(x=delay)) + geom_freqpoly(binwidth = 10)


#variation and sample size

delays <- not_cancelled %>% 
  group_by(tailnum) %>%
  summarise(
    delay=mean(arr_delay, na.rm=TRUE),
    n = n()
  )

delays
#####The code above gives a different way to study delays by frequency of each tailnumbeer
###This can be ploted with opaque scatter plot to see "rareness" seen below

ggplot(data = delays, mapping = aes(x=n, y=delay))+
  geom_point(alpha = 1/5)

#############Below are some ways to further filter to find planes which are problematic 


delays %>%
  filter(n > 5) %>%
  ggplot(mapping=aes(x=n , y = delay)) +
  geom_point(alpha=1/5)

delays %>%
  filter(n > 10) %>%
  ggplot(mapping=aes(x=n , y = delay)) +
  geom_point(alpha=1/5)

delays %>%
  filter(n > 30) %>%
  ggplot(mapping=aes(x=n , y = delay)) +
  geom_point(alpha=1/5)


delays %>%
  filter(n > 50) %>%
  ggplot(mapping=aes(x=n , y = delay)) +
  geom_point(alpha=1/5)

delays %>%
  filter(n > 100) %>%
  ggplot(mapping=aes(x=n , y = delay)) +
  geom_point(alpha=1/5)

delays %>%
  filter(n > 200) %>%
  ggplot(mapping=aes(x=n , y = delay)) +
  geom_point(alpha=1/5)


delays %>%
  filter(n > 300) %>%
  ggplot(mapping=aes(x=n , y = delay)) +
  geom_point(alpha=1/5)

###################################
#######More on variance of sample means with baseball states
###################################
###################################


Batting


batting <- as_tibble(Batting)



batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H, na.rm=TRUE)/sum(AB,na.rm = TRUE),
    ab = sum(AB, na.rm=TRUE)
  )

dbatters <- batting%>%
  group_by(playerID) %>%
  mutate(
    ba = sum(H, na.rm=TRUE)/sum(AB,na.rm = TRUE),
    ab = sum(AB, na.rm=TRUE)
  )

dbat<-dbatters %>%
  filter(ab>30)


dbat

ggplot(data=batting)+
  geom_histogram(mapping=aes(x=yearID),color="red",binwidth=10)

ggplot(data=batting)+
  geom_blotplot(mapping=aes(),color="red",binwidth=10)

#####################################
##code for boxplots (difficult)
#####################################
bin_size<-10
dbat%>%
  mutate(decade = factor(yearID%/%bin_size*10)) %>% 
  ggplot(aes(x = decade, y = ba),) +
  geom_boxplot(color="red")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Decade", y = "Batting Averages", caption = "(based on the data of all professional baseball players from 1861 to 2018)")





########################Another type of display, density much like previous work
#######optional material
#####################

batters %>% 
  filter(ab > 0) %>%
  ggplot(mapping = aes (x = ab, y = ba)) +
  geom_point(mapping=aes(color=ab), show.legend=FALSE) +
  scale_color_gradient2(low="darkorange", mid="lightblue", high="darkblue", midpoint =3000)+
  theme(aspect.ratio = 1)+
  geom_smooth(se = FALSE)




#graph for low at bats









batters %>% 
  filter(ab > 0 & ab < 100) %>%
  ggplot(mapping = aes (x = ab, y = ba)) +
  geom_point(mapping=aes(color=ab), show.legend=FALSE) +
  scale_color_gradient2(low="darkorange", mid="lightblue", high="darkblue", midpoint = 3000 )+
  theme(aspect.ratio = 1)+
  geom_smooth(se = FALSE)



batters %>% 
  filter(ab > 0 & ab < 500) %>%
  ggplot(mapping = aes (x = ab, y = ba)) +
  geom_point(mapping=aes(color=ab), show.legend=FALSE) +
  scale_color_gradient2(low="darkorange", mid="lightblue", high="darkblue", midpoint = 3000 )+
  theme(aspect.ratio = 1)+
  geom_smooth(se = FALSE)


batters %>% 
  filter(ab > 0 & ab < 1000) %>%
  ggplot(mapping = aes (x = ab, y = ba)) +
  geom_point(mapping=aes(color=ab), alpha=1/12, show.legend=FALSE) +
  scale_color_gradient2(low="darkorange", mid="lightblue", high="darkblue", midpoint = 3000 )+
  theme(aspect.ratio = 1)+
  geom_smooth(se = FALSE)



batters %>% 
  filter(ab > 0 & ab < 2000) %>%
  ggplot(mapping = aes (x = ab, y = ba)) +
  geom_point(mapping=aes(color=ab), alpha=1/12, show.legend=FALSE) +
  scale_color_gradient2(low="darkorange", mid="lightblue", high="darkblue", midpoint = 3000 )+
  theme(aspect.ratio = 1)+
  geom_smooth(se = FALSE)



batters %>% 
  filter(ab > 0 & ab < 4000) %>%
  ggplot(mapping = aes (x = ab, y = ba)) +
  geom_point(mapping=aes(color=ab), alpha=1/12, show.legend=FALSE) +
  scale_color_gradient2(low="darkorange", mid="lightblue", high="darkblue", midpoint = 3000 )+
  theme(aspect.ratio = 1)+
  geom_smooth(se = FALSE)



batters %>% 
  filter(ab > 0) %>%
  ggplot(mapping = aes (x = ab, y = ba)) +
  geom_point(mapping=aes(color=ab), show.legend=FALSE) +
  scale_color_gradient2(low="darkorange", mid="lightblue", high="darkblue", midpoint =3000)+
  theme(aspect.ratio = 1)+
  geom_smooth(se = FALSE)







not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    avg_delay1=mean(arr_delay),
    avg_delay2=mean(arr_delay[arr_delay > 0])
  )

not_cancelled %>%
  group_by(dest) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first=min(dep_time),
    last=max(dep_time)
  )

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))

not_cancelled %>%
  count(dest)

not_cancelled %>%
  group_by(year,month,day) %>% 
  summarise(n_early = sum(dep_time < 500))

flights_sml



flights_sml %>% 
  group_by(year,month,day) %>%
  filter(rank(desc(arr_delay)) < 10)

