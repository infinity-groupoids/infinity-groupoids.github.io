install.packages("mosaic")
install.packages("tidyverse")

library(mosaic)
library(tidyverse)

######Random_Walk_Example: 
create_random_walk <- function(number=1000){
  data.frame(x = rnorm(number),
             rown = c(1:1000)) %>%
    mutate(xt = cumsum(x))
}


p <- ggplot() + aes(x = rown, y = xt)

p + geom_line(data = create_random_walk())

################equiprobability
############More probability 
#########binomial

coin <- c("H","T")
resample(coin, 3)
resample(coin, 10)
resample(coin, 10, prob = c(.9,.1)) #unbalanced coin


#multinomial with resample
dice <- seq(1,6)
resample(dice, 3)
resample( dice, 15, prob=c(0.1,0.1,0.1,0.1,0.1,0.5)) #unbalanced dice


#Standard Normal

#install (if not already installed) and load ggplot2
if(!(require(ggplot2))){install.packages('ggplot2')}

#generate a normal distribution plot
ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm)


#what it looks like in real life

mtcars

ggplot(mtcars)+geom_point(mapping=aes(x=mpg,y=wt,color=carb))

#because wt=weight is continuous

ggplot(mtcars)+geom_point(mapping=aes(x=mpg,y=cyl,color=carb))

#because cyl=number of cylinders is discrete


ggplot(mtcars,mapping=aes(y=mpg))+geom_boxplot()+coord_flip()

#lets create a frequency histogram by hand


ggplot(mtcars, aes(x = mpg)) +
  stat_function(
    fun = dnorm,
    args = with(mtcars, c(mean = mean(mpg), sd = sd(mpg)))
  ) +
  scale_x_continuous("Miles per gallon")


#a normal distribution with mean 1000 and standard deviation 100

samps <-rnorm(15, mean=1000,sd=100). ##samples for rnorm
samps
newmean<-mean(samps)
newsd<-sd(samps)
print(newmean)
print(newsd)

#the above are just sample statistics but if the comp
#uter language R is correct then they model an underlying probability distribution 
#i.e., the provide the parameters to the underlying standard normal curve
#this a huge part statistics in general: understanding when our statistics can be modeled by a 
#probability distribution