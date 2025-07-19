install.packages("tidyverse")
install.packages("Lahman")
library(tidyverse, warn.conflicts=FALSE)
library(Lahman)


########################################################################





########################################################################


range = 33.9 - 10.4 #calculate range using answers from 5 number summary
width = range/5  #class width given with 5 classes
break1 <- seq(10.35,33.9, by=width) #computes the intervals for each of the 5 classes, make lowest entry here just slightly less than 10.4 (the min), usually subtract the min by 0.5 ie 0.05



###Now we use the above info to create histograms:

ggplot(data=mtcars, aes(mpg)) + 
  geom_histogram(breaks=break1, col="blue", fill="lightblue")+
  labs(title = "Histogram For Miles Per Gallon")

###histogram and density curve

ggplot(data=mtcars, aes(mpg)) + 
  geom_histogram(aes(y=..density..),breaks=break1, col="blue", fill="blue",alpha=.5)+geom_density(col="blue")+
  labs(title = "Histogram For Miles Per Gallon")

##################################################################################################################

practice1<-data.frame(v=c(22,12,44,23,26,33,70,54,40,48,21,22,23,17,22,56,38,17,21,16,19,45,33,41,84,43,27,65,34,19,20,30,100,34,48,29,54,16,43,33,37,34,89,34,43,31,37,77,43,43))
save(practice1, file="practice1.Rda")
practice1
range2 = 100 -12  #calculate range
width2 = range2/4 #calculate width
break2 <- seq(11.5,100.5, by=22.2) 

ggplot(data=practice1, aes(v)) + 
  geom_histogram(breaks=break2, col="blue", fill="blue",alpha=.5)+
  labs(title = "Histogram For Problem 1")


ggplot(data=practice1, aes(v)) + 
  geom_histogram(aes(y=..density..),breaks=break2, col="blue", fill="blue",alpha=.5)+geom_density(col="orange")+
  labs(title = "Histogram For Problem 1")
#################################################################################################################

w <- c(17,-12,25,-37,-29,-39,-22,0,-22,-63,34,-31,-64,-12,-49,5,-8,33,-50,-7,16,-11,-38,-17,0,-9,-21,1,2,-30,-32,-34,-14,-18,5,6) #input data vector
summary(w) #get 5 number summary just so we can have the max and min
range3 = 34 - (-64) #calcuate range
width3 = range3/4 #as in the previous problem we have 4 classes
width3 #we need to round this number up to the nearest whole number always (this is )
break3 <- seq(-65,35, by=25)  #create breaks, again there is a little messing around with inputs to get the right breaks. In practice this is fine because one can visually do this part. 
w.cut <- cut(w,break3) #perform cut
table(w.cut) ####This is the answer. 
practice2<-data.frame(w=c(17,-12,25,-37,-29,-39,-22,0,-22,-63,34,-31,-64,-12,-49,5,-8,33,-50,-7,16,-11,-38,-17,0,-9,-21,1,2,-30,-32,-34,-14,-18,5,6))
save(practice2, file="practice2.Rda")
practice2

ggplot(data=practice2, aes(w)) + 
  geom_histogram(aes(y=..density..),breaks=break3, col="blue", fill="blue",alpha=.5)+geom_density(col="orange")+
  labs(title = "Histogram For Problem 2")

########################################################################################################
#######This block of code continues to investigate ggplot> bar chart example.
#######################

install.packages("rattle")
library(rattle)
data(weatherAUS)

p <- ggplot(weatherAUS, aes(Location, fill=Location))

# using geom_bar will automatically make a new "count" column
# available in an internal, transformed data frame. the help
# for geom_bar says as much

p <- p + geom_bar(width=1, colour="white")

# geom_text can then access this computed variable with
# ..count.. (I still thin that's horrible syntax, hadley :-)
p <- p + geom_text(aes(y=..count.., label=..count..),
                   stat="count", color="white",
                   hjust=1.0, size=3)

p <- p + theme(legend.position="none")

p

# to be more explicit to other readers of your code, you
# could also do this instead of the `geom_bar` call
#p <- p + stat_count(width=1, colour="white", geom="bar")

########################################
###stem and leaf displays
############################################################
v=c(22,12,44,23,26,33,70,54,40,48,21,22,23,17,22,56,38,17,21,16,19,45,33,41,84,43,27,65,34,19,20,30,100,34,48,29,54,16,43,33,37,34,89,34,43,31,37,77,43,43)
stem(v)

stem(w)

mtcars

stem(mtcars$mpg)
