#This block of code is example 1. 
mtcars #data file
x=mtcars$mpg #we just the variable for mpg
table(mtcars$mpg) #table will count how many of each value
table(mtcars$cyl) #usually works better with a discrete variable
summary(x) #the 5 number summery to find max and min of mpg variable
range = 33.9 - 10.4 #calculate range using answers from 5 number summary
width = range/5  #class width given with 5 classes
breaks <- seq(10.35,33.9, by=width) #computes the intervals for each of the 5 classes, make lowest entry here just slightly less than 10.4 (the min), usually subtract the min by 0.5 ie 0.05
mtcars$mpg.cut <- cut(mtcars$mpg, breaks) #adds variable cut to mtcars data file.
table(mtcars$mpg.cut) #####this is the answer -- i.e., this gives the frequency table. 


#Solution to 1st problem using the above technique
v <-c(22,12,44,23,26,33,70,54,40,48,21,22,23,17,22,56,38,17,21,16,19,45,33,41,84,43,27,65,34,19,20,30,100,34,48,29,54,16,43,33,37,34,89,34,43,31,37,77,43,43) #imput statistics
summary(v) #get the 5 number summery to find max and min
range2 = 100 -12  #calculate range
width2 = range2/4 #calculate width
width2 #note that the width is a whole number but I will have to add some decimals to it to get the exact right frequency table.
break2 <- seq(11.5,100.5, by=22.2) #Here I need to finagle with the inputs to get the exact right answer in R (so that it will include the last entry of 100).  
#This is purely for deficiency in the cut function in R and is not a statement about our method. When we do this by hand, we should 
#always round up to the nearest whole number.
v.cut <- cut(v,break2) ##this creates the cut
table(v.cut)  ####This is th answer 


####This is the solution to the second feq. table problem in the lecture 
w <- c(17,-12,25,-37,-29,-39,-22,0,-22,-63,34,-31,-64,-12,-49,5,-8,33,-50,-7,16,-11,-38,-17,0,-9,-21,1,2,-30,-32,-34,-14,-18,5,6) #input data vector
summary(w) #get 5 number summary just so we can have the max and min
range3 = 34 - (-64) #calcuate range
width3 = range3/4 #as in the previous problem we have 4 classes
width3 #we need to round this number up to the nearest whole number always (this is )
break3 <- seq(-65,35, by=25)  #create breaks, again there is a little messing around with inputs to get the right breaks. In practice this is fine because one can visually do this part. 
w.cut <- cut(w,break3) #perform cut
table(w.cut) ####This is the answer. 

