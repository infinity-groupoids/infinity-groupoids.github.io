install.packages("tidyverse")
library(ggplot2)

#needs package tidyverse 
#https://informationisbeautiful.net/visualizations/the-microbescope-infectious-diseases-in-context/
#creates data file
vir<-data.frame(var0=c("common cold","seasonal flu", "polio" , "swine flu", "chicken pox",
                       "measles", "mers", "bird flu", "ebola", "spanish flu", "sars", "smallpox"),
                var1=c(2,1,3.5,1.5,8.5,9,0.5,1.2,2.2,2.2,2.8,5),
                var2=c(0,0.1,23,0.2,0.01,0.8,35.6,58,50,5,9.6,30))
save(vir, file="vir.Rda")
vir


#plots scatterplot and simple linear regression
ggplot(vir,mapping=aes(x=var1,y=var2))+geom_point()+scale_y_log10()+stat_smooth(method='lm',fullrange=TRUE, se = TRUE)


#calculates blue regression line
lm(formula = var2 ~ var1, data=vir)
infect=vir$var1
mort=vir$var2
####Correlation:
cov(infect,mort)



####Altenerate displays:


# Create data frame
data <- data.frame(
  var0 = c("common cold", "seasonal flu", "polio", "swine flu", "chicken pox",
           "measles", "mers", "bird flu", "ebola", "spanish flu", "sars", "smallpox"),
  var1 = c(2, 1, 3.5, 1.5, 8.5, 9, 0.5, 1.2, 2.2, 2.2, 2.8, 5),
  var2 = c(0, 0.1, 23, 0.2, 0.01, 0.8, 35.6, 58, 50, 5, 9.6, 30)
)



# Scatterplot with color
ggplot(data, aes(x = var1, y = var2, color = var0)) +
  geom_point(size = 3, alpha = 0.8) +
  ggtitle("Mortality Rate vs. Infection Rate (Colored by Disease)") +
  xlab("Infection Rate") +
  ylab("Mortality Rate") +
  scale_color_discrete(name = "Diseases") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "gray"),
    legend.position = "right"
  ) +
  geom_text(aes(label = var0), hjust = 0.5, vjust = -1, size = 3)


ggplot(data = mtcars, aes(x = cyl, y = mpg, fill = cyl)) + # Use 'fill' to color boxes by 'cyl'
  geom_boxplot() +
  labs(
    title = "MPG Distribution by Number of Cylinders",
    x = "Number of Cylinders",
    y = "Miles Per Gallon (MPG)"
  ) +
  theme_minimal() + # A clean theme
  theme(legend.position = "none") # Remove the legend if 'fill' is redundant with the x-axis

