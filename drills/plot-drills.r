library(ggplot2)

#1 pie
ggplot(diamonds, aes(x = "", fill = clarity)) + geom_bar(width = 1) + coord_polar(theta = "y")

#2 histogram
ggplot(diamonds, aes(clarity)) + geom_histogram(aes(colour = clarity)) 

# 3 Scatterplot with color
ggplot(diamonds, aes(carat, price, colour = cut)) + layer(geom = "point")

# 4 steelblue histogram
ggplot(diamonds, aes(carat)) + layer(geom = "bar",
  geom_params = list(fill = "steelblue"),
  stat = "bin",
  stat_params = list(binwidth = 2)
)

ggplot(diamonds, aes(carat)) + geom_histogram(binwidth = 2, fill = "steelblue")

# 5 scatterplot with a smooth
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth()

# 6 scatterplot with thick blue line of best fit
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point() + geom_smooth(method = "lm", se = F, colour = alpha("steelblue", 0.5), size = 2)

# 7 scatterplot with color coded points
ggplot(mtcars, aes(x = mpg, y = wt)) + 
  geom_point(aes(colour = factor(cyl)))

# 8 line graph with groups
library(nlme)
ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()

# 9 line graph with groups smooth line overlaid
library(nlme)
ggplot(Oxboys, aes(age, height, group = Subject)) + 
  geom_line() + 
  geom_smooth(aes(group = 1), method = "lm", size = 2, se = F)
  
#  10 boxplots
library(nlme)
ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()

# 11 boxplots with lines overlaid
library(nlme)
ggplot(Oxboys, aes(Occasion, height)) + 
  geom_boxplot() + 
  geom_line(aes(group = Subject), colour = "#3366ff")

# 12 stacked colored bar chart
ggplot(diamonds, aes(color)) + 
  geom_bar(aes(fill = cut))
  
# 13 histogram with height equal to density
ggplot(diamonds, aes(carat)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1)
  
# 14 bar chart stacked
ggplot(diamonds, aes(clarity)) + 
  geom_bar(aes(fill = cut), position = "stack")
  
# 15 bar chart dodged
ggplot(diamonds, aes(clarity)) + 
  geom_bar(aes(fill = cut), position = "dodge")
  
# 16 bar chart filled
ggplot(diamonds, aes(clarity)) + 
  geom_bar(aes(fill = cut), position = "fill")
  
# 17 area histogram
ggplot(diamonds, aes(carat)) + 
  xlim(0, 3) +
  stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
  
# 18 point size histogram
ggplot(diamonds, aes(carat)) + 
  xlim(0, 3) +
  stat_bin(aes(size = ..density..), binwidth = 0.1, 
    geom = "point", position = "identity")
    
# 19 tile histogram
ggplot(diamonds, aes(carat)) + 
  xlim(0, 3) +
  stat_bin(aes(y = 1, fill = ..count..), binwidth = 0.1, 
    geom = "tile", position = "identity")
    
# 20 facetted histogram
ggplot(diamonds, aes(depth)) +
  xlim(58, 68) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) + 
  facet_grid(cut ~ .)
  
