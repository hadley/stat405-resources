library(maps)
library(ggplot2)

feb13 <- read.csv("delays/delays-feb-13-2007.csv")
usa <- map_data("state")
usamap <- c(
  geom_polygon(aes(group = group), data = usa, colour = "grey70", fill = NA),
  scale_x_continuous("", limits = c(-125, -67)),
  scale_y_continuous("", limits = c(24, 50))
)


# 1. Texas Plane flights

texas <- map_data("state", "texas")
texmap <- c(
  geom_polygon(data = texas, colour = "grey70", fill = NA),
  scale_x_continuous("", limits = c(-107, -93)),
  scale_y_continuous("", limits = c(25.9, 37))
)

ggplot(feb13, aes(long, lat)) +
  texmap + 
  geom_point(aes(size = ntot, colour = ndelay / ntot)) + 
  geom_text(aes(label = origin), 
    data = subset(feb13, ndelay >= 100), 
    size = 4, hjust = 1.5) +
  scale_area("total flights", to = c(1, 8)) + 
  scale_colour_gradient("percent delayed")


# 2. class names comparison
names <- read.csv("baby-names-data/baby-names.csv", header = T, stringsAsFactors = F)

class <- c("Rakesh", "Luis", "Yanli", "Yen-yin", "Sarah", "Delma", "Chandra", "Elizabeth", "Kim-chi", "Amanda", "Thomas", "Caroline", "Da", "Christine", "Debra", "Christopher", "Justin", "Lisa", "Meng", "Emilian","Rachel", "Lu", "Casper", "Jingjing", "Chengyong", "Ruo", "Zhongyu")

class_names <- subset(names, name %in% class)
class_names <- ddply(class_names, c("name", "year"), summarise, percent = sum(percent) / length(percent))

ggplot(class_names, aes(year, percent)) +
  geom_area(aes(group = name, fill = name)) +
  geom_text(aes(year, percent, 
    label = "*some names did not appear in the dataset"), 
    data = data.frame(year = 1925, percent = 0.10), size = 4)
    
    
# 4. names boxplots
ggplot(class_names, aes(year, percent)) +
  geom_boxplot(aes(group = round_any(year, 5, floor))) +
  geom_smooth(se = F, size = 1) +
  geom_text(aes(year, percent, 
    label = "*blue line is a smoothed mean"), colour = "blue",
    data = data.frame(year = 1906, percent = 0.029), size = 4) +
  geom_text(aes(year, percent, 
    label = "Popularity of class names as a group"), 
    data = data.frame(year = 1911, percent = 0.03), size = 4)
    
    
    
# Failed -- Diamonds line graph
ggplot(diamonds, aes(price, carat)) +
  geom_smooth(aes(colour = cut), method = "lm", se = F) +
  geom_rect(xmin = 3000, xmax = 6000, ymin = 0, ymax = 3) +
  scale_fill_manual(values = alpha("red", 0.2)) +
  coord_cartesian(ylim = c(0,3)) + 
  ylab("Average Carat Size") + 
  
# 3. Airlines point map
ggplot(feb13, aes(ntot, ncancel)) + 
  geom_point(data = subset(feb13, origin == "IAH"), size = 7,
  colour = alpha("red", 0.5)) +
  geom_point() +
  geom_text(data = subset(feb13, origin == "IAH"), 
  aes(label = origin), hjust = -.5) +
  geom_smooth(method = "lm", se = T) +
  labs(y = "Number of flights cancelled", 
    x = "Total number of flights")
    
# Diamonds facetted histogram
ggplot(diamonds, aes(depth)) +
  xlim(58, 68) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) + 
  geom_vline(aes(xintercept = max(..density..), colour = "blue") +
  facet_grid(cut ~ .)
  
# Diamonds faceted line plot
ggplot(diamonds, aes(x, y)) + 
  geom_point() +
  geom_smooth(method = "lm", colour = "blue") +
  facet_grid(cut ~ clarity)
  
  
  
  
# Lame -- airlines boxplot
ggplot(feb13, aes(lat, avgdelay)) +
  geom_point() +
  geom_boxplot(aes(group = round(lat, -1)))
  geom_line(aes(group = manufacturer))
  
# Diamonds tile
ggplot(diamonds, aes(carat, price)) +
  geom_tile(stat = "density2d")
  
  
  
# 4. Diamonds pie chart
ggplot(diamonds, aes(x = "", fill = cut)) + 
  geom_bar(width = 1) + 
  coord_polar(theta = "y")
  
# 5a. 
ggplot(diamonds, aes(clarity)) + 
  geom_bar(aes(fill = cut), position = "dodge")
  
# 5b.
ggplot(diamonds, aes(clarity)) + 
  geom_bar(aes(fill = cut)) +
  facet_grid(cut ~ .)

  


  
