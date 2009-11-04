library(maps)
library(ggplot2)

feb13 <- read.csv("delays/delays-feb-13-2007.csv")


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

ggsave(filename = "texmap1.png", width = 6, height = 4, dpi = 72)


# 2. Airlines point map
ggplot(feb13, aes(ntot, ncancel)) + 
  geom_point(data = subset(feb13, origin == "IAH"), size = 7,
  colour = alpha("red", 0.5)) +
  geom_point() +
  geom_text(data = subset(feb13, origin == "IAH"), 
  aes(label = origin), hjust = -.5) +
  geom_smooth(method = "lm", se = T) +
  labs(y = "Number of flights cancelled", 
    x = "Total number of flights")
    
ggsave(filename = "airports2.png", width = 6, height = 4, dpi = 72)



# 3. class names comparison
names <- read.csv("baby-names-data/baby-names.csv", header = T, stringsAsFactors = F)

class <- c("Rakesh", "Luis", "Yanli", "Yen-yin", "Sarah", "Delma", "Chandra", "Elizabeth", "Kim-chi", "Amanda", "Thomas", "Caroline", "Da", "Christine", "Debra", "Christopher", "Justin", "Lisa", "Meng", "Emilian","Rachel", "Lu", "Casper", "Jingjing", "Chengyong", "Ruo", "Zhongyu")

class_names <- subset(names, name %in% class)
class_names <- ddply(class_names, c("name", "year"), summarise, percent = sum(percent) / length(percent))

ggplot(class_names, aes(year, percent)) +
  geom_area(aes(group = name, fill = name)) +
  geom_text(aes(year, percent, 
    label = "*some names did not appear in the dataset"), 
    data = data.frame(year = 1925, percent = 0.10), size = 4)

ggsave(filename = "classnames3.png", width = 6, height = 4, dpi = 72)

   
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

ggsave(filename = "boxplots4.png", width = 6, height = 4, dpi = 72)
    
# 5a. 
ggplot(diamonds, aes(clarity)) + 
  geom_bar(aes(fill = cut), position = "dodge")
  
ggsave(filename = "dodge5a.png", width = 6, height = 4, dpi = 72)
  
# 5b.
ggplot(diamonds, aes(clarity)) + 
  geom_bar(aes(fill = cut)) +
  facet_grid(cut ~ .)
  
ggsave(filename = "facet5b.png", width = 6, height = 4, dpi = 72)
    