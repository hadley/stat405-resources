library(maps)
library(ggplot2)

feb13 <- read.csv("delays/delays-feb-13-2007.csv", header = T, stringsAsFactors = F)


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
    data = data.frame(year = 1925, percent = 0.10), size = 3)

ggsave(filename = "classnames3.png", width = 6, height = 4, dpi = 72)

   
# 4. names boxplots
ggplot(class_names, aes(year, percent)) +
  geom_boxplot(aes(group = round_any(year, 5, floor))) +
  geom_smooth(se = F, size = 1) +
  geom_text(aes(year, percent, 
    label = "*blue line is a smoothed mean"), colour = "blue",
    data = data.frame(year = 1906, percent = 0.029), size = 3) +
  geom_text(aes(year, percent, 
    label = "Popularity of class names as a group"), 
    data = data.frame(year = 1911, percent = 0.03), size = 3)

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
    
    
# batting data set

b <- read.csv("batting.csv", header = T, stringsAsFactors = F)

# 6. Tiled density games by year
# Note: won't work unless contour = F
ggplot(b, aes(year, g)) +
  stat_density2d(geom = "tile", aes(fill = ..density..), contour = F) +
  scale_fill_gradient(low = "black", high = "white") 
  
ggsave(filename = "battile6.png", width = 6, height = 4, dpi = 72)



# 7. Homeruns Yankees vs. Red Sox
library(plyr)
yankees <- subset(b, team == "NYA")
yankees <- transform(yankees, team = "Yankees")
boston <- subset(b, team == "BOS")
boston <- transform(boston, team = "Red Sox")
yb <- rbind(yankees, boston)

yb_runs <- ddply(yb, c("year", "team"), summarise,
  total_runs = sum(r, na.rm = T)) 

ggplot(yb_runs, aes(year, total_runs)) +
  geom_smooth(aes(colour = team)) +
  scale_colour_manual(value = c("red", "blue")) +
  geom_vline(aes(xintercept = c(1918, 2004))) +
  geom_text(aes(x,y, label = "Curse Begins"), 
    data = data.frame(x = 1917, y = 400), size = 3, hjust = 0,
    vjust = 0, angle = 90) +
  geom_text(aes(x,y, label = "Curse Ends"), 
    data = data.frame(x = 2003, y = 400), size = 3, hjust = 0, 
    vjust = 0, angle = 90)
  
ggsave(filename = "hrline7.png", width = 6, height = 4, dpi = 72)


# 8. Homeruns with bars
yb_homeruns <- ddply(yb, c("year", "team"), summarise,
  total_hr = sum(hr, na.rm = T)) 


ggplot(yb_homeruns, aes(year, total_hr)) +
  geom_bar(aes(fill = team), stat = "identity", position = "dodge") + 
  scale_fill_manual(value = alpha(c("red", "blue"), 0.4)) +
  geom_smooth(aes(colour = team)) +
  scale_colour_manual(value = c("red", "blue"))  
  
ggsave(filename = "hrbars8.png", width = 6, height = 4, dpi = 72)


# 9. Homeruns area
ggplot(yb_homeruns, aes(year, total_hr)) +
  geom_area(aes(fill = team), position = "identity") +
  scale_fill_manual(value = alpha(c("red", "blue"), 0.4)) +
    geom_vline(aes(xintercept = 1918)) +
  geom_text(aes(x,y, label = "Curse Begins"), 
    data = data.frame(x = 1919, y = -10), size = 3, hjust = 0,
    vjust = 0)
    
ggsave(filename = "hrarea9.png", width = 6, height = 4, dpi = 72)

    
# 10. Homeruns boxplot facetted by curse year
yb_curse <- subset(yb, year > 1918 & year <= 2004)
yb_curse <- transform(yb_curse, curse = "Curse years") 

yb_noncurse <- subset(yb, year <= 1918 | year > 2004)
yb_noncurse <- transform(yb_noncurse, curse = "Non-curse Years") 

yb <- rbind(yb_curse, yb_noncurse)

ggplot(yb, aes(team, hr / r)) +
  geom_boxplot() +
  facet_grid( . ~ curse) 
  
ggsave(filename = "hrcurse10.png", width = 6, height = 4, dpi = 72)

  
# players data set
p <- read.csv("players.csv", header = T, stringsAsFactors = F)


# 11. World map of players
library(maps)
world_map <- map_data("world")
names(world_map)[5] <- "country"

p_country <- ddply(p, "country", summarise, total = length(country))

p_map <- merge(p_country, world_map, by = "country", all = T)
p_map <- p_map[order(p_map$order), ]

ggplot(p_map, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = log(total)), colour = "grey60", size = .3) +
  ylim(-55, 85)
  
ggsave(filename = "playermap11.png", width = 6, height = 4, dpi = 72)

  
# 12. Area map of states
bp <- merge(b, p, by = "id")
bp_country <- ddply(bp, "country", summarise, total = length(country))
bp_country <- bp_country[order(-bp_country$total), ]
bp_10 <- subset(bp, country %in% bp_country[2:11, 1])


ggplot(bp_10, aes(year)) +
  geom_area(aes(y = ..count.., fill = country), stat = "bin", binwidth = 10, position = "stack") + 
  opts(title = "10 most represented foreign countries in combined dataset") +
  xlab("year (bin = 10 years)") 
  
ggsave(filename = "statefill12.png", width = 6, height = 4, dpi = 72)


# 13.Right vs. left handers
bp_trimmed <- subset(bp, bats != "")
ggplot(bp_trimmed, aes(throws)) +
  geom_bar() + 
  facet_grid (. ~ bats) +
  opts(title = "Hand preference by batting preference")
  
ggsave(filename = "hand13.png", width = 6, height = 4, dpi = 72)

  

# 14. Strikeouts by height
ggplot(bp, aes(height, so)) +
  geom_jitter(position = position_jitter(width = 5), alpha = 0.05) +
  xlim(60, 85)
  
ggsave(filename = "soheight14.png", width = 6, height = 4, dpi = 72)

  
# 15. labelled home runs
ggplot(subset(bp, hr > 60), aes(weight, hr)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_text(aes(label = paste(first, last, sep = " ")), hjust = -0.1) +
  xlim(203, 233) +
  opts(title = "Weight vs. performance among record holders")
  
ggsave(filename = "hrweight15.png", width = 6, height = 4, dpi = 72)



# delays data set
feb13 <- read.csv("delays/delays-feb-13-2007.csv", 
  header = T, stringsAsFactors = F)

# 16. US Map
lower48 <- subset(feb13, long > -130)
lower48 <- subset(lower48, lat > 20)

ggplot(subset(lower48, ntot >= 100), aes(long, lat)) +
  borders("state") +
  geom_point(aes(size = ndelay, colour = log(avgdelay)))
  
ggsave(filename = "airmap16.png", width = 6, height = 4, dpi = 72)

  

# 17. cancelled by longitude

ggplot(feb13, aes(long, cperc)) +
  geom_point(aes(colour = cperc, size = ntot)) + 
  geom_text(data = subset(feb13, cperc > 0.4 & long < -100),
    aes(label = origin), hjust = 1.2, angle = -45, 
    colour = "orange")

ggsave(filename = "longdelay17.png", width = 6, height = 4, dpi = 72)



# 18. Number of flights by longitude

ggplot(feb13, aes(long, ntot)) +
  geom_area(aes(y = ..density..), stat = "density", alpha = 0.5) +
  geom_vline(xintercept = c(-118, -87)) +
  geom_text(aes(x,y, label = "Los Angeles"), 
    data = data.frame(x = - 119, y = 0), size = 4, hjust = 0,
    vjust = 0, angle = 90) +
  geom_text(aes(x,y, label = "Chicago"), 
    data = data.frame(x = -88, y = 0), size = 4, hjust = 0, 
    vjust = 0, angle = 90)
    
ggsave(filename = "longtot18.png", width = 6, height = 4, dpi = 72)

  

# 19. Number of flights by airport
main <- subset(feb13, ntot > 400)

ggplot(main, aes(origin, ntot)) +
  geom_bar(aes(fill = cperc)) +
  opts(axis.text.x = theme_text(angle = 90, hjust = 1))
  
ggsave(filename = "topairports19.png", width = 6, height = 4, dpi = 72)


# diamonds data set  
  
# 20. pie chart by cut
ggplot(diamonds, aes(x = "", fill = cut)) + 
  geom_bar(width = 1) + 
  coord_polar(theta = "y")  

ggsave(filename = "pie20.png", width = 6, height = 4, dpi = 72)
  
    
