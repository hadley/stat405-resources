# git project

names <- read.csv("baby-names.csv", header = T)

# extracting all data related to my name
my_name <- subset(names, name == "Garrett" & sex == "boy")

# plotting my name's popularity over time
library(ggplot2)
qplot(year, percent, data = my_name, geom = "line")


# for later
both_names <- rbind(my_name, second_name)
# this step isn't necessary, but will save MUCH time when we graph
both_names$name <- factor(both_names$name)

qplot(year, percent, data = both_names, geom = "line", colour = name, main = "Popularity of group member names over time")
ggsave("names.png", width = 6, height = 6)