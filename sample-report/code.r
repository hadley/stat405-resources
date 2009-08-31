library(ggplot2)

# Table and depth -------------------------

qplot(table, depth, data = diamonds)
qplot(table, depth, data = diamonds, xlim = c(50, 70), ylim = c(50, 70))
ggsave(file = "table-depth.png", width = 6, height = 6)

# Is there a linear relationship?
qplot(table - depth, data = diamonds,
  geom = "histogram")

# This bin width seems the most revealing 
qplot(table / depth, data = diamonds, geom = "histogram", binwidth = 0.01,
  xlim = c(0.8, 1.2))
ggsave(file = "table-depth-ratio.pdf", width = 6, height = 6)

# Also tried: 0.05, 0.005, 0.002