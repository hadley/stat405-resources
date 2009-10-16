my_sum <- function(vec){
  total <- 0
  for (i in 1:length(vec)){
  	total <- total + vec[i]
  }
  total
}

my_mean <- function(vec){
  my_sum(vec)/length(vec)
}

my_var <- function(vec){
  xbar <- my_mean(vec)
  n <- length(vec)
  sum <- 0
  
  for (i in 1:n){
  	sum <- sum + (vec[i] - xbar) ^ 2
  }
  
  # let's use n-1 so we can compare with R's var(). 
  # This is the sample variance
  sum / (n - 1)
 }	

scatterplot <- function(df){
  library(ggplot2)
  plotmatrix(df)
}

my_hist <- function(vec){
  hist(vec)
}

my_range <- function(vec){
  max(vec) - min(vec)
}

# my_breaks <- function(vec, n){
  binwidth <- my_range(vec) / n	
  min(vec) + c(1:n) * binwidth
}

better_hist <- function(vec, n){
  hist(vec, breaks = n)
}	



LCM <- function(a,b){
  test <- a * c(1:b)
  multiples <- test[integer(test/b)] 
  min(multiples)
} 
    
is.even <- function(num){
  num %% 2 == 0
}

is.odd <- function(num){
  num %% 2 == 1
}

one_to_odd <- function(num){
  if (is.odd(num))
    return(num + 1)
  return(num)
}   

one_to_odd2 <- function(vec){
  vec[is.odd(vec)] <- vec[is.odd(vec)] + 1
  vec
}	

# note: this function is better than the previous one
# It works for numbers AND vectors, and it is simpler

integer <- function(a){
  trunc(a) == a
}

split_num <- function(num){
  c(trunc(num), num - trunc(num))
}

clean <- function(vec){
  na.omit(vec)
}

	
get_NAs <- function(df){
  new <- na.omit(df)
  setdiff(row.names(df), row.names(new))
}

get_NAs2 <- function(df){
  new <- na.omit(df)
  rows <- setdiff(row.names(df), row.names(new))
  df[rows,]
}

div_vec <- function(vec){
  vec / length(vec)
}

rep_vec <- function(vec, n){
  rep(vec, n)	
}

rep_vec2 <- function(vec, n){
  vec[rep(1:length(vec), each = n)]
}

my_mean2 <- function(vec){
  my_mean(na.omit(vec))
}

random <- function(){
  sample(c("Ace", "King", "Queen"), 1)
}

random2 <- function(){
  sample(c("Ace", "King", "Queen"), 1, prob = c(2, 1, 1))
}

fortune_cookie <- function(fortune){
  paste(fortune, "...in Stat405.")
}	

fortune_cookie <- function(fortune){
  stopifnot(is.character(fortune))
  paste(fortune, "...in Stat405.")
}	

save_plot <- function(name){
  filename <- paste("name", "pdf", sep = ".")
  ggsave(filename, width = 6, height = 6)
}

save_plot2 <- function(name, width, height){
  filename <- paste("name", "pdf", sep = ".")
  ggsave(filename, width = width, height = height)
}

type <- function(obj){
  mode(obj)
}

num_seqs <- function(vec){
  n <- length(vec)
  factorial(n)
}

num_sets <- function(vec){
  vec <- unique(vec)
  n <- length(vec)
  my_sum(2 ^ c(0:(n-1)))
}

closest <- function(a){
  if (a > 1000){
  	a <- 1000
  }
  
  round(a, -3)
}

c_area <- function(r){
  pi * r ^ 2
}
  
c_circ <- function(r){
  2 * pi * r
}

c_vol <- function(r){
  4 / 3 * pi * r ^ 3
}	

c_stats <- function(r){
  c(circumference = c_circ(r),
    area = c_area(r),
    volume = c_vol(r)
  )
}	  

combos <- function(df){
  combinations <- table(df[,1], df[,2])
  df_counts <- as.data.frame(combinations)
  names(df_counts) <- c(names(df), "count")
  df_counts <- subset(df_counts, count > 0)
  df_counts
}

colorful_way_to_get_around_overplotting <- function(df){
  df_counts <- combos(df)
  df_counts <- within(df_counts, {
    x <- as.numeric(as.character(df_counts[,1]))
    y <- as.numeric(as.character(df_counts[,1]))
  })
  qplot(x, y, data = df_counts, colour = count)
}  



cumsum <- function(vec){
  new <- vector(length = length(vec))
  for(i in 1:length(vec)){
  	new[i] <- my_sum(vec[1:i])
  }
  new
}

# no perms!

n_choose_k <- function(n, k){
  factorial(n) / (factorial(k) * factorial(n - k))
}

quad_formula <- function(a, b, c){
  stopifnot(b ^ 2 >= 4 * a * c)
  c((-b - sqrt(b ^ 2 - 4 * a * c)) / (2 * a),
    (-b + sqrt(b ^ 2 - 4 * a * c)) / (2 * a))
}

min_pos <- function(vec){
  pos <- vec[vec > 0]
  min(pos)
}

min2_pos <- function(vec){
  pos <- vec[vec > 0]
  pos <- pos[-which(pos == min(pos))]
  min(pos)
}

select <- function(vec){
  distance <- abs(vec - vec[1])
  vec[which(distance == max(distance))]
}

# note: this concept only works for large amounts of skew - Garrett
skew <- function(vec){
  if (mean(vec) < median(vec))
    return("left-skewed")
  if (mean(vec) > median(vec))
    return("right-skewed")
  return("symmetric")
}

mode_vec <- function(vec){
  counts <- as.data.frame(table(vec))$Freq
  vec[which(counts == max(counts))]
}

how_to_index <- function(vec){
  vec / vec[1] * 100
} 

determinant <- function(matrix){
  matrix[1,1] * matrix[2,2] - matrix[1,2] * matrix[2,1]
}

ptile100 <- function(vec){
  vec[order(vec)]
  vec[70]
}

ptile10 <- function(vec){
  vec[order(vec)]
  vec[7]
}

ptile <- function(vec){
  vec[order(vec)]
  vec[round(.7 * length(vec), 1)]
}

save_file <- function(df){
  filename <- paste(substitute(df), "csv", sep = ".")

  # best method to avoid adding row numbers
  write.table(file, filename, sep = ",", row = F)
}

# no making directories through R

shuffle <- function(vec){
  sample(vec, length(vec), replace = F) 
}

order1 <- function(vec){
  vec[order(vec)]
}

order2 <- function(vec){
  vec[order(-vec)]
}
