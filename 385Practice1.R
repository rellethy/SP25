x <- c(5,  13,  28.9, 17)

mean(x)


state.area

median(state.area)

list(state.area)


ls(9)

list(state.name)

mean(nchar(state.name))




divisor <- function(n) {
    for (i in 3:ceiling(sqrt(n))) { 
      if (n %% i == 0) return(i)
    }
  return(n)
}


n <- 1646086081

divisor(1646086081)



3:ceiling(sqrt(n))


data(lynx)
data("USAccDeaths")


lynx_data <- as.numeric(lynx)
death_data <- as.numeric(USAccDeaths)


segment_length <- 25


best_corr <- 1
best_l_i <- 1
best_d_i <- 1


for (i in 1:(length(lynx_data) - segment_length + 1)) {
  lynx_segment <- lynx_data[i:(i + segment_length -1)]
  
  
  for (j in 1:(length(death_data) - segment_length + 1)) {
    death_segment <- death_data[j:(j + segment_length - 1)]
    
    correlation <- cor(lynx_segment, death_segment)
    
    if (correlation < best_corr) {
      best_corr <- correlation
      best_l_i <- i
      best_d_i <- j
    }
    
  }
}


cat("Lowest Corr", best_corr)


library(ggplot2)


ggplot()


