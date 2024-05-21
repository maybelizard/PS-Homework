f = function(x, p){
  s <- 0
  U <- runif(1, 0, 1)
  
  for (i in 1:length(x)){
    s <- s + p[i]
    if (s >= U) {
      return (x[i])
    }
  }
}

x <- c(2, 4, 5, 6, 7, 13, 23)
p <- c(0, 0.2, 0.1, 0.1, 0.2, 0.4, 0)

print(f(x, p))