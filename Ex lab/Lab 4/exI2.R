parabola_area <- function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, 0, 2);
    y = runif(1, 0, 2);
    if(y <= -2*x*x + 5*x - 2)
      N_C = N_C + 1;
  }
  return(N_C/N*4)
}

area <- parabola_area(10000)
print(area)
