#B1
torus_volume_monte_carlo = function(N) {
  R = 10
  r = 3
  exact_volume = 2 * pi^2 * R * r^2
  print(exact_volume)
  N_C = 0
  for(i in 1:N) {
    x = runif(1, -R-r, R+r)
    y = runif(1, -R-r, R+r)
    z = runif(1, -r, r)
    if(z^2 + (sqrt(x^2 + y^2) - R) ^ 2 < r^2){
      N_C = N_C + 1
    }
  }
  bounding_volume = 8*r*(R+r)^2
  monte_carlo_volume = bounding_volume*N_C/ N
  print(monte_carlo_volume)
  print("Relative error")
  print(abs(monte_carlo_volume - exact_volume)/abs(exact_volume))
}
torus_volume_monte_carlo(10000)
torus_volume_monte_carlo(20000)
torus_volume_monte_carlo(50000)

#B2
tringle_area_from_points = function(Ax,Bx,Cx,Ay,By,Cy){
  return (abs(0.5*((Ax*(By-Cy))+(Bx*(Cy-Ay))+(Cx*(Ay-By)))))
}
triangle_area_monte_carlo = function(N){
  N_C = 0
  x_min = 0
  y_min = 0
  x_max = 2
  y_max = 12/5
  for(i in 1:N) {
    x = runif(1, x_min, x_max)
    y = runif(1, y_min, y_max)
    if(y <= 2*x & y <= 6 - 3*x){
      N_C = N_C + 1
    }
  }
  area_bounding_box = (x_max - x_min) * (y_max - y_min)
  area_estimate = area_bounding_box * N_C / N
  print("Estimate Area:")
  print(area_estimate)
  print("Exact Area:")
  print(tringle_area_from_points(0, 6/5, 2, 0, 12/5, 0))
  
}
triangle_area_monte_carlo(20000)

#B3
estimate_integral = function(N, a, b, f, actual_value){
  x = runif(N, a, b)
  integral_estimate = sum(f(x)) * (b - a) / N
  print("Estimation") 
  print(integral_estimate)
  print("Actual value")
  print(actual_value)
}

#(a)
function_a = function(x){
  return ((2 * x - 1)/(x ^ 2 - x - 6))
}
estimate_integral(100000, -1, 1, function_a, log(3) - log(2))

#(b)
function_b = function(x){
  return ((x + 4)/((x - 3) ^ (1 / 3)))
}
estimate_integral(100000, 3, 11, function_b, 61.2)

#(c)
density <- function(x, lambda) {
  return (lambda * exp(-lambda * x))
}

estimate_inf_integral = function(N, lambda, f, actual_value){
  x = rexp(N, lambda)
  integral_estimate = sum(f(x) / density(x, lambda)) / N
  print("Estimation") 
  print(integral_estimate)
  print("Actual value")
  print(actual_value)
}

function_c = function(x){
  return (x * exp(-x ^ 2))
}
estimate_inf_integral(100000, 1, function_c, 1 / 2)

#B4
#(a)
average_years = function(N) {
  n = 1000
  p = 0.25
  q = 0.01
  avg_years = c()
  for (i in 1:N) {
    years = 0
    users = 10000
    while (users < 15000) {
      years = years + 1
      x = runif(users, 0, 1)
      users = users - sum(x <= 0.01)
      users = users + rbinom(1, n, p)
    }
    avg_years = append(avg_years, years)
  }
  return (sum(avg_years) / N)
}

print(average_years(1000))

#(b)
average_prob = function(N) {
  n = 1000
  p = 0.25
  q = 0.01
  avg_prob = 0
  for (i in 1:N) {
    years = 0
    users = 10000
    while (years < 40.83) {
      years = years + 1
      x = runif(users, 0, 1)
      users = users - sum(x <= q)
      users = users + rbinom(1, n, p)
    }
    if (users >= 15000)
      avg_prob = avg_prob + 1
  }
  return (avg_prob / N)
}

print(average_prob(1000))

#(c)
error_prob = function() {
  n = 1000
  p = 0.25
  q = 0.01
  error = 0.01
  value = 0.99
  simulations = 0
  avg_prob = 0
  while(TRUE) {
    simulations = simulations + 1
    years = 0
    users = 10000
    while (years < 40.83) {
      years = years + 1
      x = runif(users, 0, 1)
      users = users - sum(x <= q)
      users = users + rbinom(1, n, p)
    }
    if (users >= 15000)
      avg_prob = avg_prob + 1
    if (abs(avg_prob / simulations - value) <= error)
      return (list(simulations = simulations, prob = avg_prob / simulations))
  }
}

result = error_prob()
print(paste("Simulations: ", result$simulations))
print(paste("Probability: ", result$prob))