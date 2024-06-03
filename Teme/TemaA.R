#A1
#(a)
functionPoissonGeometricBinomial = function(lambda, p, k, m, n){
  poisson_list = c()
  geometric_list = c()
  binomial_list = c()
  
  for(i in k:m){
    poisson_list <- append(poisson_list, dpois(i, lambda))
    geometric_list <- append(geometric_list, dgeom(i, p))
    binomial_list <- append(binomial_list, dbinom(i, n, p))
  }
  print("Poisson list: ")
  print(poisson_list)
  
  print("Geometric list: ")
  print(geometric_list)
  
  print("Binomial list: ")
  print(binomial_list)
  
  return (matrix(c(poisson_list, geometric_list, binomial_list), nrow=3, ncol=m-k+1, byrow=TRUE))
}
probabilities = functionPoissonGeometricBinomial(0.5, 0.2, 0, 5, 6)

#(b)
plot = function(lambda, p, k, m, n){
  x = k:m
  barplot(probabilities[1,], names.arg = x)
  barplot(probabilities[2,], names.arg = x)
  barplot(probabilities[3,], names.arg = x)
}
plot(0.5, 0.2, 0, 5, 6)

#(c)
determin_smallest_k0 = function(Y, lambda){
  k0 = Y
  limit = 1 - 10^(-6)
  while(ppois(k0, lambda) < limit){
    k0 = k0 + 1
  }
  print(paste("For lambda equal to",lambda ,"K0 is :", k0))
}
determin_smallest_k0(0, 10)

#A2
#(a) 
build_samples = function(){
  grades = read.csv(file="note_PS.csv", header = T)
  P = grades[['P']]
  S = grades[['S']]
  
  print("Absolute frequencies:")
  print(table(P))
  print(table(S))
  
  print("Relative frequencies:")
  print((as.vector(table(P))) / sum(as.vector(table(P))))
  print((as.vector(table(S))) / sum(as.vector(table(S))))
  
  print("Average frequency:")
  print(mean(as.vector(table(P))))
  print(mean(as.vector(table(S))))
  
  print("Average grades:")
  print(mean(P))
  print(mean(S))
}
build_samples()

#(b)
outlier_values = function(file_name, sample_name){
  grades = read.csv(file=file_name, header = T)
  sample = grades[[sample_name]]
  print("Sample:")
  print(sample)

  q_1 = as.vector(quantile(sample))[2]
  q_3 = as.vector(quantile(sample))[4]
  iqr = q_3 - q_1
  outliers = vector()
  j = 0
  
  for(i in 1:length(sample)) {
    if(sample[i] < q_1 - 1.5 * iqr | sample[i] > q_3 + 1.5 * iqr) {
      j = j + 1
      outliers[j] = sample[i]
    }
  }
  
  no_outlier_sample = subset(sample, !sample %in% outliers)
  
  print("Sample without outliers:")
  print(no_outlier_sample)
  
  breaks = seq(1, 10, by = 1)
  hist(no_outlier_sample, breaks = breaks)
}
outlier_values("note_PS.csv", "S")

