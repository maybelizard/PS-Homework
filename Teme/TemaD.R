#D1
confidence_intervals = function(sample_name, deviatie, confidence){
  note_studenti = read.csv(file=sample_name, header = T, stringsAsFactors = FALSE)
  
  alpha = 1 - confidence
  x_n = mean(as.numeric(note_studenti[, 1]))
  n = length(note_studenti[ ,1])
  sigma = sqrt(deviatie)
  crit_z = qnorm(1-(alpha/2), 0, 1)
  
  confidence_interval = c(x_n - crit_z * (sigma/sqrt(n)), x_n + crit_z * (sigma/sqrt(n)))
  print(paste("For confidence probability of", confidence, "the interval is:"))
  print(confidence_interval)
}

confidence_intervals("probabilitati.csv", 92.16, 0.95)
confidence_intervals("probabilitati.csv", 92.16, 0.99)

#D2
confidence_intervals_no_deviation = function(sample_name, confidence){
  note_studenti = read.csv(file=sample_name, header = T, stringsAsFactors = FALSE)
  
  alpha = 1 - confidence
  x_n = mean(mean(as.numeric(note_studenti[, 1])))
  n = length(note_studenti[ ,1])
  s = sd(note_studenti[ ,1])
  crit_t = qt(1-(alpha/2), n-1)
  
  confidence_interval = c(x_n - crit_t * (s/sqrt(n)), x_n + crit_t * (s/sqrt(n)))
  print(paste("For confidence probability of", confidence, "the interval is:"))
  print(confidence_interval)
}

confidence_intervals_no_deviation("statistica.csv", 0.95)
confidence_intervals_no_deviation("statistica.csv", 0.99)

#D3
determine_hypothesis_homeworks = function(semnificatie) {
  #null hypothesis proportia de studenti care pot rezolva temele e cel mult 85%
  p0 = 0.85
  #alternative hypothesis proportia de studenti care pot rezolva temele e mai mult ca 85%
  n = 100
  prob_observata = 0.86
  z = (prob_observata - p0)/sqrt((p0*(1-p0))/n)
  z_critica = qnorm(1 - semnificatie/2, 0, 1)
  if(abs(z) > abs(z_critica)){
    print("Ipoteza nula este respinsa!")
  }
  else{
    print("Nu exista suficiente dovezi pentru a respinge ipoteza nula sau pt a accepta ipoteza alternativa!")
  }
  
}
determine_hypothesis_homeworks(0.01)
determine_hypothesis_homeworks(0.05)