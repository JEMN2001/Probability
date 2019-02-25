Bernoulli <- function(p=0.5) {
  x = sample(0:2, 1)
  return((p**x)*((1-p)**(1-x)))
}

proba = runif(1)
print(paste("Con una probabilidad de ", proba, " Bernulli dio ", Bernoulli(proba)))