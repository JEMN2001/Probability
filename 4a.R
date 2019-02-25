Bernoulli <- function(p=0.5) {
  x = sample(0:2, 1)
  return((p**x)*((1-p)**(1-x)))
}

#proba = runif(1)
#print(paste("Con una probabilidad de ", proba, " Bernulli dio ", Bernoulli(proba)))

zero = 0
one = 0
for(i in 0:1000) 
  {
  proba = runif(1)
  valp = Bernoulli(proba)
  print(valp)
  if(valp == proba) 
  {
    one = one + 1
  }
  else if (valp == 1 - proba)
  {
    zero = zero + 1
  }
}

print(zero)
print(one)

counts <- c(one, zero)

View(counts)
barplot(counts, one, main="Valor tomado", 
        xlab="Valor")

countstho <- c(one/1000, zero/1000)

View(countstho)
barplot(countstho, one, main="Valor tomado sobre mil", 
        xlab="Valor")
