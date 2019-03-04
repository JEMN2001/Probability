size = 15

fact <- function(n) {
  if (n > 0) {
    out = 1
    for(i in 1:n) {
      out = out*i
    }
    return(out)
  }
  else {fact <- function(n) {
    if (n > 0) {
      out = 1
      for(i in 1:n) {
        out = out*i
      }
      return(out)
    }
    else {
      return(1)
    }
  }
    return(1)
  }
}

Binomial <- function(p=0.5, n=10) {
  pre = p
  prf = 1-p
  x = sample(1:n, 1)
  cmb = fact(n)/((fact(n-x)*fact(x)))
  pre = pre**x
  prf = prf**(n-x)
  return(cmb*pre*prf)
}

proba <- runif(1)
xpos <- rep(0, size)
xval <- rep(0, size)
for(i in 1:size) {
  pre = proba
  prf = 1-proba
  cmb <- fact(size)/((fact(size-i)*fact(i)))
  pre <- pre**i
  prf <- prf**(size-i)
  xpos[i] <- cmb*pre*prf
}

print(xpos)

for(i in 0:1000) 
{
  test = Binomial(proba, size);
  for (j in 1:size) {
    tmp = xpos[j]
    if (test == tmp) {
      xval[j] = xval[j]+1
      break;
    }
  }
}

print(xval)


View(xval)
barplot(xval, 1, main="Valor tomado", 
        xlab="Valor")

countstho = xval/1000
print(countstho)

View(countstho)
barplot(countstho, main="Valor tomado sobre mil", 
        xlab="Valor")