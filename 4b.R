fact <- function(n) {
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

xval = c(0,0,0,0,0,0,0,0,0,0)

Binomial <- function(p=0.5, n=10) {
  pre = p
  prf = 1-p
  x = sample(1:n, 1)
  cmb = fact(n)/((fact(n-x)*fact(x)))
  pre = pre**x
  prf = prf**(n-x)
  xval[x] = xval[x]+1
  return(cmb*pre*prf)
}


for(i in 0:1000) 
{
  
  proba = runif(1)
  valp =Binomial(proba, 10)
  print(valp)

}

print(xval)


View(xval)
barplot(xval, one, main="Valor tomado", 
        xlab="Valor")

countstho = xval/1000
print(countstho)

View(countstho)
barplot(countstho, main="Valor tomado sobre mil", 
        xlab="Valor")