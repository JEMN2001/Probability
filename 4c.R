size = 15

Geometrica <- function(p=0.5, x){
  prf = 1-p
  pre = p;
  
  prf = prf**(x-1)
  
  return(prf*pre)
}

proba <- runif(1)
xval <- rep(0, size)
print(xval)

for (i in 1:1000) {
  x = sample(0:size, 1)
  #print(Geometrica(proba, x))
  xval[x] = xval[x]+1
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