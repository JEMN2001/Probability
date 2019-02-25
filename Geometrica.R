Geometrica <- function(p=0.5, x){
  prf = 1-p
  pre = p;
  
  prf = prf**(x-1)
  
  return(prf*pre)
}

x = as.integer(readline(prompt="Ingrese al intento en que desea obtener el éxito: "))
p = runif(1)

print(paste("La probabilidad geometrica con p =", p, " es ", Geometrica(p, x)))