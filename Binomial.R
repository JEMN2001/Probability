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

Binomial <- function(p=0.5, n, x) {
  pre = p
  prf = 1-p
  cmb = fact(n)/((fact(n-x)*fact(x)))
  pre = pre**x
  prf = prf**(n-x)
  return(cmb*pre*prf)
}

num = as.integer(readline(prompt="Ingrese el numero de intentos totales: "))
x = as.integer(readline(prompt="Ingrese las veces que desea tener un exito: "))
p = runif(1)

print(paste("La probabilidad binomial con p =", p, " es ", Binomial(p, num, x)))
  