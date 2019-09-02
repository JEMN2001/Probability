genSn <- function(n){
  Sn = 0
  for (i in 0:n) {
    x <- runif(1)
    y <- runif(1)
    if (0 <= cos(pi*x)+sin(pi*y) && cos(pi*x)+sin(pi*y)<= 1)
      Sn = Sn+1
  }
  return(Sn/n)
}

Test <- genSn(90000)
print(Test)