genSn <- function(n){
  Sn = 0
  for (i in 0:n) {
    x <- runif(1)
    y <- runif(1)
    if ((x-0.5)*(x-0.5)+(y-0.5)*(y-0.5) <= 1/4)
      Sn = Sn+1
  }
  return(Sn/n)
}

Test <- genSn(90000)
print(4*Test)