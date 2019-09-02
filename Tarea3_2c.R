p = .2;
q = .1;
N = rgeom(100000,q);
sums <- rep(0,100000)
for (i in 1:length(N)) {
  sum = 0;
  for (j in 1:N[i]) {
    sum = sum + rgeom(1,p);
  }
  sums[i] <- sum;
}

p_teo = rep(0,99);
p_emp = rep(0,99);
dif = rep(0,99);
for (i in 1:length(sums)) {
  if (sums[i] <= 99) {
    p_emp[sums[i]] = p_emp[sums[i]]+1
  }
}
for (i in 1:99) {
  p_teo[i] = ((1-p*q)^(i-1))*p*q;
  p_emp[i] = p_emp[i]/100000.0;
  dif[i] = abs(p_teo[i]-p_emp[i]);
}
