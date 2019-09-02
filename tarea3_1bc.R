mean_x = 10;
mean_y = 5;
mean_z = mean_x + mean_y;

sd_x = 4;
sd_y = 3;
sd_z = 5;

X = rnorm(100000, mean_x, sd_x);
Y = rnorm(100000, mean_y, sd_y);

sums <- rep(0,100000)
sum = 0;
for (i in 1:100000) {
  sum = X[i] + Y[i];
  sums[i] <- sum;
  }

mean(sums)
var(sums)

#Ejecute para ver los valores#

#Valor experimental#  #Valor teórico#
quantile(sums, c(.01))#3.35#
quantile(sums, c(.25))#11.65#
quantile(sums, c(.5))#15#
quantile(sums, c(.75))#18.4#
quantile(sums, c(.99))#26.65#
