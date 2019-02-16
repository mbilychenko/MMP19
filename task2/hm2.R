##### Ideas

#+1. Dobavit Hiksa (reshat neliney yrabnenie)
#+2. Dobavit perecklychatel
3. mozet inplementazia podhoda 2 - c dohodom otdelno esche odin temp2
4. Graphics in ggplot2
5. Shiny

##### New
set.seed(124)
library(ggplot2)
library(tidyr)

#read parameters from user
m <- 100 +1 #number of periods
n <- 10 #number of people
p1_start <- 10
p2_start <- 15
r <- 0.01
bmin <- 100
bmax <- 1000
p1_check <- 1
p2_check <- 0
check_method = 1  #1 -slyckiy, 2 - hicks

#initialization
##alpha's
alpha1 <- runif(n)
alpha2 <- 1 - alpha1
##prices
p1 <- rep(NA, m)
p2 <- rep(NA, m)
p1[1] <- p1_start
p2[1] <- p2_start
##goods/utilies
#m - periods, n - people
x1 <- matrix(NA, m, n)
x2 <- matrix(NA, m, n)
u <- matrix(NA, m, n)
##incaome
b <- matrix(NA, m, n)
b[1, ] <- runif(n, min = bmin, max = bmax)
b[2, ] <- b[1, ]
compensation <- rep(NA, m)
#temp
b_temp <- rep(NA, n)
x1_temp <- rep(NA, n)
x2_temp <- rep(NA, n)


#zero(first) period - the base equilibrium
for (j in 1:n) {
  x1[1, j] <- (alpha1[j]/alpha2[j])*b[1, j]/((alpha1[j]/alpha2[j]+1)*p1[1])
  x2[1, j] <- b[1, j]/((alpha1[j]/alpha2[j]+1)*p2[1])
  u[1, j] <- x1[1, j]^(alpha1[j])*x2[1, j]^(alpha2[j])
}
compensation[1] <- 0


#next periods
for (i in 2:m){
    
  #price increases
  p1[i] <- p1[i-1]*(1+r*p1_check)
  p2[i] <- p2[i-1]*(1+r*p2_check)
  
  #new equlibrium - e2
  for (j in 1:n) {
    x1[i, j] <- (alpha1[j]/alpha2[j])*b[i, j]/((alpha1[j]/alpha2[j]+1)*p1[i])
    x2[i, j] <- b[i, j]/((alpha1[j]/alpha2[j]+1)*p2[i])
    u[i, j] <- x1[i, j]^(alpha1[j])*x2[i, j]^(alpha2[j])
  }
  
  if (check_method == 1) {
    #ef dojoda - e3 (slyckiy)
    for (j in 1:n) {
      b_temp[j] <- p1[i]*x1[i-1, j] + p2[i-1]*x2[i-1, j]
      x1_temp[j] <- (alpha1[j]/alpha2[j])*b_temp[j]/((alpha1[j]/alpha2[j]+1)*p1[i])
      x2_temp[j] <- b_temp[j]/((alpha1[j]/alpha2[j]+1)*p2[i])
      #u[i, j] <- x1[i, j]^(alpha1[j])*x2[i, j]^(alpha2[j])
    }
  }
  
  if (check_method == 2) {
    #ef dojoda - e3 (hicks)
    for (j in 1:n) {
      target <- function(x, p1_n = p1[i], p2_n = p2[i], u_n = u[i-1, j], b_n = b[i, j], alpha1_n = alpha1[j], alpha2_n = alpha2[j])
      {
        y <- ((b_n - p2_n*x)/p1_n)^alpha1_n*x^alpha2_n - u_n
        y
      }
      xstart <- x2[i, j]
      res <- nleqslv(xstart, target, control=list(btol=.01),jacobian=TRUE,method="Newton")
      x2_temp[j] <- res$x
      x1_temp[j] <- (b[i, j] - p2[i]*x2_temp[j])/p1[i]
    }
  }
  
  ef_dohody <- abs(x1[i,]- x1_temp)
  compensation[i] <- compensation[i-1]+sum(ef_dohody)
  
  if (i==m) break
  b[i+1, ] <- b[i, ] + ef_dohody
  
}



#x1
#x2

x1_total_t <- apply(x1, 1, sum)
x2_total_t <- apply(x2, 1, sum)
dt <- data.frame(x1 = x1_total_t, x2 = x2_total_t, p1, p2, compensation, period = 1:length(p1))



plot(x1_total_t, type = "l", ylim = c(min(x1_total_t, x2_total_t),max(x1_total_t, x2_total_t)))
lines(x2_total_t, col ="red")
legend("topright", legend = c("x1", "x2"), fill = c("black", "red"))
g1 <- ggplot(dt, aes(x = period, y = x1, col = p1)) 
g1 + geom_line()

#15
temp1 <- dt[,c(1,2,6)]
temp1_1 <- gather(temp1, key = "tp", value = "meas", -period)
temp2 <- dt[,c(3,4,6)]
temp2_1 <- gather(temp2, key = "tp", value = "meas", -period)


g2 <- ggplot()
ggplot() + 
  geom_line(aes(x = period, y = x1, colour = p1), dt) +
  geom_line(aes(period, x2, colour = p2), dt) +
  labs(x = "Period", y = "Obsag sposhivannya", colour = "Price") +
  theme_classic() 



i_lasp <- (x1_total_t[1:(m-1)]*p1[2:m]+x2_total_t[1:(m-1)]*p2[2:m])/(x1_total_t[1:(m-1)]*p1[1:(m-1)]+x2_total_t[1:(m-1)]*p2[1:(m-1)])
i_pashe <- (x1_total_t[2:m]*p1[2:m]+x2_total_t[2:m]*p2[2:m])/(x1_total_t[2:m]*p1[1:(m-1)]+x2_total_t[2:m]*p2[1:(m-1)])
i_fisher <- sqrt(i_lasp*i_pashe)
plot(i_lasp, type = "l", ylim = c(min(i_fisher, i_lasp, i_pashe), max(i_fisher, i_lasp, i_pashe)))
lines(i_pashe, col = "red")
lines(i_fisher, col = "green")
legend("topright", legend = c("Lasp", "Paashe", "Fisher"), fill = c("black", "red", "green"))

temp3 <- data.frame(i_lasp, i_pashe, i_fisher, period = 1:length(i_fisher))
temp3_1 <- gather(temp3, key = "type", value = measurment, -period)
g3 <- ggplot(temp3_1, aes(x = period, y = measurment, col = type))
g3 + geom_line() +
  labs(x = "Period", y = "Index") +
  theme_classic() +
  guides(col = guide_legend(title = "Index")) +
  scale_color_manual(labels = c("Laspeyras", "Paashe", "Fisher"), values = c("red", "blue", "green"))




plot(compensation[2:m], type = "l")

g4 <- ggplot(dt, aes(x = period, y = compensation))
g4 + geom_area(fill = "blue") +
  labs(x = "Period", y = "Obsag Compensarcii") +
  theme_classic()





#variant 1: period 2 - nahodim E2, chitaem dohoda efect, period 3 - podnimaem zeny, dobovlyaem dohod - oba vnacahale, chitaem E3, potom efect dohoda
#var 2: period 2 - analog, period 3 - dohod dopablayem, chitaem E3_0, podnimaem zeny, chitaem E3, chitaem efect dohoda, 

