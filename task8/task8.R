data <- read.csv2("C:/Users/bilyc/Desktop/stav/data_task8.csv")

X <- data$X*10^6
K <- data$K*10^6
L <- data$L*10^3
T1 <- data$T1*10^6
T2 <- data$T2*10^6
T3 <- data$T3*10^6
T4 <- data$T4*10^6
t <- 1:length(X)

logK <- log(K)
logL <- log(L)

thetas <- list(NA)

for (i in 1:4) {
  T_ch <- switch(i,T1, T2, T3, T4)
  
  theta <- T_ch/X
  
  fit1 <- lm(X~t+I(theta*logK)+I(theta^2*logK)+I(theta*logL)+I(theta^2*logL))
  summary(fit1)
  a <- coef(fit1)[3]
  b <- coef(fit1)[4]
  n <- coef(fit1)[5]
  m <- coef(fit1)[6]
  theta_st <- -1/2*(a*logK+n*logL)/(b*logK+m*logL)
  theta_st2 <- 1/4*(sqrt(a*logK+n*logL^2-8*b*logK+m*logL)-a*logK-n*logL)/(b*logK+m*logL)
  #theta_st2_2 <- 1/4**((-1)*sqrt(a*logK+n*logL^2-8*b*logK+m*logL)-a*logK-n*logL)/(b*logK+m*logL)
  
  
  df_thetas <- data.frame(theta, theta_st, theta_st2)
  
  thetas[[i]] <- df_thetas
}

thetas


dt <- thetas

pos = 1
tb1 <- as.data.frame(dt[[pos]])
tb1$Years <- as.character(seq(2004,2017,by = 1))

tb1 <- tb1[c(4,1,2,3)]
colnames(tb1) <- c("Рік", "Справжне значення", "Точка Лафера 1", "Точка Лафера 2")
tb1
