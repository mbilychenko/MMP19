library(dplyr)


N = 2540

#vibor
w <- rep(NA, 6)
p_goods <- 50
w[1] <- 30
w[2] <- 40
w[3] <- 50
w[4] <- 60

t_profit <- 0.25
t_wage <- 0.13


t_oborot <- rep(NA,4)
t_oborot[1] <- 1
t_oborot[2] <- 2
t_oborot[3] <- 2
t_oborot[4] <- 2
check_t = 1
check_kartel = 1
check_zao = 1


if (check_zao == 1) {
    mrc = 0.8 # vvod danyh
} else {
    mrc = 1
}


profit_res <- matrix(NA, nrow = 2540, ncol = 12)
for (i in 1:N){

    #optim
    L <- rep(NA, 6) #1 - monop, 2-4 - firmi, 5- goverm, 6 - unempl
    K <- rep(NA, 4) #1-3- firm
    TR <- rep(NA, 4) #1 - monop, 2-4 - firm
    TC <- rep(NA, 4) #1 - monop, 2-4 - firm
    profit <- rep(NA, 4)
    
    L[1] <- i
    
    #1 - monop
    p_K <- (w[1]*L[1]+2100)/(12*L[1])
    Q_m <- 12*L[1]
    K[1] <- Q_m
    K[2] <- Q_m/3
    K[3] <- Q_m/3
    K[4] <- Q_m/3
    FC_m <- 2100
    TR[1] <- p_K*Q_m
    TC[1] <- w[1]*L[1]+2100
    profit[1] <- TR[1] - TC[1]
    
    #2 - firm1
    
    L[2] <- floor(p_K*K[2]/(2*w[2]))
    Q_1 <- K[2]*sqrt(L[2])
    TR[2] <- Q_1*p_goods
    TC[2] <- p_K*K[2]+w[2]*L[2]
    profit[2] <- TR[2]-TC[2]
    
    #3 - firm2
    
    L[3] <- floor(2*p_K*K[3]/(w[3]))
    Q_2 <- sqrt(K[3])*(L[3])
    TR[3] <- Q_2*p_goods
    TC[3] <- p_K*K[3]+w[3]*L[3]
    profit[3] <- TR[3]-TC[3]
    
    #4 - firm3
    
    
    L[4] <- floor(p_K*K[4]/(w[4]))
    Q_3 <- 2*sqrt(K[4]*L[4])
    TR[4] <- Q_3*p_goods
    TC[4] <- p_K*K[4]+w[4]*L[4]
    profit[4] <- TR[4]-TC[4]
    
    
    #5 - goverm
    L[5] <- 100
    w[5] <- 750
    
    #6 - unemp
    L[6] <- N-sum(L[1:5])
    w[6] <- 150
    
    #L
    #K
    #profit
    
    profit_res[i,1] <- i
    profit_res[i,2:5] <- profit
    profit_res[i,6:11] <- L 
    profit_res[i,12] <- sum(profit_res[i,2:5]) 
}


profit_res <- data.frame(profit_res)
colnames(profit_res) <- c("Index", "Pr_m","Pr1", "Pr2", "Pr3", "Lm", "L1", "L2", "L3", "L_gov", "L_unem", "Pr_total")

if (check_kartel == 1) {
    i_pr_max <- profit_res %>% filter(L_unem >= 0) %>% summarize(max(Pr_total))
    index_optim <- as.numeric(profit_res %>% filter(Pr_total == as.numeric(i_pr_max)) %>% select(Index))
} else {
    i_pr_max <- profit_res %>% filter(L_unem >= 0) %>% summarize(max(Pr1))
    index_optim <- as.numeric(profit_res %>% filter(Pr1 == as.numeric(i_pr_max)) %>% select(Index))
}





###             Results             ###


#optim
L <- rep(NA, 6) #1 - monop, 2-4 - firmi, 5- goverm, 6 - unempl
K <- rep(NA, 4) #1-3- firm
TR <- rep(NA, 4) #1 - monop, 2-4 - firm
TC <- rep(NA, 4) #1 - monop, 2-4 - firm
profit <- rep(NA, 4)

L[1] <- index_optim

#1 - monop
p_K <- (w[1]*L[1]+2100)/(12*L[1])
Q_m <- 12*L[1]
K[1] <- Q_m
K[2] <- Q_m/3
K[3] <- Q_m/3
K[4] <- Q_m/3
FC_m <- 2100
TR[1] <- p_K*Q_m
TC[1] <- w[1]*L[1]+2100
profit[1] <- TR[1] - TC[1]

#2 - firm1
L[2] <- floor(p_K*K[2]/(2*w[2]))
Q_1 <- K[2]*sqrt(L[2])
TR[2] <- Q_1*p_goods
TC[2] <- p_K*K[2]+w[2]*L[2]
profit[2] <- TR[2]-TC[2]



#3 - firm2
L[3] <- floor(2*p_K*K[3]/(w[3]))
Q_2 <- sqrt(K[3])*(L[3])
TR[3] <- Q_2*p_goods
TC[3] <- p_K*K[3]+w[3]*L[3]
profit[3] <- TR[3]-TC[3]

#4 - firm3
L[4] <- floor(p_K*K[4]/(w[4]))
Q_3 <- 2*sqrt(K[4]*L[4])
TR[4] <- Q_3*p_goods
TC[4] <- p_K*K[4]+w[4]*L[4]
profit[4] <- TR[4]-TC[4]


#5 - goverm
L[5] <- 100
w[5] <- 750

#6 - unemp
L[6] <- N-sum(L[1:5])
w[6] <- 150


if (check_t == 1) {
    budget_rev <- Q_m*t_oborot[1] + Q_1*t_oborot[2] + Q_2*t_oborot[3] + Q_3*t_oborot[4] +
        L[1]*w[1]*(mrc)*t_wage + L[2]*w[2]*(mrc)*t_wage + L[3]*w[3]*(mrc)*t_wage + L[4]*w[4]*(mrc)*t_wage 
} else {
    budget_rev <- profit[2]*t_profit + profit[3]*t_profit + profit[4]*t_profit +
        L[1]*w[1]*(mrc)*t_wage + L[2]*w[2]*(mrc)*t_wage + L[3]*w[3]*(mrc)*t_wage + L[4]*w[4]*(mrc)*t_wage 
}

budget_cost <- L[5]*w[5] + L[6]*w[6]
budget <- budget_rev - budget_cost

res_shiny <- data.frame(t(L), budget)



print(paste("Labor: monopoly ", res_shiny[1], ", firm 1 - ", res_shiny[2], ", firm 2 - ", res_shiny[3],
      ", firm 3 - ", res_shiny[4], ", governmnet - ", res_shiny[5], ", unemployment - ", res_shiny[6], ".", sep = ""))
print(paste("Budget: ", round(res_shiny[7]), ".",sep = ""))





