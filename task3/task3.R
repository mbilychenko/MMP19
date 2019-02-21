library(DEoptimR)


n=100
alpha0 <- runif(n, 0, 3)
alpha1 <- runif(n, 0, 1)
alpha2 <- runif(n, 0, 1)

p <- 15
k <- sample(c(rep(1,40),rep(2,20), rep(2.5, 20), rep(3, 20)))
l <- rep(1, n)
error_funct <- 1 #1 - abs,  2 -^2


funct_m <- function (x) {
    x<- floor(x)
    mp <- alpha0*k^alpha1*x^(alpha2-1)*alpha2
    w = (sum(x)+120)/30
    w_m <- rep(w, n)
    if (error_funct==1) {
        err <- sum(abs(mp*p-w_m))
    } else {
        err <- sum((mp*p-w_m)^2)
    }
    err
}

res <- JDEoptim(rep(0,n),
                rep(1000, n),
                fn = funct_m,
                tol = 1e-15, trace = TRUE, triter = 50)
result_l <- c(floor(res$par))
result_l

w <- (sum(result_l)+120)/30
l_total <- sum(result_l)

paste("The equilibrium wage will be", round(w, 2), "m.u.")

#






l <- result_l 
mp <- alpha0*k^alpha1*l^(alpha2-1)*alpha2
w = (sum(l)+120)/30
w_m <- rep(w, n)
err <- sum((mp*p-w_m)^2)
err
sqrt(err)

pressure_vessel_B <-
    list(obj = function(x) {
        x1 <- floor(x[1])*0.0625
        x2 <- floor(x[2])*0.0625
        x3 <- x[3]; x4 <- x[4]
        0.6224*x1*x3*x4 + 1.7781*x2*x3^2 +
            3.1611*x1^2*x4 + 19.84*x1^2*x3
    },
    con = function(x) {
        x1 <- floor(x[1])*0.0625
        x2 <- floor(x[2])*0.0625
        x3 <- x[3]; x4 <- x[4]
        c(0.0193*x3 - x1,
          0.00954*x3 - x2,
          750.0*1728.0 - pi*x3^2*x4 - 4/3*pi*x3^3)
    })
res <- JDEoptim(c( 18, 10, 0.0, 0.0),
                c(200+1, 200+1, 240.0, 240.0),
                fn = pressure_vessel_B$obj,
                constr = pressure_vessel_B$con,
                tol = 1e-7, trace = TRUE, triter = 50)
res
# Now convert to integer x1 and x2
c(floor(res$par[1:2]), res$par[3:4])
