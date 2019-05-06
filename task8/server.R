library(markdown)
library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    model1 <- reactive({
        
      data <- read.csv2("data_task8.csv")
      
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
        
    })
    
    

    output$text1 <- renderText({ 
            tryCatch({
      
              
                print(paste("The average crime rate is "))
                
                #print(paste(dt$n[length(dt$period)], dt$n[1]*0.001))   
                
            }, warning = function(war) {

                # warning handler picks up where error was generated
                print("The problem has no solution. Change the settings, please.")
                
            }, error = function(err) {

                # error handler picks up where error was generated
                print("The problem has no solution. Change the settings, please.")
                
            })


    })
    
    output$table1 <- renderTable({
        
        dt <- model1()
        pos <- as.numeric(input$type_t)
        
        
        tb1 <- as.data.frame(dt[[pos]])
        tb1$Years <- as.character(seq(2004,2017,by = 1))
        
        tb1 <- tb1[c(4,1,2,3)]
        colnames(tb1) <- c("Рік", "Справжне значення", "Точка Лафера 1", "Точка Лафера 2")
        tb1

    }, digits = 3)
    

    
    
})

