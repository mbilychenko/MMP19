library(markdown)
library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    model1 <- reactive({
        
        f = as.numeric(input$f)
        p = as.numeric(input$p)/100
        b = as.numeric(input$b)/100
        
        choise_f_adj = as.numeric(input$choise_f_adj)
        choise_p_adj = as.numeric(input$choise_p_adj)
        
        
        n = 30000
        first_group <- floor(n/3)
        second_group <- floor(2*n/3)
        
        wage_of <- rep(NA, n)
        wage_unof <- rep(NA, n)
        income <- 0
        u <- rep(NA, n)
        
        fin_budg <- rep(0, 1000)
        crime_level <- rep(NA, 1000)
        temp_f<- rep(NA, 1000)
        
        set.seed(7)
        ss <- sample(1:3,size=n,replace=TRUE,prob=c(0.33,0.33,0.34))
        
        wage_of[ss==1] <- 1400
        wage_of[ss==2] <- 2100
        wage_of[ss==3] <- 4500
        wage_unof[ss==1] <- 5200
        wage_unof[ss==2] <- 10000
        wage_unof[ss==3] <- 35000
        
        temp_pos_crime = 0
        
        withProgress(message = "Побудова розв'язку", value = 0, {
            n_period = 1000    
            for (i in 1:n_period){
                
                u[1:first_group] <- (wage_unof[1:first_group]-p*f)
                u[(first_group+1):second_group] <- (wage_unof[(first_group+1):second_group]-p*f)^2
                u[(second_group+1):n] <- (wage_unof[(second_group+1):n]-p*f)
                
                choice <- ifelse(income < f, 0, ifelse(u>0, 1,0))
                
                crime_level[i] <- sum(choice)/n
                
                income <- income + wage_of*0.5 + wage_unof*choice
                
                #blok of adjustment
                n_crime <- sum(choice)
                if(n_crime >0){
                    temp_pos_crime <- temp_pos_crime + 1
                    number_crime <- which(choice==1)
                    crime_position_arest <- sample(number_crime,floor(p*n_crime), replace = FALSE)
                    income[crime_position_arest] <- income[crime_position_arest] - f
                    
                    fin_t <- sum(wage_of*b)
                    fin_f <- length(crime_position_arest)*f
                    fin_budg[i] <- fin_t+ fin_f + ifelse(i>1,fin_budg[i-1],0)
                    
                    if(choise_f_adj == 1) {
                        if (temp_pos_crime >50) {
                            avg_crime_rate_temp <- crime_level[i]#mean(crime_level[1:i])
                            
                            if (avg_crime_rate_temp > 0.05) {
                                f = f*fin_budg[i]/fin_budg[i-1]
                            }
                            
                        }
                    }
                    
                    if(choise_p_adj == 1) {
                        if (temp_pos_crime >50) {
                            avg_crime_rate_temp <- crime_level[i]#mean(crime_level[1:i])
                            
                            if (avg_crime_rate_temp > 0.05) {
                                p = p + ifelse(0.001*fin_budg[i]/fin_budg[i-1]>0.1,0.1,0.001*fin_budg[i]/fin_budg[i-1]) 
                            }
                            
                            if(p>1) {p=1}
                        }
                    }
                    
                    
                }
                incProgress(1/n_period, detail = paste("Розрахунок періоду ", i))
    
            } 
            
        })
        
        dt <- data.frame(period = 1:1000, crime_level)
        
    })
    
    

    output$text1 <- renderText({ 
            tryCatch({

                
                dt <- model1()
      
                avg_crime_rate <- mean(dt$crime_level)
                
                print(paste("The average crime rate is ", round(avg_crime_rate*100, 2), "%"))
                
                #print(paste(dt$n[length(dt$period)], dt$n[1]*0.001))   
                
            }, warning = function(war) {

                # warning handler picks up where error was generated
                print("The problem has no solution. Change the settings, please.")
                
            }, error = function(err) {

                # error handler picks up where error was generated
                print("The problem has no solution. Change the settings, please.")
                
            })


    })
    
    output$plot1 <- renderPlot({
        
        dt <- model1()
        
            g1 <- ggplot(dt, aes(x = period, y = crime_level*100))
            g1  + geom_line() +
                labs(x = "Період",y = "Рівень злочинності, %") + 
                geom_smooth(method = "loess")


    })
    

    
    
})

