

library(ggplot2)
library(dplyr)
library(tidyr)
library(markdown)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    model1 <- reactive({
        #input$showTable = FALSE

        #user parameters
        task_ch = input$showtask_ch #1 - adjusted, 0  - const
        m = as.numeric(input$m)
        n0 = as.numeric(input$n0)
        growth_gamma = input$growth_gamma/10000
        alpha_first = input$alpha_first/100
        beta_first = input$beta_first/100
        period_sleep = 10 #input$period_sleep
        
        #control parameters
    
        n_check <- 0
        n_period_cycle = m
        alpha_rate = 0.01
        beta_rate = 0.01
        birth_rate = 0.03
        dead_rate = 0.01
        i_vnes = as.numeric(input$fund_base)/(n0^2)
        
        
        #initialization
        alpha = rep(NA, m)
        beta = rep(NA, m)
        n = rep(NA, m)
        n_dead_vir = rep(NA, m)
        n_born = rep(NA, m)
        n_dead = rep(NA, m)
        gamma = rep(NA, m)
        fund = rep(NA, m) 
        
        
        
        #first period
        fund[1] <- as.numeric(input$fund_base)
        alpha[1] <- alpha_first
        beta[1] <- beta_first
        gamma[1] <- 0.00
        n_dead_vir[1] <- round(alpha[1]*beta[1]*n0, 0)
        n_born[1] <- rpois(1, round(n0*0.05))
        n_dead[1] <- rpois(1, round(n0*0.01))
        n[1] <- n0 - n_dead_vir[1] + n_born[1] - n_dead[1] 
        gamma_smooth = period_sleep-1
        
        #i = 2
        for (i in 2:m) {
            alpha[i] <- alpha[i-1]*(1 + alpha_rate)
            beta[i] <- beta[i-1]*(1 + beta_rate)
            n_dead_vir[i] <- round(alpha[i]*beta[i]*n[i-1], 0)
            n_born[i] <- rpois(1, round(n[i-1]*birth_rate))
            n_dead[i] <- rpois(1, round(n[i-1]*dead_rate))
            n[i] <- n[i-1] - n_dead_vir[i] + n_born[i] - n_dead[i] 
            
            if (i<period_sleep) {
                gamma[i] = gamma[i-1]
                I_jertva = sum(gamma[i]*n[i-1]*i_vnes)
                fund[i] = fund[i-1]+I_jertva
                next
            }
            
            
            if (((n_dead_vir[i] < n_dead_vir[i-1]) | (n_check == 1)) & (task_ch == 1)) {
                
                
                gamma[i] = gamma[i-1]+growth_gamma
                if (n_check == 0) {opt_gamma = gamma[i]}
                n_check = 1
                
                
                #+ (n_dead_vir[i-1]/n_dead_vir[i-25])^(1/100)-1 
                
                #print(paste(i, "a"))
            } else {
                k1 = n[i-1]/n[i-gamma_smooth]
                k2 = n_dead_vir[i-1]/n_dead_vir[i-gamma_smooth]
                k_gr = sign(k2-1)*(abs(k2-1))^gamma_smooth
                #k_gr = sign(1-k1)*(abs(1-k1))^(n[i-1]/n0)+(k2-1)
                gamma[i] = gamma[i-1] + k_gr
                #print(paste(i, "bb"))
            } 
            
            #from 10 period gamma = base + n_dead_vir[i-1] /n_dead_vir[i-9]-1
            
            if (gamma[i]<0) gamma[i] = 0
            if (gamma[i]>1) gamma[i] = 1 
            
            I_jertva = sum(gamma[i]*n[i-1]*1000)
            fund[i] = fund[i-1]+I_jertva
            k = (fund[i]/fund[i-1]-1)
            
            alpha[i] = alpha[i]/(1+2*k) #(if((1-k*2) < 0) 0.0001 else (1-k*2))
            beta[i] = beta[i]/(1+5*k)#(if((1-k*5) < 0) 0.0001 else (1-k*5))
            
            
            #exit
            if (n[i]<=n0*0.001) {
                #print("The city is dead")
                n_period_cycle = i
                break
            }
            
            if (n_dead_vir[i] == 0) {
                #print("The city is healthy")
                n_period_cycle = i
                break
                
            }
            
        }

        
            data_sim <- data.frame(n, n_dead_vir, fund, gamma)
            data_sim <- data_sim[1:n_period_cycle,]
            data_sim$period <- 1:(dim(data_sim)[1])
            data_sim
    })
    
    
    output$table1 <- renderDataTable({

        dt <- model1()
        

            if(input$showTable) {
                as.data.frame(dt)
            }

    })
    
    output$text1 <- renderPrint({ 
            tryCatch({

                
                dt <- model1()
      
            
                if ((dt$period[length(dt$period)] < as.numeric(input$m) & dt$n[length(dt$period)]>dt$n[1]*0.002)) {
                    #x <- dt$n_dead_vir[2:length(dt$period)]-dt$n_dead_vir[1:(length(dt$period)-1)]
                    #opt_gamma_pos <- which(x < 0 )[1]+1
                    #opt_gamma <- dt$gamma[opt_gamma_pos]
                    opt_gamma <- dt$gamma[length(dt$period)]
                    
                    print(paste("Країна подолає хворобу за ", dt$period[length(dt$period)], " періодів. 
                                Оптимільна частка витрат становить ", round(opt_gamma*100, 2), "%.", sep = ""))
                } else if ((dt$period[length(dt$period)] < as.numeric(input$m) & dt$n[length(dt$period)]<dt$n[1]*0.002)){
                    print(paste("Населення країни загине через ", dt$period[length(dt$period)], " періодів.", sep = ""))
                } else {
                    print("Не вистачає обчислювальних можливостей для подолання вірусу за визначений період.")
                }
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
        if(input$showPlot_n) {
            g <- ggplot(dt, aes(x = period, y = n_dead_vir, col = gamma*100)) 
            g + geom_line(size = 2) + 
                labs(x = "Період", y = "Кількість людей, що загинула від вірусу", col='Частка витрат\nна дослідження, %') + 
                theme_classic() + # + annotate("rect", xmin=0, xmax=10, ymin=0, ymax=Inf, alpha=0.2, fill="red") 
                #theme(legend.position = c(0.8, 0.8)) + 
                scale_colour_gradient(low="blue", high="red")

        }
    })
    
    output$plot2 <- renderPlot({
        dt <- model1()
        
        if(input$showPlot_market) {
            g1 <- ggplot(dt, aes(x = period, y = n, col = gamma*100)) 
            g1 + geom_line(size = 2) + 
                labs(x = "Період", y = "Населення країни", col='Частка витрат\nна дослідження, %') + 
                theme_classic() +# + annotate("rect", xmin=0, xmax=10, ymin=0, ymax=Inf, alpha=0.2, fill="red") 
                #theme(legend.position = c(0.8, 0.8)) + 
                scale_colour_gradient(low="blue", high="red")
        
        }
    })    
    
    
    
    
})

