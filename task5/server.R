library(markdown)
library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    model1 <- reactive({
        
        #other control parametrs
        x_p_a <- as.numeric(input$x_p_a) #price 1 chorhih 
        x_p_b <- as.numeric(input$x_p_b) #price 1 roz
        y_p <- as.numeric(input$y_p) #price 1 china
        
        #lider stradegy
        check_x_mark = as.numeric(input$check_x_mark) #chto delat esli pribil padayet
        check_cost_mark = as.numeric(input$check_cost_mark)
        check_x_factory = as.numeric(input$check_x_factory) #checkbox 0/1
        check_n_x_new_a = as.numeric(input$check_n_x_new_a)
        check_n_x_new_b = as.numeric(input$check_n_x_new_b)
        check_x_price = as.numeric(input$check_x_price) #0.1
        check_x_price_incr <- as.numeric(input$check_x_price_incr)/100
        
         #china strategy
        check_y = as.numeric(input$check_y)
        check_y_factory = as.numeric(input$check_y_factory)
        
  
        
        
        ### initialization
        n_years <- 25 
        t <- 12*n_years #kilkist periodiv
        
        #vse kto hochet kypit v mesac
        n_total <- rep(NA, t)
        n_total_x <- rep(NA, t)
        n_total_x_a <- rep(NA, t)
        n_total_x_b <- rep(NA, t)
        n_total_y <- rep(NA, t)
        
        
        x_inf <- 0.06
        x_a_1z <- 17*10^6/24 #kilkist chornih gadget from 1 zavod
        x_b_1z <- 16*10^6/24 #k-st rozevih gadget from 1 zavod
        n_x_work <- rep(NA, t) #kilkist diychih zavodiv
        n_x_work_a <- rep(NA, t) #kilkist diychih zavodiv dlya chiorhih
        n_x_work_b <- rep(NA, t) #kilkist diychih zavodiv dlya ros
        n_x_new <- rep(NA, t) #kilkist novih zavodiv y preiod t
        n_x_new_a <- rep(NA, t) #kilkist novih zavodiv y dla chornih preiod t
        n_x_new_b <- rep(NA, t) #kilkist novih zavodiv y dla ros preiod t
        #n_x_work[7] <- n_x_work[6] + n_x_new[1] ==t-6
        c_x_work <- 165.4*10^6 #vartist ekspl 1 zavody v rik (nachalna)
        c_x_new <- 165.4*10^6 #vartist bydiv 1 novogo zavody
        
        TC_x <- rep(NA, t)    
        TR_x <- rep(NA, t)
        profit_x <- rep(NA, t)
        marketing_cost_x <- rep(NA, t)
        
        x_a_product <- rep(NA,t)  #obsee vosmoznosti proizvodsva v mesac
        q_x_a <- rep(NA, t)
        x_b_product <- rep(NA,t)  #obsee vosmoznosti proizvodsva v mesac
        q_x_b <- rep(NA, t)
        
        perc_x_increase_market <- rep(NA, t)
        
        
        #china
        y_inf <- 0.046
        y_1z <- 10*10^6/24 #kilkist gadget from 1 zavod
        
        n_y_work <- rep(NA, t) #kilkist diychih zavodiv
        n_y_new <- rep(NA, t) #kilkist novih zavodiv y preiod t
        
        c_y_work <- 97.5*10^6 #vartist ekspl 1 zavody v rik (nachalna)
        c_y_new <- 97.5*10^6 #vartist bydiv 1 novogo zavody
        
        y_product <- rep(NA,t)  #obsee vosmoznosti proizvodsva v mesac
        q_y <- rep(NA, t)
        
        TC_y <- rep(NA, t)    
        TR_y <- rep(NA, t)
        profit_y <- rep(NA, t)
        

        
        #first period
        
        x_p <- (x_p_a + x_p_b)/2
        n_total[1] <- 0.06*7*10^9 
        n_total_x[1] <- n_total[1]*(1-(1-y_p/x_p)^2)
        n_total_y[1] <- n_total[1]*(1-y_p/x_p)^2
        n_total_x_a[1] <- n_total_x[1]*0.6
        n_total_x_b[1] <- n_total_x[1]*(1-0.6)
        
        #first period X
        n_x_work_a[1] <- 10
        n_x_work_b[1] <- 5
        n_x_new_a[1] <- 0
        n_x_new_b[1] <- 0
        
        n_x_work[1] <- n_x_work_a[1] + n_x_work_b[1] 
        n_x_new[1] <- n_x_new_a[1] + n_x_new_b[1]
        
        x_a_product[1] <- x_a_1z*n_x_work_a[1]
        q_x_a[1] <- min(x_a_product[1],n_total_x_a[1])
        x_b_product[1] <- x_b_1z*n_x_work_b[1]
        q_x_b[1] <- min(x_b_product[1],n_total_x_b[1])
        
        marketing_cost_x[1]<- 0#
        
        TC_x[1] <- n_x_work[1]*(c_x_work*(1+(1%/%12)*x_inf)/12) + 
            n_x_new[1]*(c_x_new*(1+(1%/%12)*x_inf)) +  #sum of diychi + vitraty na now
            marketing_cost_x[1]
        TR_x[1] <- q_x_a[1]*x_p_a + q_x_b[1]*x_p_b
        profit_x[1] <- TR_x[1] - TC_x[1]
        
        
        #first period Y
        n_y_new[1] <- 0     #ifelse(i<9,0,1)
        n_y_work[1] <- 0        #ifelse(i<12,0,n_y_work[i-1]+n_y_new[i-3]) #3 month to build a factory
        y_product[1] <- y_1z*n_y_work[1]
        q_y[1] <- min(y_product[1],n_total_y[1])
        
        TC_y[1] <- n_y_work[1]*(c_y_work*(1+(1%/%12)*y_inf)/12) + 
            n_y_new[1]*(c_y_new*(1+(1%/%12)*y_inf)) #sum of diychi + vitraty na now
        TR_y[1] <- q_y[1]*y_p
        profit_y[1] <- TR_y[1] - TC_y[1]
        
        
        
        
        #next periods
        perc_x_increase_market[1]=0
        marketing_cost_x[1] <- 0
        marketing_cost_x[2] <- 0
        n_x_new_a[2] = 0
        n_x_new_b[2] = 0
        n_y_new[2] = 0
        for (i in 2:t){
            
            ## Copmpany X
            n_total_x_a[i] <- (n_total_x_a[i-1] - ifelse(i<2, 0, q_x_a[i-1]) + ifelse(i<7, 0, q_x_a[i-6]))*(1+perc_x_increase_market[i-1])
            n_total_x_b[i] <- (n_total_x_b[i-1] - ifelse(i<2, 0, q_x_b[i-1]) + ifelse(i<7, 0, q_x_b[i-6]))*(1+perc_x_increase_market[i-1])
            
            n_x_work_a[i] <- n_x_work_a[i-1]+ ifelse(i < 7, 0, n_x_new_a[i-6])
            n_x_work_b[i] <- n_x_work_b[i-1]+ ifelse(i < 7, 0, n_x_new_b[i-6])
            n_x_work[i] <-  n_x_work_a[i] + n_x_work_b[i]
            
            n_x_new[i] <- n_x_new_a[i] + n_x_new_b[i]
            
            x_a_product[i] <- x_a_1z*n_x_work_a[i]
            q_x_a[i] <- min(x_a_product[i],n_total_x_a[i])
            x_b_product[i] <- x_b_1z*n_x_work_b[i]
            q_x_b[i] <- min(x_b_product[i],n_total_x_b[i])
            
            
            perc_x_increase_market[i] <- 10^(-9)*marketing_cost_x[i] + 10^(-18)*marketing_cost_x[i]^2
            
            TC_x[i] <- n_x_work[i]*(c_x_work*(1+(i%/%12)*x_inf)/12) + 
                n_x_new[i]*(c_x_new*(1+(i%/%12)*x_inf)) +  #sum of diychi + vitraty na now
                marketing_cost_x[i]
            TR_x[i] <- q_x_a[i]*x_p_a + q_x_b[i]*x_p_b
            profit_x[i] <- TR_x[i] - TC_x[i]
            
            
            if(i < t) {
                marketing_cost_x[i+1] = 0
                n_x_new_a[i+1] = 0
                n_x_new_b[i+1] = 0
                if ((profit_x[i] - profit_x[i-1] < 0)) {
                    if (check_x_mark == 1) {
                        #yvelich marketing na 5 mln
                        marketing_cost_x[i+1] <- marketing_cost_x[i] + check_cost_mark
                    } 
                    if (check_x_factory == 1) {
                        #stroum 1 novih zavodov
                        n_x_new_a[i+1] <- check_n_x_new_a
                        n_x_new_b[i+1] <- check_n_x_new_b
                    }
                    if (check_x_price == 1) {
                        x_p_a <- x_p_a*(1 + check_x_price_incr)
                        x_p_b <- x_p_b*(1 + check_x_price_incr)
                    } 
                } 
            }
            
            
            ## Copmpany Y
            
            n_total_y[i] <- (n_total_y[i-1] - ifelse(i<2, 0, q_y[i-1]) + ifelse(i<7, 0, q_y[i-6]))
            
            n_y_work[i] <- n_y_work[i-1] + ifelse(i < 4, 0, n_y_new[i-3])
            
            #n_y_new[i] = ifelse(i < 9, 0, 2)  #zadavat
            
            y_product[i] <- y_1z*n_y_work[i]
            q_y[i] <- min(y_product[i],n_total_y[i])
            
            TC_y[i] <- n_y_work[i]*(c_y_work*(1+(i%/%12)*y_inf)/12) + 
                n_y_new[i]*(c_y_new*(1+(i%/%12)*y_inf)) #sum of diychi + vitraty na now
            TR_y[i] <- q_y[i]*y_p
            profit_y[i] <- TR_y[i] - TC_y[i]
            
            if (y_product[i] >= n_total_y[i]) {
                n_total_x_a[i+1] <- n_total_x_a[i]*0.9
                n_total_y[i+1] <- n_total_y[i] + n_total_x_a[i]*0.1
                n_y_new[i+1] = 0
                next
            }
            
            
            if(i < t) {
                
                if (i < 8) {
                    n_y_new[i+1] = 0
                } else if (i > 11) {
                    
                    n_y_new[i+1] = check_y_factory
                    
                    if ((profit_y[i] - profit_y[i-1] < 0)) {
                        if (check_y == 1) {
                            #yvelich marketing na 5 mln
                            n_y_new[i+1] = 0
                        } 
                    } 
                    
                } else {
                    n_y_new[i+1] = 3
                }
                
                
            }
            
            
            
            ###
        }
        
        df_last <- data.frame(period = 1:t,q_y,q_x_a,q_x_b)
        df_last
        
    })
    
    

    output$text1 <- renderText({ 
            tryCatch({

                
                dt <- model1()
      
                share_leader <- mean((dt$q_x_a+ dt$q_x_b)/(dt$q_y+dt$q_x_a+ dt$q_x_b))
                print(paste("The average share of the leader in the industry is ", round(share_leader, 2), "%"))
                
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
            dt$Q <- dt$q_y+dt$q_x_a + dt$q_x_b
            g1 <- ggplot(dt, aes(x = period, y = (q_x_a + q_x_b)/Q*100))
            g1 + geom_line() + 
                ylim(0,100) + 
                labs(x = "Період",y = "Частка лідера, %") +
                geom_smooth(method = "loess")

        }
    })
    

    
    
})

