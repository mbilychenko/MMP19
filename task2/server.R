

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)
library(nleqslv)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  set.seed(124)
  
  model1 <- reactive({
    #read parameters from user
    m <- input$m +1 #number of periods
    n <- input$n #number of people
    p1_start <- as.numeric(input$p1)
    p2_start <- as.numeric(input$p2)
    r <- as.numeric(input$r)/100
    bmin <- as.numeric(input$b[1])
    bmax <- as.numeric(input$b[2])
    #p1_check <- as.numeric(input$price_check) #1 -price 1, 0 - price 2
    check_method = as.numeric(input$method)  #1 -slyckiy, 2 - hicks
    p1_check = 1
    
    #Validation
    validate(
      need(is.numeric(input$p1), "Please input a number")
    )
    validate(
      need(is.numeric(input$p2), "Please input a number")
    )
    
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
    
    withProgress(message = 'Процесс моделювання', value = 0, {
      #next periods
      for (i in 2:m){
        #random choice of goods
        p1_check <- rbinom(1, size = 1, prob = 0.5)

        #price increases
        p1[i] <- p1[i-1]*(1+r*p1_check)
        p2[i] <- p2[i-1]*(1+r*(1-p1_check))
        
        #new equlibrium - e2
        for (j in 1:n) {
          x1[i, j] <- (alpha1[j]/alpha2[j])*b[i, j]/((alpha1[j]/alpha2[j]+1)*p1[i])
          x2[i, j] <- b[i, j]/((alpha1[j]/alpha2[j]+1)*p2[i])
          u[i, j] <- x1[i, j]^(alpha1[j])*x2[i, j]^(alpha2[j])
        }
        
        if (check_method == 1) {
          # e3 (slyckiy)
          for (j in 1:n) {
            b_temp[j] <- p1[i]*x1[i-1, j] + p2[i]*x2[i-1, j]
            x1_temp[j] <- (alpha1[j]/alpha2[j])*b_temp[j]/((alpha1[j]/alpha2[j]+1)*p1[i])
            x2_temp[j] <- b_temp[j]/((alpha1[j]/alpha2[j]+1)*p2[i])
            #u[i, j] <- x1[i, j]^(alpha1[j])*x2[i, j]^(alpha2[j])
          }
        }
        
        if (check_method == 2) {
          # e3 (hicks)
          for (j in 1:n) {
            target <- function(x, p1_n = p1[i], p2_n = p2[i], u_n = u[i-1, j], b_n = b[i, j], alpha1_n = alpha1[j], alpha2_n = alpha2[j])
            {
              y <- ((b_n - p2_n*x)/p1_n)^alpha1_n*x^alpha2_n - u_n
              y
            }
            xstart <- x2[i, j]
            res <- nleqslv(xstart, target, control=list(btol=.01, allowSingular=TRUE),method="Newton")
            x2_temp[j] <- res$x
            x1_temp[j] <- (b[i, j] - p2[i]*x2_temp[j])/p1[i]
          }
        }
        
        #compensation
        if (p1_check == 1) {
          ef_dohody <- abs(x1[i,]- x1_temp)
        } else {
          ef_dohody <- abs(x2[i,]- x2_temp)
        }
        
        compensation[i] <- compensation[i-1]+sum(ef_dohody)
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/m, detail = paste("Опрацювання періоду ", i))
        
        if (i==m) break
        b[i+1, ] <- b[i, ] + ef_dohody
        
      }
    })
    #creating final dataset
    x1_total_t <- apply(x1, 1, sum)
    x2_total_t <- apply(x2, 1, sum)
    dt <- data.frame(x1 = x1_total_t, x2 = x2_total_t, p1, p2, compensation, period = 1:length(p1))
    dt
    
  })
  
#####   RESULTS ######  
  
  output$plot1 <- renderPlot({
    dt <- model1()
    if(input$showPlot_n) {
      g1_1 <- ggplot(dt, aes(x = x1, y = p1, col = period))
      g1_1 + geom_point(size = 1.5) +
        labs(x = "Загальне споживання товару 1", y = "Ціна товару 1") +
        theme_classic() +
        scale_colour_gradient(low="blue", high="red", name = "Період")
    }
  })
  
  output$plot2 <- renderPlot({
    dt <- model1()
    if(input$showPlot_n) {
      g1_2 <- ggplot(dt, aes(x = x2, y = p2, col = period))
      g1_2 + geom_point(size = 1.5) +
        labs(x = "Загальне споживання товару 2", y = "Ціна товару 2") +
        theme_classic() +
        scale_colour_gradient(low="blue", high="red", name = "Період")
    }
  })
  
  output$plot3 <- renderPlot({
    dt <- model1()
    if(input$showPlot_n) {
      g2 <- ggplot()
      ggplot() + 
        geom_line(aes(x = period, y = x1, colour = p1), dt, size = 2) +
        geom_line(aes(period, x2, colour = p2), dt, size = 2) +
        labs(x = "Період", y = "Обсяг споживання", colour = "Ціна") +
        theme_classic()  +
        scale_colour_gradient(low="blue", high="red")
    }
  })
  
  output$plot4 <- renderPlot({
    dt <- model1()
    m <- length(dt$p1)
    
    if(input$showPlot_n) {
      i_lasp <- (dt$x1[1:(m-1)]*dt$p1[2:m]+dt$x2[1:(m-1)]*dt$p2[2:m])/(dt$x1[1:(m-1)]*dt$p1[1:(m-1)]+dt$x2[1:(m-1)]*dt$p2[1:(m-1)])
      i_pashe <- (dt$x1[2:m]*dt$p1[2:m]+dt$x2[2:m]*dt$p2[2:m])/(dt$x1[2:m]*dt$p1[1:(m-1)]+dt$x2[2:m]*dt$p2[1:(m-1)])
      i_fisher <- sqrt(i_lasp*i_pashe)
      temp3 <- data.frame(i_lasp, i_pashe, i_fisher, period = 1:length(i_fisher))
      temp3_1 <- gather(temp3, key = "type", value = measurment, -period)
      g3 <- ggplot(temp3_1, aes(x = period, y = measurment, col = type))
      g3 + geom_line(size = 0.5) +
        labs(x = "Період", y = "Значення індексу") +
        theme_classic() +
        guides(col = guide_legend(title = "Індекси")) +
        scale_color_manual(labels = c("Фішера", "Ласпейреса", "Пааше"), values = c("red", "green","blue")) +
        geom_smooth(method = "loess")
    }
  })  
  
  output$plot5 <- renderPlot({
    dt <- model1()
    if(input$showPlot_n) {
      g4 <- ggplot(dt, aes(x = period, y = compensation))
      g4 + geom_area(color="darkblue",
                     fill="lightblue") +
        labs(x = "Період", y = "Сумарна величина компенсації") +
        theme_classic() 
    }
  })  
  
  
  output$download_data <- downloadHandler(
    filename =  function(){"data.csv"},
    content = function(file) {
      # Code that creates a file in the path <file>
      write.csv(model1(), file, row.names = FALSE)
    }
  )

  
})


