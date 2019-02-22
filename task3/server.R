#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DEoptimR)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  set.seed(124)

  model1 <- reactive({
    
    #initialization
    n <- as.numeric(input$n)
    alpha0_min <- as.numeric(input$alpha0[1])
    alpha0_max <- as.numeric(input$alpha0[2])
    alpha1_min <- as.numeric(input$alpha1[1])
    alpha1_max <- as.numeric(input$alpha1[2])
    alpha2_min <- as.numeric(input$alpha2[1])
    alpha2_max <- as.numeric(input$alpha2[2])
    error_funct <- as.numeric(input$error_function) #1 - abs,  2 -^2
    
    alpha0 <- runif(n, alpha0_min, alpha0_max)
    alpha1 <- runif(n, alpha1_min, alpha1_max)
    alpha2 <- runif(n, alpha2_min, alpha2_max)
    
    p <- 15
    k <- sample(c(rep(1,40),rep(2,20), rep(2.5, 20), rep(3, 20)))
    l <- rep(1, n)
    
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
    
    w <- (sum(result_l)+120)/30
    l_total <- sum(result_l)

    
    df <- data.frame(alpha0, alpha1, alpha2, k, l = result_l)
    res_list <- list(w, df)
    res_list
    
    
  })
  
  #####   RESULTS ######  
  
  
  output$text1 <- renderText({
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    result <- model1()
    print(paste("The equilibrium wage will be", round(result[[1]][1], 2), "u."))
  })
  
  output$download_data <- downloadHandler(
    filename =  function(){"data.csv"},
    content = function(file) {
      result <- model1()
      # Code that creates a file in the path <file>
      write.csv(result[[2]], file, row.names = FALSE)
    }
  )
  
  
})
