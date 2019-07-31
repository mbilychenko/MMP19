#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(forecast)
library(zoo)
library(lubridate)
library(dplyr)
library(tidyr)
library(shiny)
library(plotly)
library(ggplot2)
library(stringr)
library(hts2)
library(hts)
library(shinyjs)


shinyServer(function(input, output) {

    
    #read full database from file
    db_base <- reactive({
        database <- read.csv("database.csv")
        database$date_d <- as.Date(database$date_d, format = "%d-%m-%y")
        database
    })
    
    #generate select input for item numbers from db_base
    output$select_item <- renderUI({
        choices_db = as.character(unique(db_base()$item_id))
        if (!(input$group_predict)) {
          selectInput("selected_item_number","Item code", choices = choices_db)
        }
    })
    
    #generate select input for group numbers from db_base
    output$select_group <- renderUI({
      choices_db = as.character(unique(db_base()$group_id))
      if (input$group_predict) {
        selectInput("selected_group_number","Group code", choices = choices_db)
      }
      
    })
    
    #generate select input for models
    output$select_model <- renderUI({
      if(input$group_predict) {
        models_choice <- c("Regression", "ARIMA", "ETS", "Hierarchical")
      } else {
        models_choice <- c("Regression", "ARIMA", "ETS", "Automatically")
      }
      
      selectInput("selected_model","Model list", choices = models_choice)
    })
    
    #generate citeria input if automatic
    output$select_criteria <- renderUI({
      if(input$selected_model == "Automatically") {
        selectInput("selected_criteria_auto","Criteria list", choices = c("AIC", "RMSE"), selected = "RMSE")
      }
      
    })
    
    
    #item number
    item_react <- reactive({
      t <- as.character(input$selected_item_number)
      t
    })
    
    #dataframe creation for selected item id
    db_base_item <-reactive({
        db_base() %>%
            filter(item_id == item_react())
    })
    
    #generate select input for date range to predict
    output$select_date <- renderUI({
        last_date_db <- as.Date(db_base_item()$date_d[length(db_base_item()$date_d)], format = "%Y-%m-%d")
        dateRangeInput("daterange_selected", "Date range to predict:",
                       start = last_date_db,
                       end = last_date_db+14,
                       min   = last_date_db)
    })    
    
    
    #read dates and create date ranges for final prediction
    date_for_predictions <- reactive({
        
        date_start_range <- as.Date(as.numeric(input$daterange_selected[1]))
        date_end_range <- as.Date(as.numeric(input$daterange_selected[2]))
        
        date_start_pred <- as.Date(db_base_item()$date_d[length(db_base_item()$date_d)], format = "%Y-%m-%d") #last date from the dataset
        date_end_pred <- date_end_range #date from the range
        period_for_final_predict <- as.numeric(difftime(date_end_pred, date_start_pred, units = "days")) + 1
        period_for_zakypka <- as.numeric(difftime(date_end_range, date_start_range, units = "days")) + 1
        
        last_date_pos_to_predict <- period_for_final_predict
        first_date_pos_to_predict <- last_date_pos_to_predict - period_for_zakypka
        
        #df: 1 element - time horizont to predict in models, 2nd/3rd element - elements for start/end positions of dataframes in report values functions
        df <- c(period_for_final_predict, first_date_pos_to_predict,last_date_pos_to_predict)
        df
    })
    
    
    #Adjusting data prediction
    data_adjust <- function(x) {
        x <- ceiling(x)
        x <- replace(x, x<0,0)
        x
    }

# --------------------------------------------------------------------------------------------------------------------------------------------------   #  

#################                                                 ITEM Forecast                                                       ##################    
    
# --------------------------------------------------------------------------------------------------------------------------------------------------   #   

    #Item forecasting 
    
    #Model 1 - Regression
    
    #function to estimate tslm model
    reg_est <- function(dataset, check) {
        
        #tranform df to time series for tslm
        dataset <- ts(dataset, frequency = 7, start = c(2015,1))
 
        #prepare column indexes for x-variables
        price_column <- which( colnames(dataset)=="item_price")
        holiday_column <- which( colnames(dataset)=="holiday")
        avgtemp_column <- which( colnames(dataset)=="average_temp")
        avghumidity_column <- which( colnames(dataset)=="average_humidity")
        column_index <- c(price_column, holiday_column, avgtemp_column, avghumidity_column)
        x_columns <- check*column_index
        x_columns <- x_columns[x_columns!=0]

        #model evaluation
        if (sum(check) == 0){
            fit<-tslm("daily_sales~season+trend", data=dataset)
           
        } else{
           
            #prepare formula for tslm
            formula_names <- paste(colnames(dataset)[x_columns], collapse = "+")
            formula_reg <- paste("daily_sales~season+trend", formula_names, sep = "+")

            fit<-tslm(formula=formula_reg,data=dataset)
            
        }
        return(fit)
    }
    
    #function to predict using tslm model
    reg_pred <- function(fit_mod, dataset, horizont, check, new_dataset) {
        
        #tranform df to time series for tslm
        dataset <- ts(dataset, frequency = 7, start = c(2015,1))
        
        #prepare column indexes for x-variables
        price_column <- which( colnames(dataset)=="item_price")
        holiday_column <- which( colnames(dataset)=="holiday")
        avgtemp_column <- which( colnames(dataset)=="average_temp")
        avghumidity_column <- which( colnames(dataset)=="average_humidity")
        column_index <- c(price_column, holiday_column, avgtemp_column, avghumidity_column)
        x_columns <- check*column_index
        x_columns <- x_columns[x_columns!=0]
        
        
        if(sum(check) == 0) {
            predict_final <- forecast(fit_mod, h = horizont)
        } else {
            #prediction for test/train and final results
            if(missing(new_dataset)) {
                #prepare datase for x-variables
                x_last_one <- tail(dataset, n = 1)
                x_for_final_predict <- as.data.frame(lapply(x_last_one, rep, horizont))
                
                predict_final <- forecast(fit_mod, h = horizont, newdata = x_for_final_predict)
            } else {
                
                predict_final <- forecast(fit_mod, h = horizont, newdata = new_dataset)
            }
        }
        
        return(predict_final)
    }
    

    
    model1_reg <- reactive({
        
show("loading-content")
       
      data_byitem_train <- db_base_item() %>% filter(testing_data == 0)
      data_byitem_test <- db_base_item() %>% filter(testing_data == 1)

      #choose columns
      choose_price = as.numeric(input$check_price)
      choose_avg_humidity = as.numeric(input$check_avg_humidity)
      choose_holiday = as.numeric(input$check_holiday)
      choose_aver_t = as.numeric(input$check_avg_temp)
      reg_check <- c(choose_price, choose_holiday, choose_aver_t, choose_avg_humidity) #vector of binary values for factors to model
      
      #estimate model on train subset
      reg_model <- reg_est(data_byitem_train, reg_check)
      reg_model_AIC<-AIC(reg_model)
      
      #calculate RMSE value on test set
      test_length <- nrow(data_byitem_test)
      reg_test_predict <- reg_pred(fit_mod = reg_model,dataset =  data_byitem_train,horizont = test_length,check = reg_check, new_dataset = data_byitem_test)
      reg_test_predict_values <- reg_test_predict$mean
      real_sales <- data_byitem_test$daily_sales
      reg_rmse <- sqrt(sum((reg_test_predict_values-real_sales)^2)/length(real_sales))
      
      
      result_reg<- c(AIC = reg_model_AIC, RMSE = reg_rmse)
hide("loading-content")
      result_reg

    })
    
 
    model1_reg_predict <- reactive({ 
       
        #input$reload
show("loading-content")
        
      period_for_final_predict <- date_for_predictions()[1]
      data_byitem <- db_base() %>% filter(item_id == input$selected_item_number)
 
      #choose columns
      choose_price = as.numeric(input$check_price)
      choose_avg_humidity = as.numeric(input$check_avg_humidity)
      choose_holiday = as.numeric(input$check_holiday)
      choose_aver_t = as.numeric(input$check_avg_temp)
      reg_check <- c(choose_price, choose_holiday, choose_aver_t, choose_avg_humidity) #vector of binary values for factors to model
      
      #estimate and predict using tslm
      reg_fit <- reg_est(data_byitem, reg_check)

      reg_predict_final <- reg_pred(fit_mod = reg_fit,dataset =  data_byitem,
                                   horizont = period_for_final_predict ,check = reg_check)
      
      #prepare data frame for reports
      reg_predicted_final_95 <- as.numeric(reg_predict_final$upper[,2])
      reg_predicted_final_80 <- as.numeric(reg_predict_final$upper[,1])
      reg_predicted_final_50 <- as.numeric(reg_predict_final$mean)
      reg_predicted_final_20 <- as.numeric(reg_predict_final$lower[,1])
      reg_predicted_final_5 <- as.numeric(reg_predict_final$lower[,2])
      
      reg_df_predicted <- data.frame(reg_predicted_final_50, reg_predicted_final_80, 
                                     reg_predicted_final_95, reg_predicted_final_20, 
                                     reg_predicted_final_5)
      
      reg_df_predicted <- data_adjust(reg_df_predicted)
hide("loading-content")
      
      reg_df_predicted
     
    }) 


    #Model 2  - ARIMA
    
    #function to estimate arima model
    arima_est <- function(target, dataset, check) {
        #if the user chose variables for the model
        price_column <- which( colnames(dataset)=="item_price")
        holiday_column <- which( colnames(dataset)=="holiday")
        avgtemp_column <- which( colnames(dataset)=="average_temp")
        avghumidity_column <- which( colnames(dataset)=="average_humidity")
        
        column_index <- c(price_column, holiday_column, avgtemp_column, avghumidity_column)

        x_columns <- check*column_index
        x_columns <- x_columns[x_columns!=0]
        
        #model evaluation
        if(sum(check) == 0) {
            fit <- auto.arima(target)
        } else {
            x_arima <- as.matrix(dataset[,c(x_columns)])
            fit <- auto.arima(target, xreg = x_arima)
        }
        return(fit)
    }
    
    #function to predict using arima model
    arima_pred <- function(fit_mod, dataset, horizont, check, new_dataset) {
        #if the user chose variables for the model
        price_column <- which( colnames(dataset)=="item_price")
        holiday_column <- which( colnames(dataset)=="holiday")
        avgtemp_column <- which( colnames(dataset)=="average_temp")
        avghumidity_column <- which( colnames(dataset)=="average_humidity")
        
        column_index <- c(price_column, holiday_column, avgtemp_column, avghumidity_column)
        x_columns <- check*column_index
        x_columns <- x_columns[x_columns!=0]
        
        if(sum(check) == 0) {
            predict_final <- forecast(fit_mod, h = horizont)
        } else {
            #prediction for test/train and final results
            if(missing(new_dataset)) {
                #prepare datase for x-variables
                x_last_one <- tail(dataset, n = 1)
                x_for_final_predict <- as.data.frame(lapply(x_last_one, rep, horizont))
                x_final <- as.matrix((x_for_final_predict[,c(x_columns)]))
                
                predict_final <- forecast(fit_mod, h = horizont, xreg = x_final)
            } else {
                x_final <- as.matrix((new_dataset[,c(x_columns)]))
                predict_final <- forecast(fit_mod, h = horizont, xreg = x_final)
            }
        }
        
        return(predict_final)
    }    
    
    

    model2_arima <- reactive({

show("loading-content")
        data_byitem_train <- db_base_item() %>% filter(testing_data == 0)
        data_byitem_test <- db_base_item() %>% filter(testing_data == 1)
        
        arima_check_price = as.numeric(input$check_price)
        arima_check_holiday = as.numeric(input$check_holiday)
        arima_check_avg_temp = as.numeric(input$check_avg_temp)
        arima_check_avg_humidity = as.numeric(input$check_avg_humidity)
        arima_check <- c(arima_check_price, arima_check_holiday, arima_check_avg_temp, arima_check_avg_humidity) #vector of binary values for factors to model

        y <- ts(data_byitem_train$daily_sales, frequency = 7, start = c(2015,1))

        #estimate arima model on train set
        fit_arima <- arima_est(y, data_byitem_train, arima_check)
        arima_aic <- AIC(fit_arima)


        #calculate RMSE value on test set
        test_length <- nrow(data_byitem_test)
        arima_test_predict <- arima_pred(fit_mod = fit_arima, dataset = data_byitem_train, 
                   horizont = test_length, check = arima_check, new_dataset = data_byitem_test)
        arima_test_predict_values <- arima_test_predict$mean
        real_sales <- data_byitem_test$daily_sales
        arima_rmse <- sqrt(sum((arima_test_predict_values-real_sales)^2)/length(real_sales))
        
        result_arima <- c(AIC = arima_aic, RMSE = arima_rmse)
hide("loading-content")
        result_arima
    })
    

    model2_arima_predict <- reactive({        
        
show("loading-content")
        
        period_for_final_predict <- date_for_predictions()[1]
        data_byitem <- db_base_item()
        
        y <- ts(data_byitem$daily_sales, frequency = 7, start = c(2015,1))
        
        arima_check_price = as.numeric(input$check_price)
        arima_check_holiday = as.numeric(input$check_holiday)
        arima_check_avg_temp = as.numeric(input$check_avg_temp)
        arima_check_avg_humidity = as.numeric(input$check_avg_humidity)
        arima_check <- c(arima_check_price, arima_check_holiday, arima_check_avg_temp, arima_check_avg_humidity) #vector of binary values for factors to model
        

        fit_arima <- arima_est(y, data_byitem, arima_check)
        arima_predict_final <- arima_pred(fit_arima, data_byitem, period_for_final_predict, arima_check)
        

        arima_predicted_final_95 <- as.numeric(arima_predict_final$upper[,2])
        arima_predicted_final_80 <- as.numeric(arima_predict_final$upper[,1])
        arima_predicted_final_50 <- as.numeric(arima_predict_final$mean)
        arima_predicted_final_20 <- as.numeric(arima_predict_final$lower[,1])
        arima_predicted_final_5 <- as.numeric(arima_predict_final$lower[,2])

        arima_df_predicted <- data.frame(arima_predicted_final_50, arima_predicted_final_80, 
                                         arima_predicted_final_95, arima_predicted_final_20, 
                                         arima_predicted_final_5)

        arima_df_predicted <- data_adjust(arima_df_predicted)
        
hide("loading-content")
        
        arima_df_predicted

    })
    

    #Model 3 - ETS
    
    model3_ets <- reactive({
show("loading-content")
      data_byitem_train <- db_base_item() %>% filter(testing_data == 0)
      data_byitem_test <- db_base_item() %>% filter(testing_data == 1)
      
      y <- ts(data_byitem_train$daily_sales, frequency = 7, start = c(2015,1))
      
      fit_ets <- ets(y, damped = T,ic = c("aic"))
      ets_aic <- AIC(fit_ets)
      
      test_length <- nrow(data_byitem_test)
      ets_test_predict <- forecast(fit_ets, h = test_length)

      #calculate RMSE value on test set
      ets_test_predict_values <- ets_test_predict$mean
      real_sales <- data_byitem_test$daily_sales
      ets_rmse <- sqrt(sum((ets_test_predict_values-real_sales)^2)/length(real_sales))
      
      result_ets <- c(AIC = ets_aic, RMSE =ets_rmse)
hide("loading-content")
      result_ets
    })
    
    
    model3_ets_predict <- reactive({        

show("loading-content")
        
      period_for_final_predict <- date_for_predictions()[1]
      data_byitem <- db_base_item()
      
      y <- ts(data_byitem$daily_sales, frequency = 7, start = c(2015,1))
      
      
      fit_ets_final <- ets(y, damped = T,ic = c("aic"))
      ets_predict_final <- forecast(fit_ets_final, h = period_for_final_predict)
      
      ets_predicted_final_95 <- as.numeric(ets_predict_final$upper[,2])
      ets_predicted_final_80 <- as.numeric(ets_predict_final$upper[,1])
      ets_predicted_final_50 <- as.numeric(ets_predict_final$mean)
      ets_predicted_final_20 <- as.numeric(ets_predict_final$lower[,1])
      ets_predicted_final_5 <- as.numeric(ets_predict_final$lower[,2])
      
      ets_df_predicted <- data.frame(ets_predicted_final_50, ets_predicted_final_80, 
                                     ets_predicted_final_95, ets_predicted_final_20, 
                                     ets_predicted_final_5)
      
      ets_df_predicted <- data_adjust(ets_df_predicted)
      
hide("loading-content")
      
      ets_df_predicted
      
    })   
    
    
    #Model 4 - Automatic 
    model_item_auto <- reactive({
        

        
      reg_res <- model1_reg()
      arima_res <- model2_arima()
      ets_res <- model3_ets()

      choice_criteria = ifelse(as.character(input$selected_criteria_auto) == "AIC", 1, 2) #1 - aic, 2- rmse
    
      results <- data.frame(reg_res, arima_res, ets_res)
      
      if (choice_criteria == 1) {
        model_auto_select_number <- as.numeric(which.min(results[1,]))
      } else {
        model_auto_select_number <- as.numeric(which.min(results[2,]))
      }
      
      if (model_auto_select_number == 1) {
        auto_df_predicted <- model1_reg_predict()
      } else if (model_auto_select_number == 2) {
        auto_df_predicted <- model2_arima_predict()
      } else if (model_auto_select_number == 3) {
        auto_df_predicted <- model3_ets_predict()
      } else {
        auto_df_predicted <- model3_ets_predict()
      }

      auto_df_predicted
      
    })
    
    
# --------------------------------------------------------------------------------------------------------------------------------------------------   #  
    
#################                                                 GROUP Forecast                                                      ##################    
    
# --------------------------------------------------------------------------------------------------------------------------------------------------   #       
    
    #Group forecasting using 4 methods
    
    model_group_predict <- reactive({
        
      show("loading-content")  
    
      database <- db_base()
      all_items <- as.character(unique(database$item_id))
      item_numbers_gr <- all_items[str_detect(all_items, as.character(input$selected_group_number))]
      reg_list_predicted <- list() 
      n_items <- length(item_numbers_gr)
      
      period_for_final_predict <- date_for_predictions()[1]
      
      for (i in 1:n_items) {

        item_number = item_numbers_gr[i] #parameter of item number to predict sales
        data_byitem <- database %>% filter(item_id == item_number)

        if(input$selected_model=="Regression"){
            
              choose_price = as.numeric(input$check_price)
              choose_avg_humidity = as.numeric(input$check_avg_humidity)
              choose_holiday = as.numeric(input$check_holiday)
              choose_aver_t = as.numeric(input$check_avg_temp)
              reg_check <- c(choose_price, choose_holiday, choose_aver_t, choose_avg_humidity) #vector of binary values for factors to model
            
              reg_fit <- reg_est(data_byitem, reg_check)
            
            
              reg_predict_final <- reg_pred(fit_mod = reg_fit,dataset =  data_byitem,
                                          horizont = period_for_final_predict ,check = reg_check)

              reg_predicted_final_95 <- as.numeric(reg_predict_final$upper[,2])
              reg_predicted_final_80 <- as.numeric(reg_predict_final$upper[,1])
              reg_predicted_final_50 <- as.numeric(reg_predict_final$mean)
              reg_predicted_final_20 <- as.numeric(reg_predict_final$lower[,1])
              reg_predicted_final_5 <- as.numeric(reg_predict_final$lower[,2])
            
              df_predicted <- data.frame(reg_predicted_final_50, reg_predicted_final_80, 
                                           reg_predicted_final_95, reg_predicted_final_20, 
                                           reg_predicted_final_5)

        } else if(input$selected_model=="ARIMA"){
    
              y <- ts(data_byitem$daily_sales, frequency = 7, start = c(2015,1))
              
              arima_check_price = as.numeric(input$check_price)
              arima_check_holiday = as.numeric(input$check_holiday)
              arima_check_avg_temp = as.numeric(input$check_avg_temp)
              arima_check_avg_humidity = as.numeric(input$check_avg_humidity)
              arima_check <- c(arima_check_price, arima_check_holiday, arima_check_avg_temp, arima_check_avg_humidity) #vector of binary values for factors to model
              
              
              fit_arima <- arima_est(y, data_byitem, arima_check)
              arima_predict_final <- arima_pred(fit_arima, data_byitem, period_for_final_predict, arima_check)
        
              arima_predicted_final_95 <- as.numeric(arima_predict_final$upper[,2])
              arima_predicted_final_80 <- as.numeric(arima_predict_final$upper[,1])
              arima_predicted_final_50 <- as.numeric(arima_predict_final$mean)
              arima_predicted_final_20 <- as.numeric(arima_predict_final$lower[,1])
              arima_predicted_final_5 <- as.numeric(arima_predict_final$lower[,2])
              
              df_predicted <- data.frame(arima_predicted_final_50, arima_predicted_final_80, 
                                               arima_predicted_final_95, arima_predicted_final_20, 
                                               arima_predicted_final_5)

        } else if(input$selected_model=="ETS"){
              
              y <- ts(data_byitem$daily_sales, frequency = 7, start = c(2015,1))
              fit_ets_final <- ets(y, damped = T,ic = c("aic"))
              
              ets_predict_final <- forecast(fit_ets_final, h = period_for_final_predict)
              
              ets_predicted_final_95 <- as.numeric(ets_predict_final$upper[,2])
              ets_predicted_final_80 <- as.numeric(ets_predict_final$upper[,1])
              ets_predicted_final_50 <- as.numeric(ets_predict_final$mean)
              ets_predicted_final_20 <- as.numeric(ets_predict_final$lower[,1])
              ets_predicted_final_5 <- as.numeric(ets_predict_final$lower[,2])
              
              df_predicted <- data.frame(ets_predicted_final_50, ets_predicted_final_80, 
                                             ets_predicted_final_95, ets_predicted_final_20, 
                                             ets_predicted_final_5)
    
              df_predicted

        } else if(input$selected_model=="Hierarchical"){
            break
        } else {
          df_predicted <- 0
        }
        
        df_predicted <- data_adjust(df_predicted)

        reg_list_predicted[[i]] <- df_predicted
        
      } 
     
      if(input$selected_model=="Hierarchical") {

          group_number = as.character(input$selected_group_number) #parameter of item number to predict sales
          data_bygroup <- database %>% 
              filter(group_id == group_number) %>% 
              select(date_d, item_id, daily_sales) %>% 
              spread(key = item_id, value = daily_sales) %>%
              select(-date_d) %>%
              ts(start = 2016, freq = 7)
          
          data_bygroup_hts <- gts(data_bygroup)
          data_bygroup_fc <- hts2::forecast.gts(data_bygroup_hts, h = period_for_final_predict, keep.intervals = TRUE)

          hts_list_predicted <- list() 
          for (j in 1:n_items) {
              df_temp <- data.frame(as.numeric(data_bygroup_fc$bts[,j]),
                                    as.numeric(data_bygroup_fc$upper[,c(j)]),
                                    as.numeric(data_bygroup_fc$upper[,c(j+n_items)]), 
                                    as.numeric(data_bygroup_fc$lower[,c(j)]),
                                    as.numeric(data_bygroup_fc$lower[,c(j+n_items)]))
              colnames(df_temp) <- c("hts_predicted_final_50", "hts_predicted_final_80", 
                                     "hts_predicted_final_95", "hts_predicted_final_20", 
                                     "hts_predicted_final_5")
              df_temp <- data_adjust(df_temp)
              reg_list_predicted[[j]] <- df_temp
          }
         
      }

      horizont <- date_for_predictions()[1]
      df <- data.frame(matrix(unlist(reg_list_predicted), nrow=5*horizont, byrow = F),stringsAsFactors=FALSE)
      colnames(df) <- item_numbers_gr
      df <- data.frame(df, conf = rep(c("50", "80", "95", "20", "5"),each = horizont),
                       period = rep(c(1:horizont),n = 5))
      
      data_long <- gather(df, item, measurement, c(1:n_items), factor_key=TRUE)
      
      hide("loading-content")
      
      data_long
    })
    
# --------------------------------------------------------------------------------------------------------------------------------------------------   #  
    
#################                                                 REPORT                                                              ##################    

# --------------------------------------------------------------------------------------------------------------------------------------------------   #  
        
    #Preparation data for reports
    
    #Predict values for item-report 
    preicted_val_report_item <- reactive({
        
        conf_level <- as.numeric(input$conf_level) #1-50, 2 - 80, 3-95
        
        if(input$selected_model=="Regression"){
          reg_predicted <- model1_reg_predict()
          precited_final <- reg_predicted
        } else if(input$selected_model=="ARIMA"){
          arima_predicted <-  model2_arima_predict()
          precited_final <- arima_predicted
        } else if(input$selected_model=="ETS"){
          ets_predicted <- model3_ets_predict()
          precited_final <- ets_predicted
        } else if(input$selected_model=="Automatically"){
          auto_predicted <- model_item_auto()
          precited_final <- auto_predicted 
        } else {
          precited_final <- 0 #
        }

        recomend_zakupka <- c(sum(precited_final[date_for_predictions()[2]:date_for_predictions()[3],conf_level]))
        recomend_zakupka
    })
    

    #Predict values for group-report 
    preicted_val_report_group <- reactive({
      
      conf_level <- as.numeric(input$conf_level) #1-50, 2 - 80, 3-95
      data_long <- model_group_predict()
      
      if (conf_level == 3) {
        conf_level_number <- "95"
      } else if (conf_level == 2) {
        conf_level_number <- "80"
      } else {
        conf_level_number <- "50"
      }
      
      dt_1 <- date_for_predictions()[2]
      dt_2 <- date_for_predictions()[3]
      
      df_to_report <- data_long %>% filter(conf == conf_level_number, 
                                           period %in% seq(dt_1,dt_2,by = 1)) %>% group_by(item) %>% summarize(total_zak = sum(measurement))
      
      df_to_report

    })
    

    #Prepare data for item-report 
    item_data_preparation_for_plot <- reactive({
        
        if(input$selected_model=="Regression"){
            arima_df_predicted <- model1_reg_predict()
        } else if(input$selected_model=="ARIMA"){
            arima_df_predicted <- model2_arima_predict()
        } else if(input$selected_model=="ETS"){
            arima_df_predicted <- model3_ets_predict()
        } else if(input$selected_model=="Automatically"){
            arima_df_predicted <- model_item_auto()
        } else {
            arima_df_predicted <- model3_ets_predict()
        }
        
        
        horizont <- nrow(arima_df_predicted)
        new_date_d <- seq.Date(from = db_base_item()$date_d[length(db_base_item()$date_d)]+1, by = "day",length.out = horizont)
        x <- c(db_base_item()$date_d, new_date_d)
        y_real <- c(db_base_item()$daily_sales, rep(NA, horizont))
        y_pred_mean <- c(rep(NA, length(x)-horizont), arima_df_predicted[,1])
        y_pred_up <- c(rep(NA, length(x)-horizont), arima_df_predicted[,3])
        y_pred_low <- c(rep(NA, length(x)-horizont), arima_df_predicted[,5])
        
        df_to_plotly <- data.frame(x, y_real, y_pred_mean, y_pred_up,y_pred_low)
        df_to_plotly
    })  
    

    #Prepare data for group-report 
    group_data_preparation_for_plot <- reactive({
        
        conf_level <- as.numeric(input$conf_level) #1-50, 2 - 80, 3-95
        data_long <- model_group_predict()
        
        if (conf_level == 3) {
            conf_level_number <- "95"
        } else if (conf_level == 2) {
            conf_level_number <- "80"
        } else {
            conf_level_number <- "50"
        }
        
        df_to_plot <- data_long %>% filter(conf == conf_level_number) 
        
        df_to_plot 
    })        
    
    
#-----------------------------------------------------------------------------------------------------------------------------------------------------#    
    
    #Preparaion results for output
    #plots for temperature and humidity
    
    output$add1 <- renderPlot({
      
      data_byitem <- db_base_item()
      av_temp <-  data_byitem$average_temp
      
      add1 <- ggplot(data_byitem,aes(x = data_byitem$date_d, y = av_temp)) + 
          geom_point(aes(colour = av_temp)) +
          scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 10) + 
          geom_smooth(color = "red",size = 1) +
          ggtitle ("Daily average temperature") +
          xlab("Date") +  ylab ("Average Temperature (C)")
    add1
    
    })
    
    output$add2 <- renderPlot({
      
      data_byitem <- db_base_item()
      humidity <- data_byitem$average_humidity
      
      add2 <- ggplot(data_byitem,aes(x = data_byitem$date_d, y = humidity, group = 1))+
          geom_point(aes(colour = humidity)) +
          scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 65)+
          geom_smooth(color = "red",size = 1)+
          ggtitle ("Daily average humidity") +
          xlab("Date") +  ylab ("humidity, %")
    add2
    
    })

    output$add3 <- renderPlot({
        
        data_byitem <- db_base_item()
        price <- data_byitem$item_price
        
        add3 <- ggplot(data_byitem,aes(x = data_byitem$date_d, y = price, group = 1))+
            geom_line(aes(colour = price)) +
            scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 65)+
            geom_smooth(color = "red",size = 1)+
            ggtitle ("Daily average price") +
            xlab("Date") +  ylab ("price, %")
        add3
        
    })    
    
    #Prepare table for group forecast
    output$table_group_report <- renderUI({

        if (input$group_predict) {
          output$aa <- renderDataTable(preicted_val_report_group())
          dataTableOutput("aa")
         
        }
    })

    
    
    #Prepare plot for item/group
    output$distPlot <- renderPlotly({
        
    if(input$group_predict) {
      
      df_to_plot <- group_data_preparation_for_plot()
      dt_end <- as.Date(as.numeric(input$daterange_selected[2]))
      n_period_len <- length(as.character(unique(df_to_plot$period)))
      dt_st <- dt_end-n_period_len+1
      period_date <- seq.Date(from = dt_st, to = dt_end, by = "day")
      n_repeat <- length(as.character(unique(df_to_plot$item)))
      df_to_plot$date_period <- rep(period_date, n_repeat)

      p2 <- plot_ly(df_to_plot, x = ~date_period, y = ~measurement, color = ~item, type = 'scatter', mode = 'lines',
              showlegend = TRUE) %>% 
              layout(title = "Time Series for Group (predicted)",
                 paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                 xaxis = list(title = "Date",
                              gridcolor = 'rgb(255,255,255)',
                              showgrid = TRUE,
                              showline = FALSE,
                              showticklabels = TRUE,
                              tickcolor = 'rgb(127,127,127)',
                              ticks = 'outside',
                              zeroline = FALSE),
                 yaxis = list(title = "Sales, item",
                              gridcolor = 'rgb(255,255,255)',
                              showgrid = TRUE,
                              showline = FALSE,
                              showticklabels = TRUE,
                              tickcolor = 'rgb(127,127,127)',
                              ticks = 'outside',
                              zeroline = FALSE))
      p2
    } else {
      df_to_plotly <- item_data_preparation_for_plot()
      
      p <- plot_ly(df_to_plotly, x = ~x, y = ~y_real, type = 'scatter', mode = 'lines',
                   line = list(color='rgba(2,16,86,0.8)'), 
                   showlegend = FALSE, name = 'Main') %>%
        add_trace(y = ~y_pred_up, type = 'scatter', mode = 'lines',
                  line = list(color = 'transparent'),
                  name = 'High') %>%
        add_trace(y = ~y_pred_low, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low') %>%
        add_trace(y = ~y_pred_mean, type = 'scatter', mode = 'lines',
                  line = list(color='rgb(0,100,80)'),
                  name = 'Mean') %>%
        layout(title = "Time Series for Item",
               paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
               xaxis = list(title = "Date",
                            gridcolor = 'rgb(255,255,255)',
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside',
                            zeroline = FALSE),
               yaxis = list(title = "Sales, item",
                            gridcolor = 'rgb(255,255,255)',
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside',
                            zeroline = FALSE))

      p
      
    }

  })

    
    
    #Prepare text for item/group
    output$report_text <- renderText({

        if(input$group_predict) {
          text <- paste("The recommended volume of purchases for group", input$selected_group_number,
                        "from", as.Date(as.numeric(input$daterange_selected[1])), "to",
                        as.Date(as.numeric(input$daterange_selected[2]))  ,"are presented in the table:")
        } else {
          
          text <- paste("The recommended volume of purchases for item", input$selected_item_number,
                        "from", as.Date(as.numeric(input$daterange_selected[1])), "to",
                        as.Date(as.numeric(input$daterange_selected[2]))  ,"is", round(preicted_val_report_item(), 0), sep = " ")
          
        }
        
        print(text)
    })
  
    
  
    #Prepare report download
    output$report_download <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.doc",
        content = function(file) {
    
          if(input$group_predict) {
            tempReport <- file.path(tempdir(), "report_group.Rmd")
            file.copy("report_group.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(table_t = preicted_val_report_group(), 
                           group_n = as.character(input$selected_group_number),
                           df_plot = group_data_preparation_for_plot(),
                           dt_st = as.Date(as.numeric(input$daterange_selected[1])),
                           dt_end = as.Date(as.numeric(input$daterange_selected[2])))
          } else {
            tempReport <- file.path(tempdir(), "report_item.Rmd")
            file.copy("report_item.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(n_t = round(preicted_val_report_item(), 0),
                           item_n = item_react(),
                           data_plot = item_data_preparation_for_plot(),
                           dt_st = as.Date(as.numeric(input$daterange_selected[1])),
                           dt_end = as.Date(as.numeric(input$daterange_selected[2])))
          }

          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
    )
  

})
