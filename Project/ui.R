#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(shinyjs)
library(shinythemes)


appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"


# Define UI for application that draws a histogram
shinyUI(
    navbarPage("Case 3", theme = shinytheme("cerulean"), 

               
              # main panel with information about the project and our team        
               tabPanel( "About"
                         , fluidPage(
                            
                           fluidRow( 
                              column(12,  
                                     div(
                                            p("Case study")
                                            , style = "font-family: 'Century Gothic'; font-size: 60pt; color: rgb(0,191,255); text-align: center;"
                                     )
                                  )
                           
                              , fluidRow(
                                  column(8, offset = 2,  
                                         
                                         br()
                                         , div(
                                                p("Initial conditions of the task:")
                                                , br()
                                                , p("You know structure of sales of a particular store in a few years. It is necessary to develop an information system that will predict the required number of purchases for the composition by: groups of products, for each product. Provide Projections for purchases that are made only through a user-specified time period.")
                                                , br()
                                                , br()
                                                , p("The complete version of the software and presentation materials can be found on the", a(href = "https://github.com/mbilychenko/", "GitHub"))
                                                , br()
                                                , p("If you have any comments or suggestions for this work, write to:", span("maksym.bilychenko@gmail.com", style = "color:blue"), ".")
                                                , br()
                                                , style = "font-family: 'Century Gothic';")
                                          
                                )
                              )
                           
                         )
                        ) 
                 
               ), 
               
               tabPanel("Main",
                        
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(

                    sidebarPanel(
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),

                       uiOutput("select_item"), #select input of item number from database
                       uiOutput("select_group"),
                       checkboxInput("group_predict", "Do you want to predict for all group?", FALSE),
                       selectInput("conf_level", "Confidence level:",
                                   c("50% confidence" = "1",
                                     "80% confidence" = "2",
                                     "95% confidence" = "3"),
                                   selected = "2"),
                       uiOutput("select_date"), #select date range number from the last date of database
                       uiOutput("select_model"),
                       uiOutput("select_criteria"),
                       checkboxInput("check_price", "X - Price", FALSE),
                       checkboxInput("check_holiday", "X - Holiday", FALSE),
                       checkboxInput("check_avg_humidity", "X - Humidity", FALSE),
                       checkboxInput("check_avg_temp", "X - Temperature", FALSE),
                       br()
                       
                       #actionButton("reload","Reload name")

                    ),
                
                    # Show a plot of the generated distribution
                    mainPanel(
                        useShinyjs(),
                        inlineCSS(appCSS),
                        
                        # Loading message
                        div(
                            id = "loading-content",
                            h2("Loading...")
                        ),
                      tabsetPanel(
                        tabPanel("Plot",
                                 fluidRow(
                                   column(4, plotOutput(outputId = "add1", height = "250px")),
                                   column(4, plotOutput(outputId = "add2", height = "250px")),
                                   column(4, plotOutput(outputId = "add3", height = "250px")),
                                   
                                   fluidRow(
                                      
                                   column(12, hr()
                                           , br(), plotlyOutput(outputId = "distPlot", height = "500px")),
                                   fluidRow(column(12, 
                                                   br()       
                                                   , h4(textOutput("report_text"), align = "center")
                                                   , uiOutput("table_group_report")
                                   ))
                                   
                                   
                                 ))
                        ),
                        #plotlyOutput(outputId = "distPlot", height = "600px")),
                        tabPanel("Report",
                                 h3("To download the report, click on the button:"),
                                 downloadButton("report_download", "Generate report"))
                        #textOutput("distText"))
                      )
                      
                      

                       #textOutput("distText")
                    )
                    
                    #
                    #
                  )
               )
#-----------------------------------------------------------------------------------------------------------------------
            #   tabPanel("Group sales forecasting",
            #            sidebarLayout(
            #              sidebarPanel(
            #                h3("first")
            #              ),
            #              
            #              mainPanel(
            #                tabsetPanel(
            #                )
            #              )
            #            )
            #   )
))
