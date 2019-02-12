library(shiny)
library(markdown)

shinyUI(navbarPage("Задача №1", 
                   tabPanel("Результат", 
                            
                            # Sidebar 
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("alpha_first",
                                                "Швидкість розповсюдження вірусу, %",
                                                min = 1,
                                                max = 100,
                                                value = 10),
                                    sliderInput("beta_first",
                                                "Смертність вірусу, %",
                                                min = 1,
                                                max = 100,
                                                value = 15),
                                    sliderInput("growth_gamma",
                                                "Фіксований приріст частки витрат на вірус, 0.01%",
                                                min = 1,
                                                max = 1000,
                                                value = 100),
                                    selectInput("m",
                                                "Обчислювальні можливості:",
                                                choices = list("Small" = 100, "Medium" = 1000,
                                                               "Large" = 10000), 
                                                selected = 1000),
                                    selectInput("fund_base",
                                                "Початковий розмір фонду на боротьбу з вірусом:",
                                                choices = list("Small" = 10^5, "Medium" = 10^10,
                                                               "Large" = 10^15), 
                                                selected = 10^10),
                                    selectInput("n0",
                                                "Початкова кількість населення:",
                                                choices = list("Small" = 10^4, "Medium" = 10^10,
                                                               "Large" = 10^15), 
                                                selected = 10^10),
                                    #sliderInput("period_sleep", "'Sleep' period:", min = 1,max = 30,value = 10),

                                    
                                    checkboxInput("showtask_ch", "Змінити функцію витрат після проходження піку сметрності?", value = TRUE),
                                    
                                    checkboxInput("showPlot_n", "Показати/Сховати графік 1", value = TRUE),
                                    checkboxInput("showPlot_market", "Показати/Сховати графік 2", value = TRUE),
                                    #checkboxInput("showTable", "Show/Hide Table 1", value = TRUE),
                                    
                                    submitButton("Submit") 
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden;}",
                                               ".shiny-output-error:before { visibility: hidden;}"),
                                    textOutput("text1"),
                                    plotOutput("plot1"),
                                    plotOutput("plot2"),
                                    dataTableOutput("table1")
                                )
                            )
                   ), 
                   tabPanel("Опис моделі",
                            mainPanel( 
                                includeMarkdown("about.Rmd") 
                            ) 
                   ) 
) 
)    
