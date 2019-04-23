
library(shiny)

shinyUI(navbarPage("Задача №7", 
                   tabPanel("Результат", 
                            
                            # Sidebar 
                            sidebarLayout(
                              sidebarPanel(
                                h3("Початкові параметри"),
                                sliderInput("f",
                                            "Тяжкість покарання, гр.од:",
                                            min = 50000,
                                            max = 1000000,
                                            value = 100000),
                                sliderInput("p",
                                            "Імовірність викриття злочину:",
                                            min = 1,
                                            max = 100,
                                            value = 25),
                                sliderInput("b",
                                            "Ставка податку, %:",
                                            min = 1,
                                            max = 40,
                                            value = 10),
                                h3("Використання оптимізаційних алгоритмів"),
                                checkboxInput("choise_f_adj", "Оптимізація тяжкості покарання", value = TRUE),
                                checkboxInput("choise_p_adj", "Оптимізація ймовірності викриття злочину", value = TRUE),

                                submitButton("Submit") 
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden;}",
                                           ".shiny-output-error:before { visibility: hidden;}"),
                                h3("Результат"),
                                textOutput("text1"),
                                h3("Динаміка рівня злочинності"),
                                plotOutput("plot1")
                                )
                                
                              
                            )
                   ), 
                   tabPanel("Про модель/задачу",
                            mainPanel( 
                              withMathJax(includeMarkdown("about.Rmd"))
                            ) 
                   ) 
) 
)    
