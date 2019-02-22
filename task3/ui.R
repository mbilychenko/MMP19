
library(shiny)

shinyUI(navbarPage("Задача №3", 
                   tabPanel("Результат", 
                            
                            # Sidebar 
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("n",
                                            "Кількість фірм:",
                                            min = 10,
                                            max = 200,
                                            value = 100),
                                sliderInput("alpha0",
                                            "Початкові межі коефіцієнта а0:",
                                            min = 0,
                                            max = 5,
                                            value = c(0.5, 3)),
                                sliderInput("alpha1",
                                            "Початкові межі коефіцієнта а1:",
                                            min = 0,
                                            max = 2,
                                            value = c(0, 1)),
                                sliderInput("alpha2",
                                            "Початкові межі коефіцієнта а2:",
                                            min = 0,
                                            max = 2,
                                            value = c(0, 1)),
                                selectInput("error_function",
                                            "Оберіть метод для обрахунку похибки",
                                            choices = list("Абсолютна/Absolute" = 1, "Квадратична/Quadratic" = 2), 
                                            selected = 1),

                                submitButton("Submit") 
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden;}",
                                           ".shiny-output-error:before { visibility: hidden;}"),
                                h3("Result"),
                                textOutput("text1"),
                                h3("Download data"),
                                downloadButton(outputId = "download_data",
                                               label = "Завантажити дані")
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
