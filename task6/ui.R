library(shiny)
library(markdown)


shinyUI(navbarPage("Задача №6", 
                   tabPanel("Результат", 
                            
                            # Sidebar 
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("w1",
                                                "Заробітна плата на заводі:",
                                                min = 10,
                                                max = 100,
                                                value = 30),
                                    sliderInput("w2",
                                                "Заробітна плата на 1 фірмі:",
                                                min = 10,
                                                max = 100,
                                                value = 40),
                                    sliderInput("w3",
                                                "Заробітна плата на 2 фірмі:",
                                                min = 10,
                                                max = 100,
                                                value = 50),
                                    sliderInput("w4",
                                                "Заробітна плата на 3 фірмі:",
                                                min = 10,
                                                max = 100,
                                                value = 60),
                                    sliderInput("p_goods",
                                                "Ціна кінцевого товару:",
                                                min = 50,
                                                max = 200,
                                                value = 50),
                                    checkboxInput("check_t", "Використовувати податок на оборот", FALSE),
                                    sliderInput("t_oborot1",
                                                "Податок на оборот заводу:",
                                                min = 1,
                                                max = 10,
                                                value = 1),
                                    sliderInput("t_oborot2",
                                                "Податок на оборот 1 фірми:",
                                                min = 1,
                                                max = 20,
                                                value = 2),
                                    sliderInput("t_oborot3",
                                                "Податок на оборот 2 фірми:",
                                                min = 1,
                                                max = 20,
                                                value = 2),
                                    sliderInput("t_oborot4",
                                                "Податок на оборот 3 фірми:",
                                                min = 1,
                                                max = 20,
                                                value = 2),
                                    checkboxInput("check_kartel", "Фірми об'єднуються у картель", FALSE),                                    
                                    checkboxInput("check_zao", "Люди роблять накопичення", FALSE), 
                                    sliderInput("mrc",
                                                "Гранична норма споживання",
                                                min = 50,
                                                max = 100,
                                                value = 80),
                                    
                                    submitButton("Submit") 
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden;}",
                                               ".shiny-output-error:before { visibility: hidden;}"),
                                    h3("Result"),
                                    textOutput("text1")

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


