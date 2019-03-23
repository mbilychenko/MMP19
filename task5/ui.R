library(shiny)
library(markdown)



shinyUI(navbarPage("Задача №5", 
                   tabPanel("Результат", 
                            fluidPage(
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden;}",
                                           ".shiny-output-error:before { visibility: hidden;}"),
                                textOutput("text1"),
                                plotOutput("plot1"),
                                
                                hr(),
                                
                                fluidRow(
                                    column(3,
                                           h3("Загальні параметри:"),
                                           sliderInput("x_p_a",
                                                       "Початкова ціна чорного гаджету компанії 'Надкушена груша', гр. од",
                                                       min = 250,
                                                       max = 400,
                                                       value = 300),
                                           sliderInput("x_p_b",
                                                       "Початкова ціна рожевого гаджету компанії 'Надкушена груша', гр. од",
                                                       min = 250,
                                                       max = 500,
                                                       value = 350),
                                           sliderInput("y_p",
                                                       "Початкова ціна гаджету китайських компаній, гр. од",
                                                       min = 50,
                                                       max = 250,
                                                       value = 150),
                                           checkboxInput("showPlot_n", "Показати/Сховати графік 1", value = TRUE),
                                           submitButton("Submit")
                                    ),
                                    column(4, offset = 1,
                                           h3("Стратегія компанії 'Надкушена груша'"),
                                           checkboxInput("check_x_mark", "Чи потрібно збільшувати витрати на маркетинг?", value = TRUE),
                                           checkboxInput("check_x_factory", "Чи потрібно будувати нові заводи?", value = TRUE),
                                           checkboxInput("check_x_price", "Чи потрібно підвищувати ціну?", value = FALSE),
                                           
                                           sliderInput("check_cost_mark",
                                                       "Обсяги збільшення витрат на маркетинг, гр. од",
                                                       min = 0,
                                                       max = 10^7,
                                                       value = 5*10^6),
                                           sliderInput("check_n_x_new_a",
                                                       "Кількість нових заводів для чорних гаджетів",
                                                       min = 0,
                                                       max = 5,
                                                       value = 2),
                                           sliderInput("check_n_x_new_b",
                                                       "Кількість нових заводів для рожевих гаджетів",
                                                       min = 0,
                                                       max = 5,
                                                       value = 2), 
                                           sliderInput("check_x_price_incr",
                                                       "Ріст ціни кожного періоду, %",
                                                       min = 0,
                                                       max = 50,
                                                       value = 10)
                                    ),
                                    column(4,
                                           h3("Стратегія китайських компаній"),
                                           checkboxInput("check_y", "Чи потрібно зупиняти будівництвоб коли прибутки падають?", value = TRUE),
                                           sliderInput("check_y_factory",
                                                       "Кількість нових заводів для китайських гаджетів кожного періоду",
                                                       min = 0,
                                                       max = 5,
                                                       value = 5)                    
                                           
                                           
                                    )
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
