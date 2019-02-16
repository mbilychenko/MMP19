library(shiny)
library(markdown)

shinyUI(navbarPage("Задача №2", 
                   tabPanel("Результат", 
                            
                            # Sidebar 
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("m",
                                            "Кількість періодів:",
                                            min = 10,
                                            max = 1000,
                                            value = 100),
                                sliderInput("n",
                                            "Населення:",
                                            min = 10,
                                            max = 1000,
                                            value = 10),
                                sliderInput("b",
                                            "Початковий дохід:",
                                            min = 50,
                                            max = 5000,
                                            value = c(100, 1000)),
                                sliderInput("r",
                                            "Ріст ціни кожного періоду, %:",
                                            min = 1,
                                            max = 50,
                                            value = 1),
                                numericInput("p1", 
                                             h3("Price 1"), 
                                             value = 1, 
                                             min = 1, 
                                             max = 100),
                                numericInput("p2", 
                                             h3("Price 2"), 
                                             value = 50, 
                                             min = 1, 
                                             max = 100), 
                                selectInput("method",
                                            "Оберіть метод для обрахунку ефекту доходу",
                                            choices = list("За Cлуцьким" = 1, "За Хіксом" = 2), 
                                            selected = 1),
                                
                              checkboxInput("showPlot_n", "Показати/Сховати графіки", value = TRUE),
                                
                                submitButton("Submit") 
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden;}",
                                           ".shiny-output-error:before { visibility: hidden;}"),
                                fluidRow( 
                                  verticalLayout(
                                    h3("Графіки ціна/споживання кожного товару"),
                                    splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2")), 
                                    h3("Споживання товарів у часі"),
                                    plotOutput("plot3"),
                                    h3("Значення індексів у кожному періоді"),
                                    plotOutput("plot4"),
                                    h3("Сумарна величина компенсації у часі"),
                                    plotOutput("plot5"),
                                    downloadButton(outputId = "download_data",
                                                   label = "Завнтажити дані")
                                  )
                                )

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
