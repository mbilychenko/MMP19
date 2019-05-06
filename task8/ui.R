
library(shiny)

shinyUI(navbarPage("Задача №8", 
                   tabPanel("Результат", 
                            
                            # Sidebar 
                            sidebarLayout(
                              sidebarPanel(
                                h3("Початкові параметри"),
                                selectInput("type_t", "Variable:",
                                            c("ПДВ" = "1",
                                              "Податок на доходи" = "2",
                                              "Податок на прибуток" = "3",
                                              "Податок на землю" = "4"),
                                            selected = "3"),
                                submitButton("Submit") 
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden;}",
                                           ".shiny-output-error:before { visibility: hidden;}"),
                                h3("Результат"),
                                tableOutput('table1')
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
