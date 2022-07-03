#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# create 10 sequences of number starting from length 1 to 10
rand_df <- c()
for(l in 1:10){
   rand_df <-  c(rand_df, 
                 paste0(round(runif(l, min = 0, max = 9)),
                 collapse = " "))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
            
    actionButton("nex", label = "Next"),
    
    
    fluidRow(column(3, verbatimTextOutput("stimuli"))),
    # fluidRow(column(3, verbatimTextOutput("nex"))),
    # numericInput("ans", label = h3("Answer"), value = 0),
    uiOutput("test"),
    fluidRow(column(3, verbatimTextOutput("allans")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    answers <- list()
    
    tohide <- reactiveTimer(intervalMs = 5000)
    
    observeEvent(tohide(),
                 {
                     
                     output$stimuli <- renderPrint({NULL}) 
                     output$test <- renderUI({
                         numericInput("ans", label = h3("Answer"), value = 0)
                     })
                     
                 })
    
    observeEvent(input$nex,{
        output$test <- NULL
        
        output$stimuli <- renderPrint({rand_df[input$nex]})
        answers[[input$nex]] <<- input$ans
        # output$nex <- renderPrint({
        #     paste(input$nex, answers[[input$nex]])
        # })
        output$allans <- renderPrint({answers})
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
