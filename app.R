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
    uiOutput("ansbox"),
    fluidRow(column(3, verbatimTextOutput("allans")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    answers <- list()
    EventTime <- reactiveVal(Sys.time())
    
    observeEvent(input$nex,{
        EventTime(Sys.time() + 2)
    })
    
    output$stimuli <- renderPrint({
        timeLeft <<- round(difftime(EventTime(), Sys.time(), units = 'secs'))
        if(timeLeft > 0){
            invalidateLater(1000)
            rand_df[input$nex]
        } else {
            if(input$nex != 0){
                "HIDDEN"
            }
        }
    })
    
    output$ansbox <- renderUI({
        timeLeft <<- round(difftime(EventTime(), Sys.time(), units = 'secs')) + 1
        if(timeLeft > 0){
            invalidateLater(1000)
        } else {
            if(input$nex != 0){
                textInput("ans", label = h3("Answer"), value = 0)
            }
        }
    })
    
    
    observeEvent(input$nex,{
        myindex <- ifelse((input$nex - 1) < 1, 1, input$nex - 1)
        answers[[myindex]] <<- input$ans
        output$allans <- renderPrint({answers})  
    })
    
    
    
    # observeEvent(input$nex,{
    #     print("next")
    #     timeLeft <- round(difftime(EventTime(), Sys.time(), units='secs'))
    #     print(timeLeft)
    #     if(timeLeft > 0){
    #         print("invalidating")
    #         invalidateLater(500)
    #         output$ansbox <- NULL
    #         output$stimuli <- renderPrint({rand_df[input$nex]})
    #         answers[[input$nex]] <<- input$ans
    #         output$allans <- renderPrint({answers})
    #     } else {
    #         if(input$nex != 0){
    #             output$stimuli <- renderPrint({"HIDDEN"})
    #             if(params$counter == 0){
    #                 output$ansbox <- renderUI({
    #                     numericInput("ans", label = h3("Answer"), value = 0)})
    #             }
    #     }}})

    
    # observeEvent(tohide2(),{
    #     print("updating params")
    #     print(tohide2)
    #     params$counter <- params$counter + 1
    #     params$counter <- ifelse(params$counter > 0, 1, 0)})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
