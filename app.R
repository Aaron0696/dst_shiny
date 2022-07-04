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
    output$stimuli <- renderPrint({"INSTRUCTIONS"})
    
    
    tohide2 <- reactiveTimer(intervalMs = 7000)
    params <- reactiveValues(counter = 0)
    
    observeEvent(input$nex,{
        params$counter <- 0
        output$ansbox <- NULL
        output$stimuli <- renderPrint({rand_df[input$nex]})
        print("next")
        answers[[input$nex]] <<- input$ans
        output$allans <- renderPrint({answers})
        
        tohide <<<- reactiveTimer(intervalMs = 6000)
    })
    
    observeEvent(tohide(),
                 {
                     print(params$counter)
                     if(input$nex != 0){
                         output$stimuli <- renderPrint({"HIDDEN"})
                         if(params$counter == 0){
                             output$ansbox <- renderUI({
                                 numericInput("ans", label = h3("Answer"), value = 0)})
                         }
                     }})
    
    observeEvent(tohide2(),{
        print("updating params")
        print(tohide2())
        params$counter <- params$counter + 1
        params$counter <- ifelse(params$counter > 0, 1, 0)})
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
