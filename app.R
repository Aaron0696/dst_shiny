library(shiny)
library(shinythemes)

# create [x] sequences of number starting from length 1 to [x]
num_seq <- 12
set.seed(Sys.time())
rand_df <- c()
for(l in 2:(num_seq + 1)){
   rand_df <-  c(rand_df, 
                 paste0(round(runif(l, min = 0, max = 9)),
                 collapse = " "))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # select theme
    theme = shinytheme("flatly"),
    # title
    titlePanel(h1("Forward Digit Span Task (FDST)")),
    # spiel
    fluidRow(
        column(
            wellPanel(h3("Please do not click next without reading the instructions!"),
                      "The FDST was designed to measure working memory spans in the field of Cognitive Psychology.", 
                      br(),
                      "I translated the task into an R Shiny app for fun, please do not use this for any rigourous assessments or purposes.",
                      br(),
                      br(),
                      "Below are the instructions for the task:",
                      br(),
                tags$ul(
                    tags$li("Do not click 'Next' until you are ready to begin"), 
                    tags$li("A sequence of numbers will be presented in the window below"),
                    tags$li("Your job is to memorize the sequence of numbers before they disappear in ~6 seconds"),
                    tags$li("When the numbers become hidden, an input box will appear after another ~2 seconds, where you must input the sequence of numbers as you recalled it"),
                    tags$li("You do not need to input any space or separators between numbers in your input"),
                    tags$li("Click on 'Next' to submit your answer and receive the next sequence"),
                    tags$li("You will receive a point for each sequence recalled correctly"),
                    tags$li("The sequence will progressively get longer, up to 12 numbers!")
                ),
                "For example, if the sequence shown is '0 1 2 3', you can respond with '0123' or '0 1 2 3'",
                br(),
                "Click on 'Next' only when you are ready to begin"), 
            width = 8),
        column(
            wellPanel("Access the README and codes on ", tags$a(href = "https://github.com/Aaron0696/dst_shiny", "Github."),
                      br(),
                      "Connect with me on ", tags$a(href = "https://www.linkedin.com/in/aaron-lim-b30898135/", "LinkedIn.")),
            width = 4)),
    # next button
    fluidRow(column(5, uiOutput("nex"))),
    br(),
    # stimuli presentation
    fluidRow(column(8, verbatimTextOutput("stimuli"))),
    fluidRow(column(8, uiOutput("ansbox"))),
    br(),
    # uiOutput("ansbox"),
    fluidRow(column(3, verbatimTextOutput("score"))),
    fluidRow(column(3, verbatimTextOutput("moreinfo"))),
    br()
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # store answers
    answers <- list()
    # reactive event time for timer
    EventTime <- reactiveVal(Sys.time())
    # next button
    output$nex <- renderUI({actionButton("nex", label = "Next")})
    # timer logic, give 6s before hiding stimuli
    observeEvent(input$nex,{
        # give 3 seconds for easier sequences, and 6 for harder ones
        EventTime(Sys.time() + ifelse(input$nex > 4, 6, 3))
    })
    
    # stimuli
    output$stimuli <- renderPrint({
        timeLeft <<- round(difftime(EventTime(), Sys.time(), units = 'secs'))
        if(ifelse(is.null(input$nex), -1, input$nex) == (num_seq + 1)){
            "END OF TASK"
        } else {
            # if there is still time left, show the sequence
            if(timeLeft > 0){
                invalidateLater(1000)
                rand_df[input$nex]   
            } else { # else hide it
                if(ifelse(is.null(input$nex), -1, input$nex) == 0){
                    "Sequences will appear here!"
                } else {"### THE SEQUENCE HAS BEEN HIDDEN ####"}
            }
        }
        })
    # box for inputting answers
    output$ansbox <- renderUI({
        if(ifelse(is.null(input$nex), -1, input$nex) == (num_seq + 1)){
            output$ans <- NULL
        } else {
            timeLeft <<- round(difftime(EventTime(), Sys.time(), units = 'secs')) + 2
            if(timeLeft > 0){
                invalidateLater(1000)
            } else {
                if(ifelse(is.null(input$nex), -1, input$nex) != 0){
                    textInput("ans", label = h4("Your Answer"), value = 0)
                }
            }
        }
    })
    # saving the answers
    observeEvent(input$nex,{
        myindex <- ifelse((input$nex - 1) < 1, 1, input$nex - 1)
        if(is.null(input$ans)){
            answers[[myindex]] <<- "No_Response"
        } else {
            answers[[myindex]] <<- input$ans
        }
        output$allans <- renderPrint({answers})
        # print(input$nex)
    })

    # calculating score
    observeEvent(input$nex, {
        # when final button is pressed
        if(input$nex == (num_seq + 1)){
            # disable button
            output$nex <- renderUI({actionButton("does_nothing", label = "End")})
            # compute score
            answers_unlist <- gsub(" ", "", unlist(answers)) 
            stimuli_unspace <- gsub(" ", "", rand_df)
            score <- sum(answers_unlist == stimuli_unspace)
            
            output$score <- renderPrint({paste0("Your score is ",
                                                score,
                                                " out of ",
                                                num_seq,
                                                ".")})
            output$moreinfo <- renderPrint({
                data.frame(Your_Answer = answers_unlist,
                           Correct_Answer = stimuli_unspace,
                           Score = as.numeric(answers_unlist == stimuli_unspace))
            })
            # debugging
            # print(answers_unlist)
            # print(stimuli_unspace)
            # print(score)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
