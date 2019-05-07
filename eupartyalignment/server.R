library(shiny)
# generate dataframe for comparison of userinputs with the other parties
library(corrplot)
library(RColorBrewer)
# data ==== 
parties <- c("cdu","sdp","gruen","linke","afd","fdp","freiewaehler","piraten",
             "npd","familienpart","oedp","diepartei",
             "mlpd","dritteweg","diegrauen","diehumanisten","volt", as.character(input$username))
data <- as.data.frame(matrix(NA, 38, length(parties)))
names(data) <- parties
# data copied from www.wahl-o-mat.de/europawahl2019/ ####
data$cdu <- c(F,NA,T,NA,F,F,T,NA,T,F,T,T,F,T,NA,F,NA,T,T,F,T,T,NA,NA,T,F,T,F,NA,F,NA,T,T,T,T,F,F,F)
data$sdp <- c(T,T,T,T,F,T,T,F,F,T,T,NA,T,F,T,F,F,T,T,F,T,T,T,T,NA,T,F,F,F,T,F,T,T,T,T,F,T,T)
data$gruen <- c(T,T,F,T,F,T,T,F,F,T,T,F,T,F,T,F,F,T,T,F,F,T,T,T,NA,T,F,F,F,F,T,T,T,T,T,F,T,T)
data$linke <- c(T,T,F,T,F,T,T,F,F,F,T,F,T,F,T,F,F,NA,T,F,NA,F,T,T,F,T,F,T,F,T,T,T,F,T,T,F,F,T)
data$afd <- c(F,F,F,F,T,F,NA,T,T,F,F,NA,F,F,F,T,T,F,F,T,T,F,F,F,T,F,T,F,T,T,NA,NA,F,F,F,T,F,F)
data$fdp <- c(NA,F,T,F,F,NA,F,T,T,T,NA,F,F,T,F,F,T,T,T,NA,T,T,F,NA,T,F,F,F,F,F,F,T,T,T,NA,F,T,F)
data$freiewaehler <- c(T,T,T,NA,F,F,T,F,T,F,T,T,F,T,F,F,F,T,T,F,F,T,F,NA,T,T,T,F,T,F,F,T,F,T,F,F,F,F)
data$piraten <- c(T,T,F,T,F,T,T,F,F,T,T,F,T,F,F,F,F,T,T,F,T,F,T,T,F,F,F,F,F,F,T,T,T,T,T,F,T,T)
data$npd <- c(F,F,F,T,T,F,T,F,T,F,F,T,F,F,F,T,NA,F,F,T,F,T,F,F,F,F,T,F,T,T,T,F,F,F,T,T,F,F)
data$familienpart <- c(T,NA,T,NA,F,T,T,F,T,F,T,T,T,F,NA,F,F,F,T,F,NA,T,T,F,T,T,T,F,F,T,T,T,T,T,T,F,F,T)
data$oedp <- c(T,T,F,T,F,T,T,F,NA,F,T,F,T,NA,F,F,F,NA,T,F,F,T,T,NA,T,T,NA,F,F,T,T,T,F,T,T,F,F,T)
data$diepartei <- c(T,T,F,T,F,T,T,F,T,F,T,F,T,F,T,F,F,T,T,F,F,F,T,T,T,T,F,F,F,T,T,T,T,T,T,F,T,T)
data$mlpd <- c(T,T,F,NA,NA,T,T,F,F,T,NA,F,T,F,NA,NA,F,F,T,F,T,F,T,T,F,T,F,T,F,T,T,T,NA,F,T,F,NA,T)
data$dritteweg <- c(F,F,F,T,T,F,T,F,T,F,F,F,T,F,F,T,NA,F,F,T,F,F,F,F,F,T,F,T,T,T,NA,F,F,F,T,T,F,F)
data$diegrauen <- c(T,T,T,T,F,F,T,F,NA,F,NA,T,T,T,T,F,F,F,T,T,T,T,T,T,T,T,NA,NA,T,T,T,NA,NA,T,T,T,F,T)
data$diehumanisten <- c(F,T,T,F,F,F,NA,T,F,T,T,F,T,NA,F,F,T,T,T,F,T,T,T,T,T,F,F,F,F,F,F,T,T,T,NA,F,T,T)
data$volt <- c(F,T,T,T,F,T,T,T,F,T,T,F,T,NA,T,F,T,T,T,F,T,T,T,T,F,T,F,F,F,F,T,T,T,T,T,F,T,T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # parse data and create correlationplot in here, preprocess data in server but outside of reactive scope
    output$uservector <- renderText({
        # parsing userinputs
        userv <- c(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9,input$q10,
                   input$q11,input$q12,input$q13,input$q14,input$q15,input$q16,input$q17,input$q18,input$q19,input$q20,
                   input$q21,input$q22,input$q23,input$q24,input$q25,input$q26,input$q27,input$q28,input$q29,input$q30,
                   input$q31,input$q32,input$q33,input$q34,input$q35,input$q36,input$q37,input$q38)
        userv[userv=="Agree"] <- 1
        userv[userv=="Neutral"] <- 0
        userv[userv=="Disagree"] <- -1
        userv <- as.numeric(userv)
        parties <- c("cdu","sdp","gruen","linke","afd","fdp","freiewaehler","piraten",
                     "npd","familienpart","oedp","diepartei",
                     "mlpd","dritteweg","diegrauen","diehumanisten","volt", as.character(input$username))
        
    })
    
    output$parties <- renderText({
        
    })
    
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
