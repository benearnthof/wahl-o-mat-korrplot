library(shiny)
# defining number of questions to answer
numberelements <- 38
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Questions"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            textInput("username", "Your name:", value = "username"),
            
            lapply(1:numberelements, function(i) {
                selectInput(paste0("q", i), paste0("Question ", i),
                            choices = c("Agree", "Neutral", "Disagree"), multiple = F, width = '40%')
            })
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("uservector"),
            textOutput("parties")
        )
    )
))
