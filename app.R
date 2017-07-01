#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Approximating e with Euler's Algorithm"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of steps:",
                     min = 1,
                     max = 50,
                     value = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     n <- input$bins
     delta <- 1/n
     f <- numeric(n + 1)
     f[1] <- 1
     for (i in 2:(n + 1)) {
       print(i)
       f[i] = f[i - 1] + delta * f[i - 1]
     }
     data <- data.frame(x = 1:(n+1), y=f)
     ggplot(data) + geom_point(aes(x=x, y=y)) + ylim(0, 3) +
      geom_segment(x=0, y=exp(1), xend=n+1, yend=exp(1), col="red")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

