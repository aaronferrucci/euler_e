
library(shiny)
library(ggplot2)
ui <- fluidPage(
   # Application title
   titlePanel("Approximating e with Euler's Algorithm"),
   
   # Sidebar with a slider input for number of steps 
   sidebarLayout(
      sidebarPanel(
        sliderInput("steps",
                     "Number of steps:",
                     min = 1,
                     max = 50,
                     value = 10)
      ),
      # Show the plot
      mainPanel(
        plotOutput("distPlot"),
        a("source code on github", href="https://github.com/aaronferrucci/euler_e", target="_blank")
      )
   )
)

server <- function(input, output) {
   output$distPlot <- renderPlot({
     n <- input$steps
     delta <- 1/n
     f <- numeric(n + 1)
     f[1] <- 1
     for (i in 2:(n + 1)) {
       print(i)
       f[i] = f[i - 1] + delta * f[i - 1]
     }
     data <- data.frame(step = 1:(n+1), y=f)
     p <- ggplot(data[1:n,]) +
       geom_point(aes(x=step, y=y)) +
       geom_segment(x=0, y=exp(1), xend=n+2, yend=exp(1), col="green") +
       scale_y_continuous(limits=c(0, 3), labels = c(0, 1, 2, "2.718...", 3), breaks=c(0, 1, 2, exp(1), 3))
     p + geom_point(data=data[n+1,], aes(x=step, y=y), col="red", shape=8, size=3)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

