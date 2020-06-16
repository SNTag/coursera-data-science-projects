## ----self_running, eval=FALSE, include = FALSE--------------------------------
## knitr::purl("./simple-shiny-application.Rmd")
## source("./simple-shiny-application.R")


## ----ui, echo = F, eval = T---------------------------------------------------
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

                                        # Define UI for dataset viewer application
ui <- fluidPage(

                                        # Sidebar with options selectors
    sidebarLayout(
        sidebarPanel(
            helpText("This application predicts the price of a diamond based on its characteristics."),
            h3(helpText("Select:")),
            numericInput("mpg", label = h4("MPG"), step = 0.01, value = 2),
            sliderInput("wt", label = h4("Weight"), min = 1.513, max = 5.424, value = 3.217),
            selectInput("cyl", label = h4("Number of Cylinders"),
                   choices = list("4" = 4, "6" = 6, "8" = 8)),
            sliderInput("disp", label = h4("Displacement (cu.in)"), min = 71.1, max = 472, value = 230.7)
        ),

                                        # Show a plot with mpg and regression line
        mainPanel(
            plotOutput("distPlot"),
            h4("Predicted value of mpg is:"),
            h3(textOutput("result"))
        )
    )
)



## ----server, echo = F, eval = T-----------------------------------------------
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
data(mtcars)

data_cars <- mtcars[,c(1:3,6)]

# Define server logic required to summarize and view the selected dataset
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # select diamonds depending of user input
        data_cars <- filter(mtcars, grepl(input$wt, weight), grepl(input$cyl, cycle), grepl(input$disp, display))
        # build linear regression model
        fit <- lm( hp ~ mpg, data_cars)
        # predicts the price
        pred <- predict(fit, newdata = data.frame(mpg = input$mpg,
                                                  wt = input$wt,
                                                  cyl = input$cyl,
                                                  disp = input$disp))
        # Drow the plot using ggplot2
        plot <- ggplot(data=data_cars, aes(x=mpg, y = hp))+
            geom_point(aes(color = wt), alpha = 0.3)+
            geom_smooth(method = "lm")+
            geom_vline(xintercept = input$mpg, color = "red")+
            geom_hline(yintercept = pred, color = "green")
       plot
    })
    output$result <- renderText({
        # renders the text for the prediction below the graph
        data_cars <- filter(mtcars, grepl(input$wt, wt), grepl(input$cyl, cycle), grepl(input$disp, display))
        fit <- lm( hp~mpg, data_cars)
        pred <- predict(fit, newdata = data.frame(mpg = input$mpg,
                                                  wt = input$wt,
                                                  cyl = input$cyl,
                                                  disp = input$disp))
        res <- paste(round(pred, digits = 2), "$")
        res
    })

}


shinyApp(ui = ui, server = server)

