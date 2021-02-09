
library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib)

nada_theme <- bs_theme(
    bg = "#ff6666",
    fg = "white",
    primary = "black",
    base_font = font_google("Oswald"))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = nada_theme,
                # Application title
                navbarPage("NADAtrace Carbon Footprint Market Tool",
                           tabPanel("Visual 1"),
                            tabPanel("Visual 2"),
                            tabPanel("Visual 3"),
                           tabPanel("Visual 4")
                           )
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
