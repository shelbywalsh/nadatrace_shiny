
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
                navbarPage(h2("NADAtrace Carbon Footprint Market Tool"),
                           
                           tabPanel("About the Tool",
                                    
                                    mainPanel(
                                        
                                        h3("Background (information about Nada, and the motivation behind measuring their carbon footprint.)"),
                                        
                                        h5("Background body text: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                                        
                                        h3("About the Tool"),
                                        
                                        h5("This set of tools allows the user to visually explore the carbon footprint of Nada, a zero-waste grocery store, frok 2019 and 2020. 4 interactive data visualization tools are included in the subsequent tabs:"),
                                        
                                        h5("Tab 1: Scope 3 CO2 emissions broken down by food category or supplier;"),
                                        
                                        h5("Tab 2: Magnitude of carbon offset from food waste diversion (relative to Scope 1,2 or 3);"),
                                        
                                        h5("Tab 3: Change in emissions between 2019 and 2020;"),
                                        
                                        h5("Tab 4: Interactive treemap (or stacked bar chart) of total carbon footprint."),
                                        
                                        h5("Data Source: Nada.")
                                        
                                    )
                                        
                                    ),
                           tabPanel("Scope 3 Emissions",
                                    
                                    sidebarLayout(
                                        
                                        sidebarPanel(
                                                
                                        ),
                                        
                                        mainPanel()
                                    )
                                    ),
                           tabPanel("Food Waste Diversion"),
                           tabPanel("2019 vs. 2020"),
                           tabPanel("Where Are Emissions Coming From?"  # Tab names need work 
                                    
                                    )  
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
