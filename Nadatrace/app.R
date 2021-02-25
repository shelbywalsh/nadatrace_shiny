
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
                           
                           tabPanel("About Nada and the Tool",
                                    
                                    mainPanel(
                                        
                                        h3("Background (information about Nada, and the motivation behind measuring their carbon footprint.)"),
                                        
                                        h5("Nada is a Canadian grocery store that offers sustainably sourced foods, zero waste lifestyle products, and a package-free shopping experience. The business is also committed to environmental and social justice. One of the ways Nada is working to further reduce its environmental impacts is quantifying the carbon footprint of its business model"),
                                        
                                        h3("About the Tool"),
                                        
                                        h5("This set of tools allows the user to visually explore the carbon footprint of Nada from 2019 and 2020. 4 interactive data visualization tools are included in the subsequent tabs:"),
                                        
                                        h5("Tab 1: Scope 3 CO2 emissions broken down by food category or supplier;"),
                                        
                                        h5("Tab 2: Magnitude of carbon offset from food waste diversion (relative to Scope 1,2 or 3);"),
                                        
                                        h5("Tab 3: Change in emissions between 2019 and 2020;"),
                                        
                                        h5("Tab 4: Interactive treemap (or stacked bar chart) of total carbon footprint."),
                                        
                                        h5("Data Source: Nada Grocery.")
                                        
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
                           tabPanel("Emissions Sources"  # Tab names need work 
                                    
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
