
library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib)
library(janitor)
library(here)

nada_theme <- bs_theme(
    bg = "#ff6666",
    fg = "white",
    primary = "black",
    base_font = font_google("Oswald"))

# Load data
total_emissions_19 <- read_csv(here("Nadatrace","2019_all_emissions.csv")) %>% 
    clean_names()

total_emissions_20 <- read_csv(here("Nadatrace","2020_all_emissions.csv")) %>% 
    clean_names()

total_emissions <- read_csv(here("Nadatrace","allemissions.csv")) %>% 
    clean_names() %>% 
    mutate(year = "2020")

cf <- total_emissions %>% 
    filter(!scope == "OFFSETS") %>% 
    mutate(year = as.character(year))

pur_20 <- read_csv(here("Nadatrace", "purchased_goods_2020.csv")) %>% 
    clean_names()

scope3 <- total_emissions %>% 
    filter(scope == "SCOPE 3") %>% 
    mutate(year = as.character(year))

food_waste <- total_emissions %>% 
    filter(scope == "OFFSETS") %>% 
    mutate(year = as.character(year))

food_waste_pos <- food_waste %>% 
    mutate_if(is.numeric, funs(. * -1))

# Define UI for application that draws a histogram
ui <- fluidPage (theme = nada_theme,
                # Application title
                navbarPage(h2("NADAtrace Carbon Footprint Visualization Tool"),
                           
                           tabPanel("About Nada and the Carbon Footpring Tool",
                                    
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
                           tabPanel("2019 vs. 2020",
                                    sidebarLayout(
                                        mainPanel("Graph description",
                                                  plotOutput("tot_em_plot"
                                                  )
                                        ),
                                        sidebarPanel(
                                            "Explaning this part of the tool",
                                            checkboxGroupInput(
                                                inputId = "footprint_scope",
                                                label = "Choose Scope to compare carbon footprint:",
                                                choices = c("SCOPE 1", "SCOPE 2", "SCOPE 3"))
                                        )
                                    )
                                    
                           ),
                           
                           tabPanel(
                               "Scope 3 Emissions",
                                    
                                sidebarLayout(
                                        
                                    sidebarPanel(
                                        "Explaining this part of the tool",
                                        checkboxGroupInput(
                                            inputId = "scope3_subgroup",
                                            label = "Choose Scope 3 category to view carbon footprint:",
                                            choices = c("Transportation", "Purchased Goods & Services")
                                            )
                                        ),
                                        
                                    mainPanel(
                                        "Graph description",
                                        plotOutput("scope3_plot")
                                    )
                                    )
                                    ),
                           
                           tabPanel("Purchased Goods and Services"  # Tab names need work 
                                    
                           ), 
                           
                           tabPanel("Food Waste Diversion",
                                    sidebarLayout(
                    
                                        sidebarPanel("Explaning this part of the tool",
                                    checkboxGroupInput(
                                        inputId = "food_waste_sub_group",
                                        label = "Select Waste", 
                                        choices = c("Compost", "Café"
                                                    )
                                    )),
                                    
                                    mainPanel(
                                        "Food Waste description",
                                        plotOutput("food_waste_plot")
                                    )
                                    )
                                    )
                        
                           
                           ))
                


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # graph for "food waste" tab:
    
    # reactive data frame
    food_waste_reactive <- reactive({
        food_waste %>% 
            filter(category %in% input$food_waste_sub_group)
    })
    
    # output plot 
    output$food_waste_plot <- renderPlot({
        ggplot(data = food_waste_reactive(), aes(x = year, y = kg_co2e, fill = sub_group)) +
            geom_col() +
            theme_minimal()
    })
    

# graph for "scope 3 emissions" tab:
    
    # reactive data frame
    scope3_reactive <- reactive({
        scope3 %>% 
            filter(category %in% input$scope3_subgroup)
    })
    
    # output plot 
    output$scope3_plot <- renderPlot({
        ggplot(data = scope3_reactive(), aes(x = year, y = kg_co2e, fill = sub_group)) +
            geom_col() +
            theme_minimal()
    })
    
# graph for "2019 vs 2020" tab:    
    
    # reactive data frame
    tot_em_reactive <- reactive({
        cf %>% 
            filter(scope %in% input$footprint_scope)
    })

    # output plot #1
    output$tot_em_plot <- renderPlot({
        ggplot(data = tot_em_reactive(), aes(x = year, y = kg_co2e)) +
            geom_col(aes(fill = category)) +
            theme_minimal()
    })
    
# graph for "Compare Footprints" tab
       total_emission_reactive <- reactive({
        total_emissions %>% 
            filter(year %in% input$year_emissions)
    })
    
    output$total_emissions <- renderPlot({
        ggplot(data = total_emissions(), aes(x = scope, y = kg_co2e)) +
            geom_col(aes(color = sub_group))
    })
}

shinyApp(ui = ui, server = server)
