
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
    clean_names() 

cf <- total_emissions %>% 
    filter(!scope == "OFFSETS") %>% 
    mutate(year = as.character(year))

pur_19 <- read_csv(here("Nadatrace", "purchased_goods_19.csv")) %>% 
    clean_names()%>% 
    mutate(year = "2019")

pur_20 <- read_csv(here("Nadatrace", "purchased_goods_20.csv")) %>% 
    clean_names()%>% 
    mutate(year = "2020")

puch <- rbind(pur_19, pur_20) %>% 
    mutate(year = as.character(year))

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
                           
                           navbarMenu("Background",
                           
                           tabPanel("About Nada",
                                    
                                    mainPanel(
                                        
                                        h3("Nada is a package-free grocery store on a mission"),
                                        
                                        img(src = "nada1.jpg", height = 251, width = 444),
                                        
                                        h5("Nada is a grocery store in Vancouver, BC that offers sustainably sourced foods, zero waste lifestyle products, and a package-free shopping experience. The business is also committed to environmental and social justice. One of the ways Nada is working to further reduce its environmental impacts is quantifying the carbon footprint of its business model"))),

                                        
                            tabPanel("About this tool",
                                     mainPanel(
                                        h3("About the Tool"),
                                        
                                        h5("This set of tools allows the user to visually explore the carbon footprint of Nada from 2019 and 2020. 4 interactive data visualization tools are included in the subsequent tabs:"),
                                        
                                        h5("Tab 1: Scope 3 CO2 emissions broken down by food category or supplier;"),
                                        
                                        h5("Tab 2: Magnitude of carbon offset from food waste diversion (relative to Scope 1,2 or 3);"),
                                        
                                        h5("Tab 3: Change in emissions between 2019 and 2020;"),
                                        
                                        h5("Tab 4: Interactive treemap (or stacked bar chart) of total carbon footprint."),
                                        
                                        h5("Data Source: Nada Grocery.")
                                        
                                    )
                                        
                                    )),
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
                           
                           tabPanel(
                               "Purchased Goods and Services",  # Tab names need work 
                                    sidebarLayout(
                                   
                                            sidebarPanel("Explaning this part of the tool",
                                                    selectInput(
                                                        inputId = "pick_year",
                                                            label = "Select Year:",
                                                             choices = c("2019", "2020")),
                                                selectInput(
                                                    inputId = "pick_prod_cat",
              label = "Pick Product Category:",
              choices = c("Poultry & Eggs"= "eggs", "Cheese" = "cheese", "Meat" = "meat", "Fabrics" = "fabric","Flours" = "flours", "Bread & Bakery" = "bread", "Cookies, Crackers,Pastas & Tortillas" = "cookies", "Sugars" = "sugar", "Coffee & Tea" = "coffee", "Pickling & Canning" = "dried", "Ice Cream" = "ice cream", "Frozen Food" = "frozen", "Scrap" = "scrap", "Condensed Dairy Products" = "dairy", "Milk" = "dairy","Soybean Processing" = "oilseed", "Oilseed Farming" = "oilseed","Snack Food" = "snack","Grain Farming" = "grain", "Fish" = "fish", "Seasonings & Dressings" = "seasoning","Breweries" = "brewery", "Florals" = "floral", "Cleaning Supplies" = "cleaning", "Toiletries" = "beauty", "Fruit & Tree Nut Farming" = "fruit", "Vegetable & Melon Farming" = "vegetable", "Apparel" = "apparel", "Glass" = "glass", "Cutlery" = "cutlery", "Paper" = "paper", "Metal" = "metal", "Fibers & Yarn" = "thread", "Honey" = "honey","Other (Food)" = "other food", "Other (Non Food)" = "other all"), selected = "Vegetable & Melon Farming")),
                                   
                                   mainPanel(
                                       "Purchased Goods & Services Description",
                                       plotOutput("puch_plot")
                                   )
                               )
                           ), 
                           
                           tabPanel(
                               "Food Waste Diversion",
                                    sidebarLayout(
                    
                                        sidebarPanel("Explaning this part of the tool",
                                    checkboxGroupInput(
                                        inputId = "food_waste_group",
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
    
    
    # graph for "Purchased goods and services" tab
    
    puch_reactive <- reactive({
        puch %>% 
            filter( year == input$pick_year &
                   prod_cat == input$pick_prod_cat)
    })
    
    output$puch_plot <- renderPlot({
        ggplot(data = puch_reactive(), aes(x = prod_cat, y = total_kg_co2e)) +
            geom_col() + 
            theme_minimal()
    })
    
    # graph for "food waste" tab:
    
    # reactive data frame
    food_waste_reactive <- reactive({
        food_waste_pos %>% 
            filter(category %in% input$food_waste_group)
    })
    
    # output plot 
    output$food_waste_plot <- renderPlot({
        ggplot(data = food_waste_reactive(), aes(x = year, y = kg_co2e, fill = sub_group)) +
            geom_col() +
            theme_minimal()
    })
    
}

shinyApp(ui = ui, server = server)
