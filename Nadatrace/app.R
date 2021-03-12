
library(shiny)
library(tidyverse)
library(shinythemes)
library(bslib)
library(janitor)
library(here)
library(ggraph)
library(ggiraph)
library(igraph)
library(viridis)
library(packcircles)
library(data.tree)
library(circlepackeR)

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

cf$label <- paste0(cf$year)

pur_19 <- read_csv(here("Nadatrace", "purchased_goods_19.csv")) %>% 
    clean_names()%>% 
    mutate(year = "2019")

pur_20 <- read_csv(here("Nadatrace", "purchased_goods_20.csv")) %>% 
    clean_names()%>% 
    mutate(year = "2020")


puch <- rbind(pur_19, pur_20) %>% 
    mutate(year = as.character(year))

packing_pun <- circleProgressiveLayout(puch$total_kg_co2e, sizetype='area')

puch_data <- cbind(puch, packing_pun) %>% 
    mutate_if(is.numeric, ~round(., 2))

puch_data$label <- paste0(puch_data$category,  puch_data$total_kg_co2e)
puch_data$label2 <- paste0(puch_data$category, "", puch_data$year)

puch_gg <- circleLayoutVertices(packing_pun, npoints=50)

scope3 <- total_emissions %>% 
    filter(scope == "SCOPE 3") %>% 
    mutate(year = as.character(year))

scope3_4zoomcircle <- scope3 %>% 
    mutate(root = "root") %>% 
    mutate(pathString = paste("nada", year, category, sub_group, sep = "/")) 

scope3_4zoomcircle <- scope3_4zoomcircle[, c(6,1,2,3,4,5,7)]

diversion_cc_19 <- total_emissions %>% 
    filter(scope ==  "OFFSETS", 
           year == "2019") %>% 
    filter(!sub_group == "Low") %>% 
    filter(!sub_group == "Medium") %>% 
    mutate(year_tot = sum(kg_co2e))

diversion_cc_20 <- total_emissions %>% 
    filter(scope ==  "OFFSETS", 
           year == "2020") %>% 
    filter(!sub_group == "Low") %>% 
    filter(!sub_group == "Medium")%>% 
    mutate(year_tot = sum(kg_co2e))

diversion_cc_1 <- rbind(diversion_cc_19, diversion_cc_20)

diversion_cc <- diversion_cc_1 %>% 
    mutate(category = str_replace(category, pattern = "Compost", replacement = "COMPOST")) %>% 
    mutate(category = str_replace(category, pattern = "Café", replacement = "CAFÉ")) %>% 
    mutate(kg_co2e = kg_co2e* -1,
           year_tot = year_tot* -1)



diversion_cc$fraction <- diversion_cc$kg_co2e/sum(diversion_cc$kg_co2e)

diversion_cc$fraction

diversion_cc$ymax <- cumsum(diversion_cc$fraction)

diversion_cc$ymin <- c(0, head(diversion_cc$ymax, n=-1))

diversion_cc$labelPosition <- (diversion_cc$ymax + diversion_cc$ymin) / 2

diversion_cc$label <- paste0(diversion_cc$category,":\n", diversion_cc$kg_co2e, " kg")
diversion_cc$label2 <- paste0("Carbon Savings: \n", diversion_cc$year_tot, " kg")





# Define UI for application that draws a histogram
ui <- fluidPage (theme = nada_theme,
                # Application title
                navbarPage(h2("NADAtrace Carbon Footprint Visualization Tool"),
                           
                           navbarMenu("Background",
                           
                           tabPanel("About Nada",
                                    
                                    mainPanel(
                                        
                                        h3("Nada is a package-free grocery store on a mission"),
                                        
                                        img(src = "nada1.jpg", height = 251, width = 444),
                                        
                                        h5("Nada is a grocery store in Vancouver, BC that offers sustainably sourced foods, zero waste lifestyle products, and a package-free shopping experience. The owners also have a committment to environmental and social justice and are constantly looking at ways to reduce their carbon footprint. What sets Nada apart is their drive to not only understand the carbon footprint from their own business operations, but also the footprints of all the suppliers and transportators that are involved up and down their supply chain. Woah!"),
                                        
                                        img(src = "nada2.jpg", height = 251, width = 444),
                                        
                                        h5("By taking this vertically integrated approach to analyzing and understanding the carbon footprint at each point along their supply chain, Nada hopes to set a bold new example for grocers by encouraging locally sourced products and rewarding suppliers who take environmental action seriously. This application will help that goal by providing a quick and easy way to see which areas along Nada's supply chain (including their own operations) are the larger emitters and thus target them for CO2 emission reduction. See how the footprint is broken down in Scoping and Data Sources."))),

                            tabPanel("Scoping and Data Sources",
                                     mainPanel(
                                         h3("What do we mean by Scopes 1, 2, and 3 and where does our data come from?"),
                                         h5("Nada has broken down their entire carbon footprint into three categories called 'Scopes' so they can visualize where there is the most room to improve. See below for the exact breakdown. Note that Scopes 1 and 2 represent different aspects of Nada's own CO2 emissions while Scope 3 captures the footprints of all suppliers and transporters who help put  food on the shelves."),
                                         
                                         h5(strong("Scope 1"), "The first category of Nada's on-site emissions. Scope 1 is all of the CO2 equivalent emissions that come from leaking of refrigerants over time at the store. Data acquired about appliances were acquired from equipment spec sheets and charge capacity information was provided by the manufacturer."),
                                         
                                         h5(strong("Scope 2"), "The second grouping of Nada's on-site emissions. Scope 2 is the amount of C02 emitted due to the consumption of electricity at the store. Nada's utility bills from 2019-2020 were used to determine energy consumption and CO2 equivalent was calculated from that result."),
                                         
                                         h5(strong("Scope 3"), "The big one. Scope 3 captures the carbon footprint generated from all operations required to put food on the shelf at the Nada store. It's further divided into Purchased Goods and Services and Upstream Transportation. PG&S is the amount of emissions produced from the suppliers in the course of producing the good that Nada will sell (ie: how much carbon is emitted to take to make the candy bar). Upstream Transportation are the emissions from getting those products to the store."),
                                         
                                         h5("While Scopes 1 and 2 effectively capture Nada's direct carbon footprint, their committment to emissions reduction pushed them to include Scope 3 in their analysis. Because Scope 3 encapsulates the total carbon footprints of all of their suppliers as well as upstream transportation to deliver products, the user will see that in scale, it dwarfs Scopes 1 and 2. See more in How to Use this Tool."),
                                         
                                     )),
                                      
                            tabPanel("How to Use this Tool",
                                     mainPanel(
                                        h3("Using this tool to visualize the carbon footprint of Nada's supply chain and operations (Scopes 1,2, and 3)"),
                                        
                                        h5("This set of tools allows the user to visually explore the carbon footprint of Nada from 2019 and 2020. 4 interactive data visualization tools are included in the subsequent tabs:"),
                                        
                                        h5("Tab 1: Side by side comparison of total carbon footprint (Scopes 1,2,3) for the years 2019 and 2020;"),
                                        
                                        h5("Tab 2: Circle plots displaying the relative emissions breakdown within Scope 3. Highest level suppliers and transporters are displayed for both years;"),
                                        
                                        h5("Tab 3: Deep dive into emissions by PG&S for both years. Emissions from all 33 types of suppliers can be explored and compared for both 2019 and 2020;"),
                                        
                                        h5("Tab 4: Column graph displaying Nada's CO2 offset actions by way of composting and using sustainably sourced materials in their in store cafe."),
                                        
                                        h5("Data Source: Nada Grocery.")
                                        
                                    )
                                        
                                    )),
                           tabPanel("2019 vs. 2020",
                                    sidebarLayout(
                                        mainPanel("Here you can see the relative magnitude of CO2 emissions of Scopes 1, 2, and 3 for the years 2019 and 2020. Play around with visualizing each of them individually then all at once. Notice how Scopes 1 and 2 have comparable emissions while Scope 3 dwarfs them.",
                                                  plotOutput("tot_em_plot"
                                                  )
                                        ),
                                        sidebarPanel(
                                            
                                            checkboxGroupInput(
                                                inputId = "footprint_scope",
                                                label = "Choose Scope to compare carbon footprint:",
                                                choices = c("SCOPE 1", "SCOPE 2", "SCOPE 3"),
                                                selected = c("SCOPE 1", "SCOPE 2", "SCOPE 3"))
                                        )
                                    )
                                    
                           ),
                           
                           tabPanel(
                               "Scope 3 Emissions",
                                    
                                sidebarLayout(
                                        
                                    sidebarPanel(
                                        "This tool allows you to visualize the six largest emitters of CO2 in each of the two subdivisions of Scope 3 (Purchased Goods and Services and Upstream Transportation) for both years. Try clicking through the bubbles to see who the big emitters are for each part of Scope 3.",
                                        checkboxGroupInput(
                                            inputId = "scope3_category",
                                            label = "Choose Scope 3 category to view carbon footprint:",
                                            choices = c("Transportation", "Purchased Goods & Services"),
                                            selected = c("Transportation", "Purchased Goods & Services")
                                            )
                                        ),
                                        
                                    mainPanel(
                                        
                                        circlepackeROutput("scope3_zoomcircle")
                                    )
                                    )
                                    ),
                           
                           tabPanel(
                               "Purchased Goods and Services",  # Tab names need work 
                                    sidebarLayout(
                                   
                                            sidebarPanel("This tool allows you to visualize the carbon footprint of all 33 food service industry categories offerred at Nada. Try selecting different categories in just one year than click both years and see how each category's footprint changed between 2019 and 2020.",
                                                         checkboxGroupInput(
                                                             inputId = "pick_year",
                                                             label = "Choose Year:",
                                                             choices = c("2019", "2020")),
                                                selectInput(
                                                    inputId = "pick_prod_cat",
              label = "Pick Product Category:",
              choices = c("Poultry & Eggs"= "eggs", "Cheese" = "cheese", "Meat" = "meat", "Fabrics" = "fabric","Flours" = "flours", "Bread & Bakery" = "bread", "Cookies, Crackers,  Pastas & Tortillas" = "cookie", "Sugars" = "sugar", "Coffee & Tea" = "coffee", "Pickling & Canning" = "dried", "Ice Cream" = "ice cream", "Frozen Food" = "frozen", "Scrap" = "scrap", "Condensed Dairy Products" = "dairy", "Milk" = "dairy","Soybean Processing" = "oilseed", "Oilseed Farming" = "oilseed","Snack Food" = "snack","Grain Farming" = "grain", "Fish" = "fish", "Seasonings & Dressings" = "seasoning","Breweries" = "brewery", "Florals" = "floral", "Cleaning Supplies" = "cleaning", "Toiletries" = "beauty", "Fruit & Tree Nut Farming" = "fruit", "Vegetable & Melon Farming" = "vegetable", "Apparel" = "apparel", "Glass" = "glass", "Cutlery" = "cutlery", "Paper" = "paper", "Metal" = "metal", "Fibers & Yarn" = "thread", "Honey" = "honey","Other (Food)" = "other food", "Other (Non Food)" = "other all"), selected = "Vegetable & Melon Farming")),
                                   
                                   mainPanel(
                                       "Purchased Goods & Services Description",
                                       plotOutput("puch_plot")
                                   )
                               )
                           ), 
                           
                           tabPanel(
                               "Food Waste Diversion",
                                    sidebarLayout(
                    
                                        sidebarPanel("This tool allows you to visualize the two ways Nada participates in diverting food waste from the landfill, composting and the sale of 'imperfect' food in their cafe. This diversion is a CO2 offset because it precludes the need for new food to be grown for the cafe and prevents it from decomposing in a landfill where the nutrients would be lost.",
                                                     radioButtons(inputId = "diversion_year",
                                                                  label = "Select Year:",
                                                                  choices = c("2019","2020") 
                                                     ),
                                    ),
                                    
                                    mainPanel(
                                        "Click between the two years to see the change in carbon offsets through food waste diversion.",
                                        plotOutput("food_waste_plot")
                                    )
                                    )
                                    )
                        
                           
                           ))
                


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # graph for "2019 vs 2020" tab:    
    
    cf_sum <- cf %>% 
        group_by(year, scope) %>% 
        summarize(kg_co2e = sum(kg_co2e))
        
    
    tot_em_reactive <- reactive({
        cf_sum %>% 
            filter(scope %in% input$footprint_scope)
            
    })
    
    output$tot_em_plot <- renderPlot({
        ggplot(data = tot_em_reactive(), aes(x = year, y = kg_co2e, fill = scope, label = kg_co2e)) +
            #geom_col(aes(fill = scope)) +
            geom_bar(stat = "identity", position = "dodge") +
            #geom_point(aes(colour = scope, shape = scope, size = kg_co2e)) +

            geom_text(position = position_dodge2(width = 0.9), vjust=-0.2, hjust=0.5) +
            #scale_fill_brewer(palette = "RdPu") +
            theme_void() +
            #theme_minimal() +
            theme(axis.text.y=element_blank()) +
            scale_fill_manual(values = c("SCOPE 1" = "pink",
                                         "SCOPE 2" = "turquoise1",
                                         "SCOPE 3" = "lightcoral")) +
            #geom_text(y = 600, aes(label = label), size = 7) +
            #theme_void() +
            scale_size_continuous(range = c(3, 10)) +
            guides(fill=guide_legend(title=NULL)) +
            labs(x = "",
                 y = "Kilograms CO2 Equivalent",
                 size = "Kg CO2 Equivalent") 
    })
    
    
    # graph for "Scope 3 Emissions" tab:
    
    output$scope3_zoomcircle <- renderCirclepackeR({
        req(input$scope3_category)
        
        zoomcircle <- scope3_4zoomcircle %>% 
            filter(category %in% input$scope3_category) 
        
        node <- as.Node(zoomcircle)
        
        circlepackeR(node, size = "kg_co2e")
    })
    
    # graph for "Purchased goods and services" tab
    
    puch_reactive <- reactive({
        puch_data %>% 
            filter(year %in% input$pick_year &
                   prod_cat == input$pick_prod_cat)
    })
    
    output$puch_plot <- renderPlot({
        ggplot(data = puch_reactive(), aes(x = input$pick_year, y = total_kg_co2e)) +
            ylim(0,90000) + 
            geom_col(aes(fill = year)) +
            scale_fill_manual(values = c(
                "2020" = "turquoise1",
                "2019" = "lightcoral")) +
            geom_text(y = 40000, aes(label = label), size = 7) +
            geom_text(y = 2000, aes(label = label2), size = 4) +
            #geom_point(aes(text = total_kg_co2e)) +
            theme_void() +
            theme(legend.position="none") +
            #labs() +
            ylab(expression(paste("Kilograms CO" [2]))) +
            theme(axis.title.y = element_text(size = 14),
                  axis.text.y = element_text(size = 12))
    })
    
    # graph for "food waste" tab:
    
    # reactive data frame
    food_waste_reactive <- reactive({
        diversion_cc %>% 
            filter(year %in% input$diversion_year)
    })
    
    # output plot 
    output$food_waste_plot <- renderPlot({
        ggplot(food_waste_reactive(), aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
            geom_rect() +
            scale_fill_manual(values = c(
                "COMPOST" = "turquoise1",
                "CAFÉ" = "lightcoral")) +
            geom_text(x = 3.45, aes(y = labelPosition, label = label), size = 6) +
            geom_text(x = 1, aes(y = labelPosition, label = label2), size = 10) +
           #scale_fill_manual(c()) +
            coord_polar(theta = "y") +
            xlim(c(1, 4)) +
            theme_void() +
            theme(legend.position = "none")
    })
    
    
}

shinyApp(ui = ui, server = server)
