library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(here)
# load files
techcredit <- read_csv(here("proc", "techcredit.csv"))

# create country names named vector
country_names <- unique(techcredit$country_name)
names(country_names) <- unique(techcredit$country_name)
country_names <- sort(country_names)

# create variable names vector
var_names <- c("big_tech_credit", "fintech_credit", "total_alternative_credit", 
              "big_tech_credit_per_capita", "fintech_credit_per_capita", "total_domestic_credit_by_financial_sector")
names(var_names) <- gsub("_", " ", var_names)

ui <- dashboardPage(
  dashboardHeader(title = "Fintech and Big-Tech Credit Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("FAQ", tabName = "FAQ", icon = icon("th"))
    )
  ),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabItems(
      # First Tab Content: Main Dashboard
      tabItem(tabName = "Dashboard",
              fluidRow(
                box(
                  title = "Control single-country graph",
                  selectInput("country", 
                              label = h3("Select country"), 
                              choices = country_names, 
                              selected = "United States of America", 
                              multiple = FALSE
                              )
                  ), 
                
                box(
                  title = "Control multi-country graph",
                  selectInput("country2", 
                              label = h3("Select countries"), 
                              choices = country_names, 
                              selected = c("United States of America", "China", "Kenya", 
                                           "Japan", "Korea", "United Kingdom", "Indonesia",
                                           "Netherlands", "Russia", "Germany"),
                              multiple = TRUE), 
                  selectInput("variable", 
                              label = h3("Select output"), 
                              choices = var_names, 
                              selected = c("big_tech_credit")
                              )
                )
              )
              , 
              
              fluidRow(
                
                box(
                  title = "Alternative Credit Distribution by Country", 
                  plotOutput("country_stacked_bar", height = 500)
                  ), 
              
                box(
                  title = "Variation in Credit By Country", 
                  plotlyOutput("country_line_plot", height = 500)
                )
                
                
                )
              ), 
      # Second Tab Content: FAQ's and Underlying Data
      tabItem(tabName = "FAQ", 
              fluidRow(
                h2("Information on the Underlying Data and Frequently Asked Questions"),
                h3("Underlying Data"),
                p("The data that supports this dashboard is taken from the Fintech and Big Tech Credit database, which was released in September 2020 and supported by the following others:  Giulio Cornelli, Jon Frost, Leonardo Gambacorta, Raghavendra Rau, Robert Wardrop and Tania Ziegler.")
              ))
      )
    )
  )

server <- function(input, output) {
  output$country_stacked_bar <- renderPlot({
    graph_dat <- techcredit %>% 
      filter(country_name == input$country) %>%
      group_by(year) %>%
      summarise(total_fintech = sum(fintech_credit), 
                total_bigtech = sum(big_tech_credit), 
                total_domestic_fin = sum(total_domestic_credit_by_financial_sector)) %>%
      mutate(alt_share = 100*(total_fintech + total_bigtech)/total_domestic_fin) %>%
      mutate(alt_share = ifelse(alt_share > 10000000000, NA, alt_share)) %>%
      select(year, total_fintech, total_bigtech, alt_share) %>% 
      #filter(alt_share < 100) %>%
      pivot_longer(2:3) %>%
      mutate(value = value/1000) %>%
      group_by(year) %>%
      mutate(total_alt = sum(value)) %>%
      mutate(share = "Total Alternative Credit Share") %>%
      mutate("Lending volume" = name)
    
    scaling_factor <- max(graph_dat$total_alt)
    ymax = max(graph_dat$alt_share[!is.na(graph_dat$alt_share)])
    
    graph_dat %>%
      ggplot() + 
      geom_col(aes(x = as.factor(year), y = value, fill = name)) + 
      geom_line(aes(x = as.factor(year), y = alt_share*scaling_factor, color = share, group = 1))   + 
      geom_point(aes(x = as.factor(year), y = alt_share*scaling_factor, color = share, group = 1))   + 
      scale_fill_manual(values = c("cornflowerblue", "bisque3"), 
                        labels = c("Big Tech", "Fintech")) + 
      scale_y_continuous(
        name = "Billions of USD",
        sec.axis = sec_axis(~./scaling_factor, name="Alternative credit percentage of total credit", 
                            breaks = seq(0,max(1, ymax),.1), labels = paste0(seq(0,max(1, ymax),.1), "%"))
      ) + 
      scale_color_manual(values = "black") + 
      xlab(element_blank()) + 
      ggthemes::theme_economist_white() + 
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            axis.text.y.right = element_text(hjust  = 1.5))
    
    
  })
  
  
  output$country_line_plot <- renderPlotly(
    ggplotly(techcredit %>%
               mutate(year = as.factor(year)) %>%
               group_by(year, country_name) %>%
               filter(country_name %in% input$country2) %>%
               pivot_longer(4:9)  %>%
               filter(name == input$variable)  %>%
               ggplot(aes(x = year, y = value, color = iso3c, group = iso3c)) + 
               geom_line() + 
               geom_point() + 
               theme(legend.title = element_blank(), 
                     axis.text.x = element_text(size = 6)) + 
               set_theme(legend_position = "bottom") + 
               xlab("year"))

      
  )
}
shinyApp(ui, server)
