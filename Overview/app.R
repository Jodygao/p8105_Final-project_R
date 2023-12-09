library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(readr)
library(ggiraph)

overall <- read_csv("/Users/olivia/Desktop/Columbia/Semester 3/Data Science/Final/p8105_Final-project_R/data/BRFSS/2022-overall-prevalence.csv", show_col_types = FALSE)
asian <- read_csv("/Users/olivia/Desktop/Columbia/Semester 3/Data Science/Final/p8105_Final-project_R/data/BRFSS/2022-asian.csv", show_col_types = FALSE)
black <- read_csv("/Users/olivia/Desktop/Columbia/Semester 3/Data Science/Final/p8105_Final-project_R/data/BRFSS/2022-black.csv", show_col_types = FALSE)
white <- read_csv("/Users/olivia/Desktop/Columbia/Semester 3/Data Science/Final/p8105_Final-project_R/data/BRFSS/2022-white.csv", show_col_types = FALSE)
hispanic <- read_csv("/Users/olivia/Desktop/Columbia/Semester 3/Data Science/Final/p8105_Final-project_R/data/BRFSS/2022-hispanic.csv", show_col_types = FALSE)

states_map <- map_data("state")

ui <- fluidPage(
  titlePanel("US Prevalence Overview"),
  sidebarLayout(
    sidebarPanel(
      selectInput("race", "Select Race Category:",
                  choices = c("Overall", "Asian", "Black", "White", "Hispanic"))
    ),
    mainPanel(girafeOutput("mapPlot"))
  )
)

server <- function(input, output) {
  
  output$mapPlot <- renderGirafe({
    data_to_use <- switch(input$race,
                          "Overall" = overall,
                          "Asian" = asian,
                          "Black" = black,
                          "White" = white,
                          "Hispanic" = hispanic)
    
    data_to_use <- data_to_use |>
      mutate(State = tolower(State))
    
    map_data <- left_join(states_map, data_to_use, by = c("region" = "State"))
    map_data <- suppressWarnings(map_data |>
                                   mutate(Prevalence = as.numeric(as.character(Prevalence))))
    
    breaks <- c(0, 20, 25, 30, 35, 40, 45, 50)
    
    p <- ggplot(data = map_data) +
      geom_polygon_interactive(aes(x = long, y = lat, group = group, fill = Prevalence, tooltip = paste("State:", region, "<br>Prevalence:", Prevalence, "<br>95% CI:", `95% CI`)), color = "white") +
      scale_fill_continuous(
        name = "Obesity Prevalence (%)",
        breaks = breaks,
        labels = c("<20%", "20%-<25%", "25%-<30%", "30%-<35%", 
                   "35%-<40%", "40%-<45%", "45%-<50%", "50%+"),
        low = "beige", high = "darkred", na.value = "grey"
      ) +
      coord_fixed(1.3) +
      theme_minimal() +
      labs(title = paste("Obesity Prevalence in", input$race, "Adults by State in 2022")) +
      theme(legend.position = "right",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    girafe(ggobj = p)
  })
}

shinyApp(ui = ui, server = server)