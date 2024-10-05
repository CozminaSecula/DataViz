
# Install and Load packages -----------------------------------------------------------

packages <- c("tidyverse", "janitor", "plotly", "shinyWidgets", "paletteer", "ggrepel", "shiny")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}


library(tidyverse)
library(janitor)
library(plotly)
library(shinyWidgets)
library(paletteer)
library(ggrepel)
library(shiny)


# Load data ---------------------------------------------------------------

df <- read.csv("./2024/2024-09-10/20240910.csv")


# Data wrangling ----------------------------------------------------------

data <- df |>
  mutate(Year = as.integer(Year),
         country = as.character(Entity),
         child_mortality_rate = round(`Under-five mortality rate`, 2)) |>
  clean_names()



# Plot --------------------------------------------------------------------


# Define countries
countries <- unique(data$country)


# Define UI
ui <- fluidPage(
  # Main title and subtitle together
  fluidRow(
    column(12,
           h1("Child Mortality Rate Over Time", style = "font-family: Sans Serif; font-size: 30px;"),  # Main title
           h4("The estimated share of newborns who die before reaching the age of five.", style = "color: grey; font-family: Sans Serif; font-size: 20px;")  # Subtitle with styling
    )
  ),

  # Add a description section above the plot
  fluidRow(
    column(12,
           p("This work is a re-creation of the original visualization:",
             tags$a(href = "https://ourworldindata.org/grapher/child-mortality", "Child mortality rate, 1751 to 2021."),
             br(),
             "The chart visualizes the changes in child mortality rate over time for selected countries.
              The data is represented as the percentage of newborns who do not survive past the age of five.",
             style = "font-size: 18px; font-family: Sans Serif;")
    )
  ),

  # Set the sidebar to be on the right side
  sidebarLayout(
    mainPanel(
      plotlyOutput("plot"),  # Plot on the left side

      # Add the source information below the plot
      fluidRow(
        column(12,
               p(tags$small("A newborn is defined as a baby born alive, and usually refers to neonates – under 28 days old.",
                            style = "color: grey; font-family: Sans Serif; font-size: 12px; margin-top: 20px;"),
                 br(),
                 "Data source: ",
                 tags$a(href = "https://data.world/makeovermonday/child-and-infant-mortality", "Makeover Monday/Child and Infant Mortality"),  # Example source link
                 style = "font-size: 12px; color: grey; margin-top: 20px;"),
               br(),
               "SOURCE & ARTICLE:",
               tags$a(href = "https://ourworldindata.org/child-mortality?insight=millions-of-children-die-every-year#key-insights", "Child and Infant Mortality - Our World in Data")
        )
      )
    ),

    sidebarPanel(
      # MultiInput with country names only
      multiInput(
        inputId = "countries",
        label = "Select Countries:",
        choices = countries,  # Only country names
        selected = c("France", "Romania", "United Kingdom", "United States")
      ),
      sliderInput("yearRange", "Year Range:",
                  min = min(data$year), max = max(data$year),
                  value = c(min(data$year), max(data$year)),
                  step = 1)
    )
  )
)

# Define server logic
server <- function(input, output) {

  filtered_data <- reactive({
    data |>
      filter(year >= input$yearRange[1] & year <= input$yearRange[2]) |>
      filter(country %in% input$countries)
  })

  output$plot <- renderPlotly({
    # Create ggplot object
    p <- ggplot(
      filtered_data(),
      aes(x = year,
          y = child_mortality_rate,
          color = country)) +
      geom_line() +
      geom_hline(yintercept = 0,
                 color = "grey40") +
      scale_y_continuous(labels = scales::percent_format(scale = 1),
                         expand = c(0,0)) +
      scale_colour_paletteer_d("DresdenColor::foolmoon") +
      labs(title = "",
           x = "Year",
           y = "Mortality Rate (per 100 live births)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted",
                                          color = "grey60",
                                          linewidth = 0.4),
        axis.ticks.x = element_line(linewidth = 0.3,
                                    color = "grey40"),
        axis.ticks.length.x = unit(1.5, "mm"),
        axis.text = element_text(family = "Sans Serif",
                                 size = 12,
                                 color = "grey40"),
        axis.text.y = element_text(margin = margin(0, 5, 0, 0)),
        axis.title = element_blank()
      )
    # Convert ggplot object to plotly object
    ggplotly(p, tooltip = c("country", "year", "child_mortality_rate"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

