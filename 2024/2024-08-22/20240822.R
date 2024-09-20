
# Load packages -----------------------------------------------------------

library(tidyverse)
library(reactable)
library(htmltools)


# Load data ---------------------------------------------------------------

df <- read_csv("./2024/2024-08-22/20240822.csv")

# Data wrangling ----------------------------------------------------------

digital_skills_summary <- df |>
  # Keep only "at least basic skills" for Overall Digital Skills
  filter(!indicator %in% c("Individuals with above basic overall digital skills (all five component indicators are at above basic level)",
                           "Individuals with basic overall digital skills (all five component indicators are at basic or above basic level, without being all above basic)"
  )) |>
  # Extract `digital skills` and `indicator level` from the `indicator` column
  mutate(
    digital_skills = case_when(
      str_detect(indicator, "communication and collaboration") ~ "Communication and Collaboration Skills",
      str_detect(indicator, "digital content creation") ~ "Digital Content Creation Skills",
      str_detect(indicator, "information and data literacy") ~ "Information and Data Literacy Skills",
      str_detect(indicator, "overall digital skills") ~ "Overall Digital Skills",
      str_detect(indicator, "problem solving skills") ~ "Problem Solving Skills",
      str_detect(indicator, "safety skills") ~ "Safety Skills"
    ),
    indicator_level = case_when(
      # Special case for overall digital skills with at least basic level
      str_detect(indicator, "basic overall digital skills \\(all five component indicators are at basic or above basic level\\)") ~
        "At least basic skills",

      # General cases
      str_detect(indicator, "basic or above basic") ~ "Basic or above basic skills",
      str_detect(indicator, "above basic") ~ "Above basic skills",
      str_detect(indicator, "basic") ~ "Basic skills")
  )



country_codes <- data.frame(
  Country = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark",
              "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
              "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
              "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "EU_27"),
  ISO_Code = c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE",
               "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO",
               "SK", "SI", "ES", "SE", "EU")
)

# Pivot Data
dsi <- digital_skills_summary |>
  filter(digital_skills == "Overall Digital Skills") |>
  select(year, country, value) |>
  pivot_wider(names_from = year, values_from = value) |>
  rename(
    `2021` = `2021`,
    `2023` = `2023`,
    Country = country
  ) |>
  mutate(Change = `2023` - `2021`,
         `2021` = round(`2021`, 1),
         `2023` = round(`2023`, 1)) |>
  left_join(country_codes, by = c("Country" = "Country"))


# Prepare data for the table

# Bar chart function for percentage points
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "1rem",
                              pos_fill = "#416ea4",
                              neg_fill = "#9D1B1FFF") {
  neg_chart <- div(style = list(flex = "1 1 0"))
  pos_chart <- div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 100, "%")

  if (value < 0) {
    bar <- div(style = list(marginLeft = "0.5rem",
                            background = neg_fill,
                            width = width,
                            height = height))
    chart <- div(
      style = list(display = "flex",
                   alignItems = "center",
                   justifyContent = "flex-end"),
      label,
      bar
    )
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- div(style = list(marginRight = "0.5rem",
                            background = pos_fill,
                            width = width,
                            height = height))
    chart <- div(
      style = list(display = "flex",
                              alignItems = "center"),
      bar, label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }

  div(style = list(display = "flex"), neg_chart, pos_chart)
}

# Define the maximum value for scaling (use absolute maximum from relevant columns)
max_change <- max(abs(dsi$Change))


# Function to generate a color based on the DSI score (percentage)
get_score_color <- function(score, min_score = 0, max_score = 100) {
  # Blue palette transition
  blue_pal <- function(x) rgb(colorRamp(c("#9fc7df", "#416ea4"))(x), maxColorValue = 255)

  # Normalize the score to the range [0, 1]
  normalized <- (score - min_score) / (max_score - min_score)

  # Return the color for the normalized score
  blue_pal(normalized)
}

# Function to generate the donut chart with a color
donut_chart <- function(value, color) {
  # All units are in rem for relative scaling
  radius <- 1.5
  diameter <- 3.75
  center <- diameter / 2
  width <- 0.25
  slice_length <- 2 * pi * radius
  slice_offset <- slice_length * (1 - value / 100)

  # Generate SVG donut chart
  donut_chart <- paste0(
    '<svg width="', diameter, 'rem" height="', diameter, 'rem" style="transform: rotate(-90deg);" focusable="false">',
    '<circle cx="', center, 'rem" cy="', center, 'rem" r="', radius, 'rem" fill="none" stroke-width="', width, 'rem" stroke="rgba(0,0,0,0.1)"></circle>',
    '<circle cx="', center, 'rem" cy="', center, 'rem" r="', radius, 'rem" fill="none" stroke-width="', width, 'rem" stroke="', color, '"',
    ' stroke-dasharray="', slice_length, 'rem" stroke-dashoffset="', slice_offset, 'rem"></circle>',
    '</svg>'
  )

  # Label in the center
  label <- paste0(
    '<div style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%)">',
    value, '%</div>'
  )

  # Final donut chart HTML
  paste0('<div style="display: inline-flex; position: relative; width: ', diameter, 'rem; height: ', diameter, 'rem;">',
         donut_chart,
         label,
         '</div>')
}


# Define a mapping from ISO codes to the flag URLs
flag_urls <- list(
  "AT" = "https://hatscripts.github.io/circle-flags/flags/at.svg",
  "BE" = "https://hatscripts.github.io/circle-flags/flags/be.svg",
  "BG" = "https://hatscripts.github.io/circle-flags/flags/bg.svg",
  "HR" = "https://hatscripts.github.io/circle-flags/flags/hr.svg",
  "CY" = "https://hatscripts.github.io/circle-flags/flags/cy.svg",
  "CZ" = "https://hatscripts.github.io/circle-flags/flags/cz.svg",
  "DK" = "https://hatscripts.github.io/circle-flags/flags/dk.svg",
  "EE" = "https://hatscripts.github.io/circle-flags/flags/ee.svg",
  "FI" = "https://hatscripts.github.io/circle-flags/flags/fi.svg",
  "FR" = "https://hatscripts.github.io/circle-flags/flags/fr.svg",
  "DE" = "https://hatscripts.github.io/circle-flags/flags/de.svg",
  "GR" = "https://hatscripts.github.io/circle-flags/flags/gr.svg",
  "HU" = "https://hatscripts.github.io/circle-flags/flags/hu.svg",
  "IE" = "https://hatscripts.github.io/circle-flags/flags/ie.svg",
  "IT" = "https://hatscripts.github.io/circle-flags/flags/it.svg",
  "LV" = "https://hatscripts.github.io/circle-flags/flags/lv.svg",
  "LT" = "https://hatscripts.github.io/circle-flags/flags/lt.svg",
  "LU" = "https://hatscripts.github.io/circle-flags/flags/lu.svg",
  "MT" = "https://hatscripts.github.io/circle-flags/flags/mt.svg",
  "NL" = "https://hatscripts.github.io/circle-flags/flags/nl.svg",
  "PL" = "https://hatscripts.github.io/circle-flags/flags/pl.svg",
  "PT" = "https://hatscripts.github.io/circle-flags/flags/pt.svg",
  "RO" = "https://hatscripts.github.io/circle-flags/flags/ro.svg",
  "SK" = "https://hatscripts.github.io/circle-flags/flags/sk.svg",
  "SI" = "https://hatscripts.github.io/circle-flags/flags/si.svg",
  "ES" = "https://hatscripts.github.io/circle-flags/flags/es.svg",
  "SE" = "https://hatscripts.github.io/circle-flags/flags/se.svg",
  "EU" = "https://hatscripts.github.io/circle-flags/flags/eu.svg"  # For EU_27
)

dsi_no_iso <- dsi|>
  select(-ISO_Code)  # Remove ISO_Code column



# Table --------------------------------------------------------------------


final_table <- div(
  # Title and Subtitle
  h1("Digital Skills Indicator by Country", style = "text-align: left; font-family: sans-serif;"),
  h2("Changes in 2023 compared to 2021", style = "text-align: left; font-family: sans-serif; color: gray;"),

  reactable(
    dsi_no_iso,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(5, 10, 20, 28),
    defaultPageSize = 10,
    searchable = TRUE,
    defaultSorted = "2023",
    defaultSortOrder = "desc",
    style = list(fontFamily = "sans serif",
                 borderColor = "#DADADA"),
    columns = list(
      Country = colDef(
        cell = function(value, index) {
          ISO_Code <- dsi$ISO_Code[index]  # Get the ISO code for the current row
          flag_url <- flag_urls[[ISO_Code]]  # Get the flag URL using the ISO code
          if (is.null(flag_url)) {
            flag_url <- "https://hatscripts.github.io/circle-flags/flags/default.svg"
          }
          div(
            class = "country-container",
            style = "display: flex; align-items: center;",
            img(class = "flag-url",
                alt = paste(value, "flag"),
                src = flag_url,
                style = "width: 70px; height: 30px; margin-right: 10px;"),
            span(class = "country-name", value, style = "font-weight: bold;")
          )
        },
        align = "left",
        minWidth = 120,
        html = TRUE
      ),
      `2021` = colDef(
        cell = function(value) {
          # Calculate the color for the current score
          color <- get_score_color(value, min_score = min(dsi$`2021`), max_score = max(dsi$`2021`))
          as.character(donut_chart(value, color))
        },
        align = "center",
        minWidth = 160,
        html = TRUE
      ),
      `2023` = colDef(
        cell = function(value) {
          # Calculate the color for the current score
          color <- get_score_color(value, min_score = min(dsi$`2023`), max_score = max(dsi$`2023`))
          as.character(donut_chart(value, color))
        },
        align = "center",
        minWidth = 160,
        html = TRUE
      ),
      Change = colDef(
        cell = function(value) {
          label <- paste0(round(value, 1))
          span(style = "font-weight: bold;",
               bar_chart_pos_neg(label, value, max_value = max_change))
        },
        align = "center",
        minWidth = 150
      )
    ),
    defaultColGroup = colGroup(headerVAlign = "bottom"),
    columnGroups = list(
      colGroup(name = "Digital Skills Indicator",
               columns = c("2021", "2023", "Change"))
    ),
    defaultColDef = colDef(
      vAlign = "center",
      headerVAlign = "bottom",
      class = "cell",
      headerClass = "header"
    )
  ),
 )

# Save the table as an HTML file
save_html(final_table, file.path("2024", "2024-08-22", paste0("20240822", ".html")))

