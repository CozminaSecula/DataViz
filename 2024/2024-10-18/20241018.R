
# Install and Load packages -----------------------------------------------------------

packages <- c("tidyverse", "readxl", "systemfonts", "ggtext", "ggrepel", "lemon")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tidyverse)
library(readxl)
library(ggtext)
library(systemfonts)
library(ggrepel)
library(lemon)



# Load data ---------------------------------------------------------------

raw_df_2023 <- read_xlsx("D:/dataviz_challenges/ro_data_tribe/2024-10/abf_2023e.xlsx",
                         range = "COICOP group!B7:F22")


raw_df_2024 <- read_xlsx("D:/dataviz_challenges/ro_data_tribe/2024-10/abf_tr2e24.xlsx",
                         range = "COICOP group!B6:F21")

# Load fonts --------------------------------------------------------------

windowsFont(family = "Open Sans")
systemfonts::register_variant(name = "OpenSans-Bold",
                              family = "Open Sans",
                              weight = "bold")

systemfonts::register_variant(name = "OpenSans-Regular",
                              family = "Open Sans",
                              weight = "normal")

# Define colors and fonts ----------------------------------------------------------

bg_col <- "#ffffff"
title_col <- "#222222"
subtitle_col <- "#71757d"

body_font <- "OpenSans-Regular"
title_font <- "OpenSans-Bold"
subtitle_font <- "OpenSans-Regular"

# Data wrangling ----------------------------------------------------------


data_cleaned_2023 <- raw_df_2023 |>
  filter(`...1` != "NA",
         `2022` != "- lei, per household -",
         `2023` != "- lei, per household -",
         `...3` != "- percentage -",
         `...5` != "- percentage -") |>
  rename(household_spending = `...1`,
         percent_2022 = `...3`,
         percent_2023 = `...5`)|>
  mutate(`2022` = (round(as.numeric(`2022`), 1)),
         `2023` = (round(as.numeric(`2023`), 1)),
         percent_2022 = (round(as.numeric(percent_2022), 1)),
         percent_2023 = (round(as.numeric(percent_2023), 1)))


data_cleaned_2024 <- raw_df_2024 |>
  filter(`Consumption expenditure by purpose` != "NA",
         `QUARTER I 2024` != "'-lei per household-",
         `QUARTER II 2024` != "'-lei per household-",
         `...3` != "- percentage -",
         `...5` != "- percentage -") |>
  rename(household_spending = `Consumption expenditure by purpose`,
         percent_quarter_1 = `...3`,
         percent_quarter_2 = `...5`)|>
  mutate(`QUARTER I 2024` = (round(as.numeric(`QUARTER I 2024`), 1)),
         `QUARTER II 2024` = (round(as.numeric(`QUARTER II 2024`), 1)),
         percent_quarter_1 = (round(as.numeric(percent_quarter_1), 1)),
         percent_quarter_2 = (round(as.numeric(percent_quarter_2), 1)))


# Compute average values semester I 2024

expenditure_average <- data_cleaned_2024 |>
  mutate(semester_1_2024_lei = round(((`QUARTER I 2024` + `QUARTER II 2024`) /2), 1))


expenditure_total <- expenditure_average |>
  filter(household_spending == "Total consumption expenditure") |>
  pull(semester_1_2024_lei)

expenditure_semester_1_2024 <- expenditure_average |>
  mutate(percent_2024 = round(((semester_1_2024_lei / expenditure_total) * 100), 1))

# Join the data sets

household_expenditure <- data_cleaned_2023 |>
  inner_join(expenditure_semester_1_2024, by = "household_spending")

# Data for the visualization

data_plot <- household_expenditure |>
  pivot_longer(cols = c(percent_2022, percent_2023, percent_2024),
               names_to = "year",
               values_to = "value") |>
  select(household_spending, "year", "value") |>
  filter(household_spending != "Total consumption expenditure") |>
  mutate(household_spending = fct_reorder(household_spending, value, .desc = TRUE),
         year = as.numeric(gsub("percent_", "", year)))




# Define text -------------------------------------------------------------

title <- paste0("How household budget allocation has evolved from 2022 to 2024")
subtitle <- paste0("Percentage allocation of household consumption expenditure across different categories.")
caption <- paste0("**Graphic**: Cozmina Secula | **Data**: National Institute of Satatistics<br>*Data is for the 1st semester, 2024")



point_color <- c("Food and non-alcoholic beverages" = "#595959",
                 "Dwelling, water, electricity, gas and other fuels" = "#595959",
                 "Alcoholic beverages, tobacco" = "#cc850a",
                 "Clothing and footwear" = "#e85130",
                 "Transport" = "#bd53bd",
                 "Furnishings, dwelling equipment and maintenance" = "#6363c0",
                 "Health" = "#008dc9",
                 "Personal care, social protection and miscellaneous goods and services" = "#37a463",
                 "Information and communication" = "#595959",
                 "Recreation, sport and culture" = "#595959",
                 "Hotels, cafes and restaurants" = "#595959",
                 "Insurance and financial services" = "#595959",
                 "Education" = "#595959")

text_color <- c("Food and non-alcoholic beverages" = "#222222",
                "Dwelling, water, electricity, gas and other fuels" = "#222222",
                "Alcoholic beverages, tobacco" = "#754d06",
                "Clothing and footwear" = "#9a3709",
                "Transport" = "#8e2f8e",
                "Furnishings, dwelling equipment and maintenance" = "#4b4baf",
                "Health" = "#245993",
                "Personal care, social protection and miscellaneous goods and services" = "#1d6339",
                "Information and communication" = "#222222",
                "Recreation, sport and culture" = "#222222",
                "Hotels, cafes and restaurants" = "#222222",
                "Insurance and financial services" = "#222222",
                "Education" = "#222222")

emoji_label <- c("Food and non-alcoholic beverages" = "🍎",
                 "Dwelling, water, electricity, gas and other fuels" = "🏠",
                 "Alcoholic beverages, tobacco" = "🍷",
                 "Clothing and footwear" = "👞",
                 "Transport" = "🚌",
                 "Furnishings, dwelling equipment and maintenance" = "🛏️",
                 "Health" = "💊",
                 "Personal care, social protection and miscellaneous goods and services" = "🧴",
                 "Information and communication" = "📱",
                 "Recreation, sport and culture" = "🎾",
                 "Hotels, cafes and restaurants" = "🍴",
                 "Insurance and financial services" = "💰",
                 "Education" = "🎓")



# Plot --------------------------------------------------------------------

label_data <- data_plot |>
  filter(year %in% c(2022, 2024)) |>
  group_by(household_spending, year) |>
  summarize(value = first(value), .groups = 'drop') |>
  pivot_wider(names_from = year, values_from = value, names_prefix = "year_")


# Configure the plot
p <- ggplot(data_plot) +
  geom_line(aes(x = year, y = value, group = household_spending),
            color = "black",
            linewidth = 0.4)

categories <- unique(data_plot$household_spending)

for (category in categories) {
  subset_data <- subset(data_plot, household_spending == category)

  p <- p +
    geom_line(data = subset_data,
              aes(x = year, y = value),
              color = point_color[category],
              linewidth = 0.5) +
    geom_point(data = subset_data,
               aes(x = year, y = value),
               color = point_color[category],
               size = 2) +
    geom_text(data = label_data |>
                filter(household_spending == category),
              aes(x = 2022,
                  y = year_2022 + case_when(
                    household_spending == "Dwelling, water, electricity, gas and other fuels" ~ 0,
                    household_spending == "Alcoholic beverages, tobacco" ~ 0.9,
                    household_spending == "Clothing and footwear" ~ 0.5,
                    household_spending == "Transport" ~ -0.1,
                    household_spending == "Furnishings, dwelling equipment and maintenance" ~ 0.4,
                    household_spending == "Health" ~ 0.2,
                    household_spending == "Personal care, social protection and miscellaneous goods and services" ~ -0.3,
                    household_spending == "Information and communication" ~ -0.3,
                    TRUE ~ 0
                  ),
                  label = paste0(emoji_label[household_spending], " ", year_2022,  "%")),
              color = text_color[category],
              hjust = 1.4,
              size = 3.2,
              family = body_font) +
    geom_text(data = label_data |>
                filter(household_spending == category),
              aes(x = 2024 + 0.1,
                  y = year_2024 + case_when(
                    household_spending == "Dwelling, water, electricity, gas and other fuels" ~ 0,
                    household_spending == "Alcoholic beverages, tobacco" ~ 1.2,
                    household_spending == "Clothing and footwear" ~ 0.5,
                    household_spending == "Transport" ~ -0.1,
                    household_spending == "Furnishings, dwelling equipment and maintenance" ~ 0,
                    household_spending == "Health" ~ -0.38,
                    household_spending == "Personal care, social protection and miscellaneous goods and services" ~ -0.48,
                    household_spending == "Information and communication" ~ 0,
                    household_spending == "Recreation, sport and culture" ~0.25,
                    household_spending == "Hotels, cafes and restaurants" ~-0.3,
                    TRUE ~ 0
                  ),
                  label = paste0(year_2024, "%"," ", emoji_label[household_spending]," ", household_spending)),
              hjust = 0,
              size = 3.2,
              color = text_color[category],
              family = body_font)
}


p <- p +
  scale_x_continuous(limits = c(2022, 2024.5),
                     breaks = seq(from= 2022, to = 2024, by = 1),
                     expand = expansion(mult = c(0.7, 2))) +
  scale_y_continuous(transform = "sqrt",
                     limits = c(0, 35),
                     breaks = seq(from= 0, to = 35, by = 5),
                     expand = expansion(mult = c(0.01, 0.01))) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  theme_void(base_family = body_font, base_size = 12) +
  theme(plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = margin(10, 10, 10, 10),
        plot.title = element_textbox_simple(
          colour = title_col,
          hjust = 0,
          halign = 0,
          margin = margin(b = 10, t = 0),
          lineheight = 1,
          face = "bold",
          size = rel(1.9),
          family = title_font
        ),
        plot.subtitle = element_textbox_simple(
          colour = subtitle_col,
          hjust = 0,
          halign = 0,
          margin = margin(b = 20, t = 5),
          lineheight = 1,
          size = rel(1.1),
          family = subtitle_font
        ),
        plot.caption = element_textbox_simple(
          colour = subtitle_col,
          hjust = 0,
          halign = 0,
          margin = margin(b = 0, t = 15),
          lineheight = 1,
          size = rel(0.7),
          family = subtitle_font
        ),
        axis.text.x = element_text(family = body_font,
                                   color = "#595959",
                                   size = rel(0.7),
                                   lineheight = 0.4,
                                   margin = margin(b=5)),
        axis.line.x = element_line(linewidth = 0.4, color = "#595959"),
        axis.ticks.x = element_line(linewidth = 0.4, color = "#595959"),
        axis.ticks.length.x = unit(0.25, "cm")
  ) +
  geom_richtext(
    aes(x = 2022,
        y = 0.05,
        label = "3450"),
    family = body_font,
    size = 3,
    lineheight = 1.2,
    color = "#71757d",
    hjust = 0.5,
    vjust = 1.03,
    label.color = NA,
    fill = NA) +
  geom_richtext(
    aes(x = 2024 ,
        y = 0.05,
        label = "4120*"),
    family = body_font,
    size = 3,
    lineheight = 1.2,
    color = "#71757d",
    hjust = 0.5,
    vjust = 1.03,
    label.color = NA,
    fill = NA) +
  geom_richtext(
    aes(x = 2023 ,
        y = 0.05,
        label = "3860"),
    family = body_font,
    size = 3,
    lineheight = 1.2,
    color = "#71757d",
    hjust = 0.5,
    vjust = 1.03,
    label.color = NA,
    fill = NA) +
  geom_richtext(
    aes(x = 2022 ,
        y = 0.2,
        label = "Total consumption expenditure (RON)"),
    family = body_font,
    size = 3,
    lineheight = 1.2,
    color = "#71757d",
    hjust = 0.2,
    vjust = 1.03,
    label.color = NA,
    fill = NA) +
  coord_capped_cart(bottom = "both")


# Save jpg ----------------------------------------------------------------

ggsave(file.path("2024", "2024-10-18", paste0("20241018", ".jpg")),
       width = 8.5,
       height = 9,
       dpi = 400,
       bg = "#ffffff")
