
# Install and Load packages -----------------------------------------------------------

packages <- c("tidyverse", "ggrepel", "ggtext", "lemon", "showtext")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tidyverse)
library(ggrepel)
library(ggtext)
library(lemon)
library(showtext)

# Load data ---------------------------------------------------------------

df <- read.csv("./2024/2024-09-26/20240926.csv")

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "roboto"
title_font <- "roboto_slab"

# Define colors and fonts ----------------------------------------------------------

text_col <- "grey10"
bg_col <- "#FBFBF9"


# Data wrangling ----------------------------------------------------------

# Rename columns

dd_data <- df |>
  mutate(country = as.factor(country.x),
         bt_tech_indic = as.factor(indicator.x),
         bt_sme_indic = as.factor(sme_indicator),
         hc_indiv_indic = as.factor(indicator.y),
         hc_ict_spec_indic = as.factor(ifelse(!is.na(ict_value), "Employed Ict specialist", NA)),
         ps_indic = as.factor(variable),
         bt_tech_value = dtech_value,
         bt_sme_value = sme_value,
         hc_indiv_value = value,
         hc_ict_spec_value = ict_value) |>
  select(year, geo, country, bt_tech_indic, bt_sme_indic, hc_indiv_indic, hc_ict_spec_indic, ps_indic, bt_tech_value, bt_sme_value, hc_indiv_value,  hc_ict_spec_value, ps_value)

# Filter data to include only data analytics indicator

dd_data_year_da <- dd_data |>
  filter(bt_tech_indic %in% c("Analyse big data internally from any data source or externally",
                              "Data analytics for the enterprise is performed by the enterprise's own employees or by an external provider")) |>
  mutate(bt_tech_indic = ifelse(bt_tech_indic == "Analyse big data internally from any data source or externally", "Big Data 2020", "Data Analytics 2023"),
         country = as.character(country),
         country = ifelse(country == "European Union - 27 countries (from 2020)", "EU", country)) |>
  distinct(year,geo, country, bt_tech_indic, bt_tech_value)

# Prepare data for the plot

data_plot <- dd_data_year_da |>
  pivot_wider(names_from = year,
              values_from = bt_tech_value) |>
  pivot_wider(names_from = bt_tech_indic,
              values_from = c(`2020`, `2023`)) |>
  rename(`2020` = `2020_Big Data 2020` ,
         `2023` = `2023_Data Analytics 2023`) |>
  mutate(`2020` = round(`2020`, 0),
         `2023` = round(`2023`, 0)) |>
  select(country, `2020`, `2023`)


# Define text -------------------------------------------------------------

subtitle <- paste0(
  "In the EU, 33% of enterprises use data analytics, with a <br>target of 75% by 2030.<br>",
  "<span style='font-size:13px'>% of enterprises (10 persons employed or more)</span>"
)

caption <- paste0("**Graphic**: Cozmina Secula<br>**Data**: Eurostat,
                  EU survey on ICT usage and e-commerce in enterprises. <br>**Note**:
                  In 2023, Eurostat replaced the Big Data indicator with the Data Analytics one.
                  <br>Data Analytics includes a broader set of technologies than the former Big Data.")

title <- paste0("EU Enterprises and Data Analytics in 2023")

callout <- paste0("EU countries improved their <br>performance when the <b>Data <br> Analytics</b> indicator replaced <b>Big Data</b>.<br> However, increased EU and <br>national efforts may be needed <br>to reach 75% adoption by 2030.")

# Define the countries of interest
highlight_countries <- c("Hungary", "Croatia", "Denmark", "Romania", "Malta", "EU")

country_color <- c("Hungary" = "#0C8040",
                   "Croatia" = "#1f2e7a",
                   "Denmark" = "#c91d42",
                   "Romania" = "#f97a1f",
                   "Malta" = "#475ed1",
                   "EU" = "#0d0d0d",
                   "Other" = "#E5E5E5")


# Plot --------------------------------------------------------------------

# Filter the data once
filtered_data <- data_plot |>
  filter(country %in% highlight_countries)


ggplot(data_plot) +
  # Points for all countries
  geom_point(data = filtered_data,
             aes(x = 1,
                 y = `2020`,
                 color = ifelse(country %in% highlight_countries, country, "Other"),
                 alpha = 0.3),
             size = 0.4) +

  # Arrows for all countries
  geom_segment(aes(x = 1,
                   y = `2020`,
                   xend = 2,
                   yend = `2023`,
                   color = ifelse(country %in% highlight_countries, country, "Other")),
               arrow = arrow(length = unit(0.15, "cm")),
               size = 0.3) +

  # Arrows only for specific countries
  geom_segment(data = filtered_data,
               aes(x = 1,
                   y = `2020`,
                   xend = 2,
                   yend = `2023`,
                   color = ifelse(country %in% highlight_countries, country, "Other"),
                   group = country),
               arrow = arrow(length = unit(0.15, "cm")),
               size = 0.3) +

  # Percentages for 2020
  geom_text(data = filtered_data,
            aes(x = 1,
                y = `2020` + ifelse(country == "EU", -1,
                                    ifelse(country == "Croatia", 0.8, 0)),
                label = paste0(`2020`, "%"),
                color = country,
                family = body_font),
            hjust = 1.4,
            size = 5) +

  # Percentages and country names for 2023
  geom_text(data = filtered_data,
            aes(x = 2,
                y = `2023` +
                  ifelse(country == "Hungary", 0.95,
                         ifelse(country == "Croatia", 0.05, 0)),
                label = paste0(`2023`, "%  ", country),
                color = country,
                family = body_font),
            hjust = -0.1,
            size = 5) +
  scale_color_manual(values = country_color) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("2020", "2023"),
                     limits = c(0, 2.5),
                     expand = expansion(mult = c(0, 0.15))) +

  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(from = 5, to = 55, by = 10),
                     expand = expansion(mult = c(0.02, 0.02)),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption) +
  theme_void(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 1.2,
      face = "bold",
      size = rel(1.9),
      family = title_font
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 5),
      lineheight = 1,
      size = rel(1.4),
      family = title_font
    ),
    plot.caption = element_textbox_simple(
      colour = "#828282",
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      lineheight = 1,
      family = body_font
    ),
    axis.text.x = element_text(
      family = body_font,
      colour = "#595959",
      size = rel(1.2),
      lineheight = 0.4,
      margin = margin(b = 5)
    ),
    axis.line.x = element_line(linewidth = 0.4, color = "#595959"),
    axis.ticks.x = element_line(linewidth = 0.4, color = "#595959"),
    axis.ticks.length.x = unit(0.25, "cm")
  ) +
  geom_richtext(
    aes(x = 0.69,
        y = 2,
        label = "Big Data"),
    family = body_font,
    size = 5,
    lineheight = 1.2,
    color = "#595959",
    hjust = 0,
    vjust = 1.03,
    label.color = NA,
    fill = NA) +
  geom_richtext(
    aes(x = 1.8,
        y = 2,
        label = "Data Analytics"),
    family = body_font,
    size = 5,
    lineheight = 1.2,
    color = "#595959",
    hjust = 0,
    vjust = 1.03,
    label.color = NA,
    fill = NA) +
  geom_richtext(
    aes(x = 0.1,
        y = 55,
        label = callout),
    family = body_font,
    size = 5,
    lineheight = 1.2,
    color = "#595959",
    hjust = 0,
    vjust = 1,
    label.color = NA,
    fill = NA) +
  lemon::coord_capped_cart(bottom = "both")


# Save png ----------------------------------------------------------------

ggsave(file.path("2024", "2024-09-26", paste0("20240926", ".png")),
       width = 7,
       height = 8,
       units = "in",
       dpi = 300,
       bg = bg_col)
