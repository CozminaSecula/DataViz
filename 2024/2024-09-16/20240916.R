
# Install and Load packages -----------------------------------------------------------

packages <- c("tidyverse", "geofacet", "ggtext")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}


library(tidyverse)
library(geofacet)
library(ggtext)

# Load data ---------------------------------------------------------------

df <- read.csv("./2024/2024-09-16/20240916.csv")

# Load fonts --------------------------------------------------------------

windowsFonts(Georgia = windowsFont("Georgia"))

# Define colors and fonts ----------------------------------------------------------

palette_worktime <- c("Full-time" = "#950404FF",
                      "Part-time" = "#0F542FFF")

bg_color <- "#FBFBF9"


# Data wrangling ----------------------------------------------------------

eu_grid1 <- eu_grid1[eu_grid1$name != "United Kingdom", ]
eu_grid1$name[eu_grid1$name == "Czech Republic"] <- "Czechia"

plot_data <- df |>
  filter(worktime %in% c("Full-time", "Part-time"),
         sex == "Total") |>
  select(geo, year, worktime, values)

# Define text -------------------------------------------------------------

title <- paste0("How Many Hours do Europeans Work Each Week?")

subtitle <- paste0("The average weekly work hours for EU employees in 2023 was 36.1.
                   Only in Cyprus, Romania, France, and Lithuania, employees worked more hours in 2023 than in 2014.
                   <br>In 2023, EU <span style='color:#950404FF'><b>full-time</b></span> employees worked an average
                   of 39 hours per week, while <span style='color:#0F542FFF'><b>part-time</b></span> employees worked 21.7 hours.
                   Romania had the highest average <span style='color:#0F542FFF'><b>part-time</b></span> hours, at 26.7.")


# Plot --------------------------------------------------------------------

plot_data |>
  ggplot(aes(
    x = year,
    y = values,
    color = worktime)
  ) +
  geom_line(linewidth = 0.8) +
  facet_geo(vars(geo),
            grid = "eu_grid1") +
  scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), 3)) +
  scale_color_manual(values = palette_worktime) +
  labs(
    x = NULL,
    y = NULL,
    title = title,
    subtitle = str_wrap(subtitle, width = 60),
    caption = str_glue("**Data:** Eurostat (lfsa_ewhais, 2024)")
  ) +
  theme_minimal() +
  theme(strip.text = element_text(family = "Georgia", size = 10),
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(hjust = 1),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text = element_text(family = "Georgia",
                                 color = "#414040",
                                 size = 9,
                                 hjust = 0.5),
        plot.title = element_markdown(color = "#414040",
                                      size = 22,
                                      family = "Georgia",
                                      face = "bold",
                                      margin = margin(0, 0, 12, 0)),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(size = 15,
                                               vjust = 1,
                                               margin = margin(0, 0, 12, 0),
                                               color = "#646369",
                                               family = "Georgia"),
        plot.caption = element_textbox_simple(color = "#828282" ,
                                              size = 10,
                                              hjust = 0,
                                              family = "Georgia",
                                              margin = margin(12, 0,0, 0)),
        plot.caption.position = "plot",
        plot.margin = margin(10,10,10,10)
  )


# Save png ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-09-16", paste0("20240916", ".png")),
  height = 11,
  width = 10.5,
  bg = bg_color,
  units = "in",
  dpi = 300)
