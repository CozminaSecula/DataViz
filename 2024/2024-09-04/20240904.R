
# Install and Load packages -----------------------------------------------------------

packages <- c("tidyverse")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}


library(tidyverse)
source("./2024/2024-09-04/theme_ts.R")


# Load data ---------------------------------------------------------------

df <- read.csv("./2024/2024-09-04/20240904.csv")


# Plot --------------------------------------------------------------------

df |>
  mutate(Country = fct_reorder(country, CountriesWithTheMostHolidaysNumberOfPublicHolidays)) |>
  ggplot(aes(x = CountriesWithTheMostHolidaysNumberOfPublicHolidays, y = Country)) +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.9,
           fill = "#7F9DA7FF") +
  geom_vline(xintercept = 0) +
  scale_x_continuous( name = "X Axis (Bottom)",
                      sec.axis = sec_axis(~., name = "X Axis (Top)") ) +
  labs(
    x = "",
    y = "",
    title = str_wrap("Nepal has the world's highest number of public holidays, with 35 annually",
                     width = 50),
    subtitle = "Number of Public Holidays",
    caption = "Data Source: https://data.world/makeovermonday/2024w36-countries-with-the-most-holidays-2024"
  ) +
  theme_ts() +
  theme(axis.text.y = element_text(size = 10,
                                   hjust = 0,
                                   lineheight = 1.5,
                                   color = "#828282"),
        axis.text.x = element_text(size = 10,
                                   color = "#828282"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#7F9DA7FF",
                                          linetype = "dotted"),
        axis.ticks.x = element_line(linewidth = 0.5, color = "#929497"),
        axis.ticks.length = unit(.25, "cm"),
        plot.subtitle = element_text(hjust = 0.33, color = "#646369"),
        plot.title = element_text(color = "#414040"),
        plot.caption = element_text(hjust = 0, color = "#828282"),
        plot.caption.position = "plot")

# Save png ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-09-04", paste0("20240904", ".png")),
  width = 9.7,
  height = 11.7,
  dpi = 300)
