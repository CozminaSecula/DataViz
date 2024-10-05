
# Install and Load packages -----------------------------------------------------------

packages <- c("tidyverse", "janitor", "showtext", "ggtext")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)

source("./2024/2024-10-04/theme_rdv.R")


# Load data ---------------------------------------------------------------

df <- readxl::read_xlsx("./2024/2024-10-04/20241004.xlsx")

# Load fonts --------------------------------------------------------------

font_add_google("Lato", "lato")
showtext_auto()
showtext_opts(dpi = 300)

# Data wrangling ----------------------------------------------------------

df <- df |>
  mutate(date = as.Date(paste0("01/", Date), format = "%d/%m/%Y")) |>
  select(-Date) |>
  clean_names() |>
  pivot_longer(cols = c("euro_stoxx_50", "ftse_100", "msci_china", "nasdaq_100", "nyse_arca_gold_bugs", "s_p_500"), names_to = "index", values_to = "price") |>
  mutate(price = round(price, 2)) |>
  mutate(index = case_when(
    index == "euro_stoxx_50" ~ "EURO STOXX 50",
    index == "ftse_100" ~ "FTSE 100",
    index == "msci_china" ~ "MSCI China",
    index == "nasdaq_100" ~ "Nasdaq-100",
    index == "nyse_arca_gold_bugs" ~ "NYSE Arca Gold BUGS",
    index == "s_p_500" ~ "S&P 500",
    TRUE ~ index
  ))


# Plot --------------------------------------------------------------------

color_palette <- c(
  "EURO STOXX 50" = "#174A7E",
  "FTSE 100" = "#c97b1d",
  "MSCI China" = "#800000",
  "Nasdaq-100" = "#95B3D7",
  "NYSE Arca Gold Bugs" = "#fdbf11",
  "S&P 500" = "#0C8040"
)


caption <- paste0("**Graphic**: Cozmina Secula<br>**Data**: FTSE 100 vs MSCI China vs
                        Nasdaq-100 vs NYSE Arca Gold BUGS vs S&P 500:<br>historical performance from 2008 to 2024 (curvo.eu)")

labels_last_value <- df |>
  group_by(index) |>
  filter(date == last(date)) |>
  summarise(last_price = last(price),
            last_date = last(date)) |>
  ungroup()

break.vec <- c(as.Date("2011-08-01"),
               seq(from = as.Date("2011-08-01"), to = as.Date("2024-07-01"),
                   by = "2 years"),
               as.Date("2024-07-01"))

ggplot(df) +
  geom_line(aes(x = date,
                y = price,
                color = index,
                group = index)) +
  scale_x_date(breaks = break.vec,
               labels = scales::date_format("%Y"),
               expand = expansion(mult = c(0.1, 0.3))
  ) +
  scale_y_continuous(limits = c(0, 100000),
                     breaks = seq(from = 0, to = 100000, by = 20000),
                     labels = scales::dollar_format(scale = 1)) +
  scale_color_manual(values = color_palette) +
  labs(title = "Financial Markets Historical Performance",
       subtitle = "Time period: August 2011 to July 2024",
       caption = caption,
       y = "Value of portfolio",
       x = "") +
  theme_rdv() +
  theme(
    legend.position = "none",
    plot.title.position = "panel",
    plot.caption.position = "panel",
    plot.caption = element_markdown(color = "#000000",
                                    size = 10,
                                    family = "lato")
  ) +
  geom_text(data = labels_last_value,
            aes(x = last_date,
                y = last_price,
                label = index,
                color = index),
            hjust = -0.05,
            vjust = 0.5)


# Save png ----------------------------------------------------------------

ggsave(file.path("2024", "2024-10-04", paste0("20241004", ".png")),
       width = 10,
       height = 7.5,
       units = "in",
       dpi = 300)
