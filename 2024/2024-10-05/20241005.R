
# Install and Load packages -----------------------------------------------------------

packages <- c("tidyverse", "showtext", "janitor", "ggtext")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}


library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)

source("./2024/2024-10-05/theme_rdv.R")

# Load data ---------------------------------------------------------------

df <- read_csv("./2024/2024-10-05/20241005.csv")

# Load fonts --------------------------------------------------------------

font_add_google("Lato", "lato")
showtext_auto()

# Data wrangling ----------------------------------------------------------

df_long <- df |>
  pivot_longer(cols = c("EURO STOXX 50", "FTSE 100", "MSCI China", "Nasdaq-100", "NYSE Arca Gold Bugs", "S&P 500"),
               names_to = "index",
               values_to = "performance") |>
  mutate(performance = as.numeric(str_remove(performance, "%"))) |>
  clean_names()

df_long$year <- as.numeric(df_long$year)


# Define text -------------------------------------------------------------

caption <- paste0("**Graphic**: Cozmina Secula<br>**Data**:EURO STOXX 50, FTSE 100, MSCI China,
                        Nasdaq-100, <br>NYSE Arca Gold BUGS, S&P 500: annual returns (curvo.eu)")



# Plot --------------------------------------------------------------------

index_colors <- c(
  "EURO STOXX 50" = "#174A7E",
  "FTSE 100" = "#c97b1d",
  "MSCI China" = "#800000",
  "Nasdaq-100" = "#95B3D7",
  "NYSE Arca Gold Bugs" = "#fdbf11",
  "S&P 500" = "#0C8040"
)


# Create a data frame for text annotations
text_data <- data.frame(
  index = c("EURO STOXX 50", "FTSE 100", "MSCI China",
            "Nasdaq-100", "NYSE Arca Gold Bugs", "S&P 500"),
  label = c("Mild fluctuations, \nrelatively stable \noverall performance.",
            "Small fluctuations, \nmostly flat \nwithout growth.",
            "Volatile, \nwith a noticeable \ndip.",
            "Few peaks, \nstable, and limited \nvolatility.",
            "Highly volatile \nwith sharp \nfluctuations throughout.",
            "Stable performance, \nno drastic changes."),
  x = rep(2024, 6),
  y = c(35, 35, 35, 35, 35, 35)
)


break.vec <- c(as.numeric(2009),
               seq(from = as.numeric(2009), to = as.numeric(2023),
                   by = 2),
               as.numeric(2023))


ggplot(df_long,
             aes(y = performance,
                 x = year,
                 color = index)) +
  geom_line(aes(group = index, colour = index), linewidth = 0.4) +
  geom_point(size = 1.9) +
  annotate("segment",
           x = 2009, xend = 2023.5,
           y = 0, yend = 0,
           colour = "grey60",
           linewidth = 0.5)+
  facet_wrap(~index, scales = "free_y", nrow = 6) +
  scale_color_manual(values = index_colors) +
  scale_y_continuous(limits = c(-60, 70),
                     breaks = seq(-60, 70, by = 20),
                     labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = break.vec,
                     expand = expansion(mult = c(0.08, 0.6))) +
  labs(
    title = "Financial Markets Annual Returns",
    subtitle = "Time period: 2009 to 2023",
    caption = caption,
    x = "",
    y = ""
  ) +
  theme_rdv() +
  theme(legend.position = "none",
        strip.text = element_text(family = "lato", size = 12, hjust = 0.3),
        strip.text.y = element_text(angle = 0),
        plot.title.position = "panel",
        plot.subtitle = element_text(margin = margin(2,0,9,0)),
        plot.caption = element_markdown(color = "#000000",
                                        size = 10,
                                        family = "lato"),
        plot.caption.position = "panel") +
  geom_text(data = text_data,
            aes(x = x, y = y,
                label = label),
            family = "lato",
            size = 4,
            hjust = 0)


# Save png ----------------------------------------------------------------

ggsave(file.path("2024", "2024-10-05", paste0("20241005", ".png")),
       width = 6,
       height = 10,
       units = "in",
       dpi = 300)
