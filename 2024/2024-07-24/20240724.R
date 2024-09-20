
# Load packages -----------------------------------------------------------

library(tidyverse)
library(camcorder)
library(ggtext)
library(scales)

# Load data ---------------------------------------------------------------

df <- read.csv("./2024/2024-07-24/20240724.csv")


# Data wrangling ----------------------------------------------------------

# Change `Indicator` values with shorter versions
# Exclude regions, keep data only for 27 countries

data_cleaned <- df |>
  filter(Geo == "_z" &
           Country != "European Union - 27 countries (from 2020)") |>
  mutate(Value = round(Value, 1),
         Indicator_code = case_when(
           Indicator == "Enterprises use at least one of the AI technologies: AI_TTM, AI_TSR, AI_TNLG, AI_TIR, AI_TML, AI_TPA, AI_TAR" ~ "Use any AI technology",
           Indicator == "Enterprises where web sales are more than 1% of total turnover and B2C web sales more than 10% of the web sales" ~ "Have web sales >1% turnover, B2C web sales >10% web sales",
           Indicator == "Buy cloud computing services used over the internet" ~ "Buy cloud computing services used over the internet",
           Indicator == "Enterprises using Customer Relationship Management (CRM) software (as of 2023)" ~ "Use CRM software",
           Indicator == "Enterprises where more than 50% of the persons employed have access to the internet for business purposes" ~"Access to the internet for over 50% of employees",
           Indicator == "Enterprises who have ERP software package to share information between different functional areas" ~"Have ERP software",
           Indicator == "Enterprises with e-commerce sales of at least 1% turnover" ~"Have e-commerce sales of at least 1% turnover",
           Indicator == "Use two or more social media (as of 2014)" ~"Use two or more social media",
           Indicator == "Enterprises buying sophisticated or intermediate CC services, at least one of: CC_PFACC, CC_PERP, CC_PCRM, CC_PSEC, CC_PDB, CC_PDEV" ~ "Buy sophisticated or intermediate cloud computing services",
           Indicator == "Data analytics for the enterprise is performed by the enterprise's own employees or by an external provider" ~"Use data analytics*",
           Indicator == "Use any social media (as of 2014)" ~ "Use any social media",
           Indicator == "The maximum contracted download speed of the fastest fixed line internet connection is at least 30 Mb/s" ~ "Have fastest fixed line internet at least 30 Mb/s",
           TRUE ~"other")) |>
  select(Country, Indicator, Indicator_code, Value)

# Prepare data for the plot

max_min_value <- data_cleaned |>
  group_by(Indicator_code) |>
  mutate(Rank_value = rank(Value)) |>
  filter(Rank_value %in% c(1, 27)) |>
  mutate(Rank_value = ifelse(Rank_value == 1, "Lowest", "Highest")) |>
  select(Country, Indicator_code, Value, Rank_value) |>
  ungroup()|>
  mutate(Rank_value = factor(Rank_value, levels = c("Highest", "Lowest"))) |>
  arrange(desc(Value)) |>
  mutate(Indicator_code = factor(Indicator_code, levels = rev(unique(Indicator_code))))


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("2024", "2024-07-24", "recording"),
  device = "png",
  width = 9.7,
  height = 11.7,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

title <- "EU countries with the highest and lowest scores on Digital Intensity index components"
subtitle <- paste0("Finland scored the highest in 4 out of 12 components,
while Bulgaria scored the lowest in 5 out of 12 components of the digital intensity index.<br> <span style='font-size:12pt'>% enterprises (10 persons employed or more)")
caption <- "Graph: @cozminasecula \nData: Eurostat, EU survey on ICT usage and e-commerce in enterprises (2023). \n*Data analytics was the new component added in 2023, replacing the 'Enterprises use IoT'."


# Plot --------------------------------------------------------------------


ggplot(max_min_value, aes(x = Indicator_code,
                          y = Value,
                          fill = Rank_value)
       ) +
  geom_bar(position = position_dodge2(width = NULL,
                                      padding = 0.1,
                                      preserve = "single",
                                      reverse = TRUE),
           stat = "identity",
           width = 0.95) +
  coord_flip() +
  scale_x_discrete(labels = scales::label_wrap(30)) +
  scale_y_continuous(limits = c(0,120)) +
  scale_fill_manual(values = c("Highest" = "#99AACC", "Lowest" = "#26324B")) +
  ggtext::geom_textbox(aes(
    label = paste0("<span style='font-size:11pt'><br>",
                   Value, "%",
                   "<br></span>",
                   Country),
    hjust = ifelse(Rank_value == "Lowest",  0.65, 1),
    halign = ifelse(Rank_value == "Lowest", 1, 1),
    vjust = ifelse(Rank_value == "Lowest" , 1, -0.2),
    fill = ifelse(Rank_value == "Lowest" ,"white", "#1A242F")),
    size = 3,
    family = "Georgia",
    fill = NA,
    box.colour = NA,
    fontface = "bold",
    position = position_dodge2(width = 0.95, preserve = "single")) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption) +
  theme_minimal() +
  theme(
    panel.grid = element_blank() ,
    axis.title = element_blank(),
    axis.text.x = element_blank() ,
    axis.text = element_text(size = 12,
                             family = "Georgia",
                             color = "#1A242F"),
    plot.subtitle = ggtext::element_textbox_simple(size = 18,
                                                   vjust = 1,
                                                   margin = margin(0, 0, 12, 0),
                                                   color = "#1A242F",
                                                   family = "Georgia"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 22,
                                        color = "#1A242F",
                                        face = "bold",
                                        family = "Georgia",
                                        margin = margin(12, 0, 12, 0)),
    plot.caption.position = "plot",
    plot.caption = element_text(color = "#828282"
                                , size = 8,
                                hjust = 0,
                                family = "Georgia"),
    legend.position = "top",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.justification = 0.04,
    legend.text = element_text(family = "Georgia",
                               size = 12,
                               color = "#1A242F"),
  )


# Save gif ----------------------------------------------------------------

ggsave(
  file.path("2024", "2024-07-24", paste0("20240724", ".png")),
  width = 9.7,
  height = 11.7,
  dpi = 300)


gg_playback(
  name = file.path("2024", "2024-07-24", paste0("20240724", ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  bg = "white"
)
