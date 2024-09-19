
# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(giscoR)
library(grid)
library(ggtext)
library(eurostat)


source("./2024/2024-07-16/theme_ts.R")


# Load data ---------------------------------------------------------------

dii_eu_countries <- read_csv("./2024/2024-07-16/data_plot.csv")


# Load fonts --------------------------------------------------------------

windowsFonts(georg = windowsFont('Georgia'))


# Data wrangling ----------------------------------------------------------

dii_eu_countries <- dii_eu_countries |>
  # Create indicator_code and recode Country
  mutate(
    indicator_code = recode(Indicator,
                            'Enterprises with very high digital intensity index (DII version 3)' = 'Very High',
                            'Enterprises with high digital intensity index (DII version 3)' = 'High',
                            'Enterprises with low digital intensity index (DII version 3)' = 'Low',
                            'Enterprises with very low digital intensity index (DII version 3)' = 'Very Low',
                            'Enterprises with at least low (basic) digital intensity index (DII Version 3)' = 'Basic'
    ),
    Country = recode(Country, 'European Union - 27 countries (from 2020)' = 'EU'),
    Country = ifelse(Country == "France", "France*", Country),
    Value = as.numeric(round(Value, 2))
  ) |>
  # Filter for specific indicators
  filter(indicator_code %in% c('Very Low', 'Low', 'High', 'Very High')) |>
  # Reorder indicator_code as a factor
  mutate(indicator_code = factor(indicator_code,
                                 levels = c("Very Low", "Low", "High", "Very High")))

# Arrange countries by 'Very Low' indicator and reorder factor levels
countries_ordered <- dii_eu_countries |>
  filter(indicator_code == "Very Low") |>
  arrange(Value) |>
  pull(Country)

dii_eu_countries <- dii_eu_countries |>
  # Reorder Country based on the 'Very Low' indicator values
  mutate(
    Country = factor(Country, levels = rev(countries_ordered)),
    indicator_code = fct_rev(indicator_code)  # Reverse factor levels
  )

# Define text -------------------------------------------------------------

title <- "EU's Digital Intensity Index (DII) in 2023"

# Plot --------------------------------------------------------------------

# Define the colors

color_scale <- c(
  "Very Low" = "#26324B",
  "Low" = "#546FA6",
  "High" = "#99AACC",
  "Very High" = "#99AACC")

# Make the stacked chart
dii_countries_chart <- ggplot(
  dii_eu_countries,
  aes(y = Country,
      x = Value,
      fill = indicator_code)) +
  geom_bar(stat = "identity",
           width = 0.65,
           color = "white") +
  scale_fill_manual(values = color_scale,
                    guide = "none") +
  labs(title = "",
       subtitle = "",
       x = "\n% of enterprises"
  ) +
  scale_x_continuous(position = "top",
                     breaks = seq(0, 100, by = 25),
                     labels = scales::percent_format(scale = 1)) +
  theme_ts() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "grey60"),
        axis.title.x = element_text(hjust = 0.03,
                                    size = 8,
                                    colour = "grey60"),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x = element_line(colour = "grey60"),
        axis.ticks.length =  unit(0.23, "cm"),
        axis.text = element_text(size = 6, colour = "grey60",
                                 face = "bold"),
        text = element_text(family = "georg")
  ) +
  lemon::coord_capped_cart(top = "both")

# Shape files containing border polygons of European countries

shp_0 <- get_eurostat_geospatial(resolution = 10,
                                 nuts_level = 0,
                                 year = 2016)

# crsLAEA is a variable that stores a string defining a Coordinate Reference System (CRS)
# in the Proj.4 format.
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

#combine the data for DII with geographic EU data and transform the data frame into
#a sf (spatial) object for creating the map.

shp_eu <- shp_0 |>
  select(geo = NUTS_ID, geometry) |>
  inner_join(dii_eu_countries, by = "geo") |>
  st_as_sf() |>
  st_transform(crs = crsLAEA) |>
  select(Country, Value, indicator_code, geometry)

# Create a bounding box to limit the view of the map via a bounding box
# and reproject the coordinates to Lambert projection.

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

bbox <- function(bb, laeabb, b) {

  bb <- st_sfc(
    st_polygon(list(cbind(
      c(-10.6600, 33.00, 33.00, -10.6600, -10.6600),
      c(32.5000, 32.5000, 71.0500, 71.0500, 32.5000)
    ))),
    crs = crsLONGLAT)

  laeabb <- st_transform(bb, crs = crsLAEA)
  box <- st_bbox(laeabb)
  return(box)
}

b <- bbox()

# Make the map

(dii_eu_map <-
    ggplot() +
    geom_sf(data = shp_eu,
            aes(fill = indicator_code),
            color = "white",
            size = 0.15) +
    coord_sf(crs = crsLAEA,
             xlim = c(b["xmin"], b["xmax"]),
             ylim = c(b["ymin"], b["ymax"])) +
    labs(y = "",
         subtitle = "The highest proportion of enterprises (that participated in the survey) reaching a very high digital intensity was in Finland (13.01%), followed by Malta (11.43%) and the Netherlands (10.97%). Enterprises in Romania (72.07%), Bulgaria (70.64%), and Greece (56.17%) have very low digital intensity.",
         x = "",
         title = "EU's Digital Intensity Index (DII) in 2023",
         caption = "©2024 Cozmina Secula \n Source: Eurostat, EU survey on ICT usage and e-commerce in enterprises \n*Break in series due to change of statistical unit from legal unit to enterprise in 2022") +
    scale_fill_manual(name = "",
                      values = rev(c("#26324B", "#546FA6", "#99AACC", "#99AACC")),
                      drop = F) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.15, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )
    ) +
    theme_minimal() +
    theme(text = element_text(family = "georg"),
          panel.background = element_blank(),
          legend.background = element_blank(),
          legend.position = c(.45, -.02),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(size = 12, color = "#26324B", face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = ggtext::element_textbox_simple(size = 8,
                                                         vjust = 1,
                                                         hjust = 0.5,
                                                         margin = margin(0, 1, 12, 0),
                                                         color = "#26324B"),
          plot.caption = element_text(size = 6, color = "grey60", hjust = 0, vjust = -6),
          axis.title.x = element_text(size = 10, color = "grey20", hjust = 0.5, vjust = -6),
          legend.text = element_text(size = 9, color = "grey20"),
          legend.title = element_text(size = 11, color = "grey20"),
          strip.text = element_text(size = 12),
          plot.margin = unit(c(t =-4, r = 0, b = -4, l = 10), "lines"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())
)


# Bring together stacked bar and the map

chart_map <- function(m, c, vp) {
  m <- dii_eu_map
  c <- dii_countries_chart
  vp <- viewport(width = 0.3,
                 height = 0.85,
                 x = 0.15,
                 y = 0.52)
  png("./2024/2024-07-16/20240716.png", height = 4200, width = 4000, res = 600)
  grid.newpage()
  print(m)
  print(c + labs(title = ""), vp = vp)
  dev.off()
  return()
}

chart_map()

dii_eu <- chart_map()
print(dii_eu)

