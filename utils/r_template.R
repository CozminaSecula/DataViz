
# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)
library(camcorder)
library(ggtext)
library(glue)


# Load data ---------------------------------------------------------------



# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colors and fonts ----------------------------------------------------------

bg_col <- ""
text_col <- ""

body_font <- ""
title_font <- ""

# Data wrangling ----------------------------------------------------------



# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path(yr, date_chr, "recording"),
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", "DataViz:"
)


# Plot --------------------------------------------------------------------



# Save gif ----------------------------------------------------------------

gg_playback(
  name = file.path(yr, date_chr, paste0(date_strip, ".gif")),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = bg_col
)
