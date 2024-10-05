theme_rdv <- function() {
  theme_minimal(base_size = 10, base_family = "lato") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(linewidth = 0.5, color = "#000000"),
      axis.text = element_text(color = "#000000", family = "lato", size = 10),
      axis.ticks = element_line(color = "#000000", linewidth = 0.5),
      axis.title = element_text(color = "#000000", family = "lato", size = 14),
      axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15)),
      axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 0, 0)),
      plot.subtitle = element_text(color = "#000000", size = 16, family = "lato"),
      plot.title = element_text(color = "#000000", size = 20, family = "lato"),
      plot.title.position = "plot",
      plot.caption = element_text(hjust = 0, color = "#000000", size = 11, family = "lato"),
      plot.caption.position = "plot",
      strip.text = element_text(color = "#000000", family = "Lato")
    )
}


