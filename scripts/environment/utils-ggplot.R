# ------------------------------------------------------------------------------
# month_year_labeller

# Used in charts to display custom date labels on chart facets.
# Only the month and year is displayed.
# Ex: "Jan 2024"

month_year_labeller <- function(labels) {
  date <- as.Date(labels, format = "%Y-%m-%d")
  formatted_labels <- format(date, "%b %Y")
  return(formatted_labels)
}


# ------------------------------------------------------------------------------

ensure_dir <- function(path) {
  ifelse(dir.exists(path), FALSE, dir.create(path))
}


# ------------------------------------------------------------------------------

save_plot <- function(name, width = 30, height = 20) {
  ensure_dir(config.output_dir)
  filePath <- file.path(config.output_dir, name)
  ggsave(filePath, width = width, height = height, units = "cm", dpi = 300)
}