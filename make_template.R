
# Defining a date as a variable, then extracting the year and the date (without the - separators) from it.
# https://nrennie.rbind.io/blog/script-templates-r/


use_dviz_template <- function(date_chr = "2024-09-18",
                             readme = TRUE) {
  # Check date is in the correct format
  if (is.na(as.Date(date_chr, format = "%Y-%m-%d"))) {
    stop("'date_chr' is in incorrect format. Should be yyyy-mm-dd.")
  }

  # Get year from date
  yr <- sub("-.*", "", date_chr)
  date_strip <- stringr::str_remove_all(date_chr, "-")

  # Create the year and date folders if they don’t already exist
  new_folder <- file.path(yr, date_chr)
  if (!file.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
    message("Created folder: ", new_folder)
  } else {
    message("Folder already exists: ", new_folder)
  }

  # Create the .R file named date_strip.R
  new_r_file <- file.path(yr, date_chr, paste0(date_strip, ".R"))
  if (!file.exists(new_r_file)) {
    file.create(new_r_file)
    message("Created '.R' file: ", new_r_file)
  } else {
    message("'.R' file already exists: ", new_r_file)
  }

  # Optionally, create a README.md file
  if (readme) {
    new_readme <- file.path(yr, date_chr, "README.md")
    if (!file.exists(new_readme)) {
      file.create(new_readme)
      message("Created 'README.md' file: ", new_readme)
    } else {
      message("'README.md' file already exists: ", new_readme)
    }

    # Check if readme_template.md exists and copy the content
    template_file <- file.path("utils", "readme_template.md")
    if (file.exists(template_file)) {
      readme_txt <- readLines(template_file)

      # Replace placeholder text with actual values
      readme_txt <- gsub(pattern = "yr", replacement = yr, x = readme_txt)
      readme_txt <- gsub(pattern = "date_chr", replacement = date_chr, x = readme_txt)
      readme_txt <- gsub(pattern = "date_strip", replacement = date_strip, x = readme_txt)

      # Write the updated content to README.md
      writeLines(readme_txt, con = new_readme)
      message("'README.md' contents copied successfully to ", new_readme)
    } else {
      warning("Template file 'readme_template.md' not found.")
    }
  }

  # Check if r_template.R exists and copy the content
  template_r_file <- file.path("utils", "r_template.R")
  if (file.exists(template_r_file)) {
    r_txt <- readLines(template_r_file)

    # Replace placeholder text with actual values
    r_txt <- gsub(pattern = "yr", replacement = paste0("\"", yr, "\""), x = r_txt)
    r_txt <- gsub(pattern = "date_chr", replacement = paste0("\"", date_chr, "\""), x = r_txt)
    r_txt <- gsub(pattern = "date_strip", replacement = paste0("\"", date_strip, "\""), x = r_txt)

    # Write the updated content to the new .R file
    writeLines(r_txt, con = new_r_file)
    message("'.R' file contents copied successfully to ", new_r_file)
  } else {
    warning("Template file 'r_template.R' not found.")
  }
}

use_dviz_template("2024-07-16", readme = TRUE)
