library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(here)

process_one_file <- function(file_path) {
# Removes (full data) files from analyzed files
  if (grepl("\\(full data\\)\\.xlsx$", basename(file_path))) {
    message("Skipping file: ", basename(file_path), " (full data)")
    return(NULL)}
  df <- read_excel(file_path, sheet = "Specimen info")
  # Makes sure column names are consistent
  names(df) <- names(df) |> trimws() |> gsub(" ", "_", x = _)
  names(df) <- gsub("Litte_depth", "Litter_depth", names(df))
  names(df) <- gsub("Provice", "Province", names(df))
  names(df) <- gsub("Litter_depth_\\(cm\\)", "Litter_depth", names(df))
  names(df) <- gsub("Canopy_cover,_%", "Canopy_cover", names(df))
  names(df) <- gsub("Soil_humidity_\\(%\\)", "Soil_humidity", names(df))
  names(df) <- make.unique(names(df), sep = "_dup")
  
  
  # Removes distance and canopy cover squares columns
  df <- df %>%
    select(
      -matches("^distance_\\(m\\)$"), 
      -matches("^canopy_cover_\\(squares_not_occupied\\)$"))
  
  
  # Debugging printout to see what columns the file has
  message("Column names for ", basename(file_path), ": ", paste(names(df), collapse = ", "))
  
  # Force numeric for numeric columns only if they exist
  for (col in c("Male", "Females", "Nymphs", "Canopy_cover", "Soil_humidity", "Waypoints")) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  
  # Makes missing columns in filter NA
  for (col in c("Litter_depth", "Soil_humidity", "Canopy_cover")) {
    if (!(col %in% names(df))) {
      df[[col]] <- NA
    }
  }
  
  # Adds Region column based on prefix and adds Other_species column for consistency with other datasets
  df <- df %>%
    mutate(
      Region = case_when(
        str_sub(Site_ID, 1, 2) == "KG" ~ "Kingston",
        str_sub(Site_ID, 1, 2) == "OG" ~ "Ottawa-Gatineau",
        TRUE ~ NA_character_
      ),
      Other_species = NA_character_
    )
  
  return(df)
}


# Define folders
folder1 <- here("Data", "Ottawa and Kingston CaLSeN 2021", "Kingston 2021")
folder2 <- here("Data", "Ottawa and Kingston CaLSeN 2021", "Ottawa 2021")

# List files matching patterns in both folders
files1 <- list.files(path = folder1, pattern = "^Data_KG.*\\.xlsx$", full.names = TRUE)
files2 <- list.files(path = folder2, pattern = "^Data_OG.*\\.xlsx$", full.names = TRUE)

# Combine file list
all_files <- c(files1, files2)

# Process and combine
combined_df <- map_dfr(all_files, process_one_file)

# View in RStudio
View(combined_df)

# Save combined result CHANGE FILE NAME
write_xlsx(combined_df, "Data/CaLSeN_OK_2021.xlsx")
