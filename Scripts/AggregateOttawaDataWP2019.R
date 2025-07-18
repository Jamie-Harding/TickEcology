library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

process_one_file <- function(file_path) {
  df <- read_excel(file_path, sheet = "Specimen info")
  # Makes sure column names are consistent
  names(df) <- names(df) |> trimws() |> gsub(" ", "_", x = _)
  names(df) <- gsub("Litte_depth", "Litter_depth", names(df))
  names(df) <- gsub("Provice", "Province", names(df))
  names(df) <- gsub("Litter_depth_\\(cm\\)", "Litter_depth", names(df))
  names(df) <- gsub("Canopy_cover,_%", "Canopy_cover", names(df))
  names(df) <- gsub("Soil_humidity_\\(%\\)", "Soil_humidity", names(df))
  names(df) <- make.unique(names(df), sep = "_dup")
  
  
  # Printout to see what columns the file has
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
  
  # Filters out columns that don't have environmental data and aggregates their tick counts to every 8th waypoint
  df <- df %>%
    fill(Site_ID, Date, Province) %>%
    arrange (Site_ID, Date) %>%
    group_by(Site_ID, Date) %>%
    mutate(RowNum = row_number()) %>%
    mutate(GroupID = (RowNum - 1) %/% 8 + 1) %>%
    group_by(GroupID, Site_ID, Date) %>%
    summarise(Male = sum(Male, na.rm = T),
              Females = sum(Females, na.rm = T),
              Nymphs = sum(Nymphs, na.rm = T),
              Larva = sum(Larva, na.rm = T),
              Litter_depth = last(Litter_depth),
              Canopy_cover = last(Canopy_cover),
              Soil_humidity = last(Soil_humidity),
              Site_ID = last(Site_ID),
              Date = last(Date),
              Waypoints = last(Waypoints),
              Latitude = last(Latitude),
              Longitude = last(Longitude),
              Province = last(Province),
              .groups = "drop")
  return(df)
}


# Define folders CHANGE FOLDER LOCATION
user_home <- Sys.getenv("USERPROFILE")
folder1 <- file.path(user_home, "RStudio/TickEcology/Data/Ottawa and Kingston CaLSeN 2019/Kingston 2019")
folder2 <- file.path(user_home, "RStudio/TickEcology/Data/Ottawa and Kingston CaLSeN 2019/Ottawa 2019")

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
write.csv(combined_df, "RemoteSensing/KingstonOttawa2019.csv")
