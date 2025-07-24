library(readxl)
library(naniar)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)

listedfilesOK <- list.files(path = "data", pattern = "CaLSeN_OK_\\d{4}\\.xlsx$", full.names = TRUE)

# Look up table for read_xl sheet
sheet_lookup <- c(
  "CaLSeN_OK_2019.xlsx" = 1,
  "CaLSeN_OK_2021.xlsx" = 1,
  "CaLSeN_OK_2022.xlsx" = 2,
  "CaLSeN_OK_2023.xlsx" = 2,
  "CaLSeN_OK_2024.xlsx" = 2
)

waypointspecieslistOK <- lapply(listedfilesOK, function(filename){
  sheet_num <- sheet_lookup[basename(filename)]
  
  OKwp <- read_excel(path = filename, sheet = sheet_num)
  names(OKwp) <- names(OKwp) %>% 
    tolower() %>%
    trimws() %>%
    gsub(" ", "_", .)
  
  OKwp <- OKwp %>% 
    mutate(across(where(is.character), ~ trimws(.)))
  
  # Fix typos and harmonize columns
  if ("othere_species" %in% names(OKwp)) names(OKwp)[names(OKwp) == "othere_species"] <- "other_species"
  if ("other" %in% names(OKwp)) names(OKwp)[names(OKwp) == "other"] <- "other_species"
  if ("waypoints" %in% names(OKwp)) names(OKwp)[names(OKwp) == "waypoints"] <- "waypoint_number"
  
  if ("date" %in% names(OKwp)) {
    OKwp$date <- as.character(OKwp$date)
  }
  
  print(paste("File:", filename, "date class:", class(OKwp$date)))
  print(head(OKwp$date))
  # Check for any non-character
  nonchar <- !sapply(OKwp$date, is.character)
  if (any(nonchar)) {
    print(paste("Non-character types found in", filename, "at rows:", which(nonchar)))
    print(OKwp$date[nonchar])
  }
  
  return(OKwp)}
)


ixodeswpOK <- do.call(rbind, waypointspecieslistOK)

# Fixes dates
# Replace st, nd, rd, and th from text dates so they can be parsed
ixodeswpOK$date <- gsub("(\\d+)(st|nd|rd|th)", "", ixodeswpOK$date, ignore.case = TRUE)

ixodeswpOK$date <- as.character(ixodeswpOK$date)

ixodeswpOK$date <- str_replace_all(ixodeswpOK$date, "[.]", "-")
excel_serials <- grepl("^\\d{4,6}$", ixodeswpOK$date)
ixodeswpOK$date[excel_serials] <- as.Date(as.numeric(ixodeswpOK$date[excel_serials]), origin = "1899-12-30")
ixodeswpOK$date <- as.character(as.Date(parse_date_time(ixodeswpOK$date, orders = c("ymd", "dmy", "mdy"))))
ixodeswpOK$date <- as.Date(ixodeswpOK$date)

# Converts all the columns that should only contain numbers to numeric and removes letters
numeric_fields <- c("latitude", "longitude", "larva", "nymphs", "male", "females", "litter_depth", "soil_humidity", "canopy_cover")

for (field in numeric_fields){
  if (field %in% names(ixodeswpOK)){
    ixodeswpOK[[field]] <- as.numeric(gsub("[^0-9.-]", "", ixodeswpOK[[field]]))
  }
}

# Add tally and presence columns
ixodeswpOK <- ixodeswpOK %>%
  mutate(
    tick_total = rowSums(across(any_of(c("nymphs", "larva", "females", "male"))), na.rm = TRUE),
    tick_presence = if_else(tick_total > 0, 1, 0),
    tick_density = tick_total/200
  )

View(ixodeswpOK)
write.csv(ixodeswpOK, "Data/Aggregated Data/IxodesWaypointOK1924.csv", na = "", row.names = FALSE)
