library(readxl)
library(naniar)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)

listedfiles <- list.files(path = "data", pattern = "CaLSeN_GU_\\d{4}\\.xlsx$", full.names = TRUE)

waypointspecieslist <- lapply(listedfiles, function(filename){
  
dfwp <- read_excel(path = filename, sheet = "IxodesWaypoint")
names(dfwp) <- names(dfwp) %>% 
  tolower() %>%
  gsub(" ", "_", .)

  dfwp <- dfwp %>% 
    mutate(across(where(is.character), ~ trimws(.)))
  
  dfwp$date <- gsub("(\\d+)(st|nd|rd|th)", "", dfwp$date, ignore.case = TRUE)

  # Fixes date
  if(inherits(dfwp$date, "POSIXct")){
    dfwp$date <- as.Date(dfwp$date)
  } else if (is.character(dfwp$date)){
    dfwp$date <- ifelse(
      grepl("^\\d{4,6}$", dfwp$date),
      as.character(as.Date(as.numeric(dfwp$date), origin = "1899-12-30")),
      as.character(parse_date_time(dfwp$date, orders = c("mdy", "ymd", "dmy"))))
   } else {
    dfwp$date <- ymd(dfwp$date)
  }
  
  
return(dfwp)}
)

ixodeswp <- do.call(rbind, waypointspecieslist)

numeric_fields <- c("latitude", "longitude", "larva", "nymphs", "male", "females", "litter_depth", "soil_humidity", "canopy_cover")

# Converts all the columns that should only contain numbers to numeric and removes letters
for (field in numeric_fields){
  if (field %in% names(ixodeswp)){
  ixodeswp[[field]] <- as.numeric(gsub("[^0-9.-]", "", ixodeswp[[field]]))
  }
}

# Add tally and presence columns
ixodeswp <- ixodeswp %>%
  mutate(
    tick_total = rowSums(across(any_of(c("nymphs", "larva", "females", "male"))), na.rm = TRUE),
    tick_presence = if_else(tick_total > 0, 1, 0),
    tick_density = tick_total/200
  )

write.csv(ixodeswp, "Data/Aggregated Data/IxodesWaypointGU1924.csv", na = "", row.names = FALSE)


