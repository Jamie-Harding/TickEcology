library(readxl)
library(naniar)

listedfiles <- list.files(path = "data", pattern = "CaLSeN_\\d{4}\\.xlsx$", full.names = TRUE)

waypointspecieslist <- lapply(listedfiles, function(filename){
  
dtfr <- read_excel(path = filename, sheet = "IxodesWaypoint")
dtfr$Date <- as.character(dtfr$Date)
return(dtfr)}
)

ixodeswp <- do.call(rbind, waypointspecieslist)

# Data cleaning
# Replace lone decimal points with NA
ixodeswp$Latitude  <- ifelse(ixodeswp$Latitude == ".", NA, ixodeswp$Latitude)
ixodeswp$Longitude <- ifelse(ixodeswp$Longitude == ".", NA, ixodeswp$Longitude)

# Remove anything that's not a digit, minus sign, or decimal point, then convert to numeric
# Force "." or any non-numeric value to NA
ixodeswp$Latitude  <- suppressWarnings(as.numeric(ifelse(grepl("^\\.*$", ixodeswp$Latitude), NA, ixodeswp$Latitude)))
ixodeswp$Longitude <- suppressWarnings(as.numeric(ifelse(grepl("^\\.*$", ixodeswp$Longitude), NA, ixodeswp$Longitude)))



write.csv(ixodeswp, "IxodesWaypoint.csv", na = "")
