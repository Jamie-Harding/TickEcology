library(readxl)
library(naniar)

listedfiles <- list.files(path = "data", pattern = "CaLSeN_\\d{4}\\.xlsx$", full.names = TRUE)

waypointspecieslist <- lapply(listedfiles, function(filename){
  
dtfrwp <- read_excel(path = filename, sheet = "IxodesWaypoint")
dtfrwp$Date <- as.character(dtfrwp$Date)
return(dtfrwp)}
)

ixodeswp <- do.call(rbind, waypointspecieslist)

# Data cleaning
# Remove lone decimal points
ixodeswp$Latitude  <- gsub("^\\.$", "", ixodeswp$Latitude)
ixodeswp$Longitude <- gsub("^\\.$", "", ixodeswp$Longitude)

# Remove anything that's not a digit, minus sign, or decimal point, then convert to numeric
ixodeswp$Latitude <- as.numeric(gsub("[^0-9.-]", "", ixodeswp$Latitude))
ixodeswp$Longitude <- as.numeric(gsub("[^0-9.-]", "", ixodeswp$Longitude))

write.csv(ixodeswp, "IxodesWaypoint.csv")
