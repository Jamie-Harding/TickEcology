library(readxl)
library(naniar)

listedfiles <- list.files(path = "data", pattern = "CaLSeN_MK23_\\d{4}\\.xlsx$", full.names = TRUE)

waypointspecieslist <- lapply(listedfiles, function(filename){
  
dtfrwp <- read_excel(path = filename, sheet = "IxodesWaypoint")
names(dtfrwp) <- tolower(names(dtfrwp))
names(dtfrwp) <- gsub(" ", "_", names(dtfrwp))
dtfrwp$date <- as.character(dtfrwp$date)
return(dtfrwp)}
)

ixodeswp <- do.call(rbind, waypointspecieslist)

# Data cleaning for
# Remove lone decimal points
ixodeswp$latitude  <- gsub("^\\.$", "", ixodeswp$latitude)
ixodeswp$longitude <- gsub("^\\.$", "", ixodeswp$longitude)

# Remove anything that's not a digit, minus sign, or decimal point, then convert to numeric
ixodeswp$latitude <- as.numeric(gsub("[^0-9.-]", "", ixodeswp$latitude))
ixodeswp$longitude <- as.numeric(gsub("[^0-9.-]", "", ixodeswp$longitude))

write.csv(ixodeswp, "IxodesWaypointMK23.csv")
