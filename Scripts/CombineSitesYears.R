library(readxl)
library(naniar)
library(here)

listedfiles <- list.files(path = "data", pattern = "CaLSeN_\\d{4}\\.xlsx$", full.names = TRUE)

sitespecieslist <- lapply(listedfiles, function(filename){
  dtfrs <- read_excel(path = filename, sheet = "SiteSpecies")
  
  names(dtfrs) <- tolower(names(dtfrs))
  names(dtfrs) <- gsub(" ", "_", names(dtfrs))
# Fixes date
if(inherits(dtfrs$date, "POSIXct")){
  dtfrs$date <- as.Date(dtfrs$date)
} else if(is.character(dtfrs$date)) {
  dtfrs$date <- as.Date(as.numeric(dtfrs$date), origin="1899-12-30")
} else {
  dtfrs$date <- ymd(dtfrs$date)
}

# Fixes start time
if(inherits(dtfrs$start_time, "POSIXct")){
  dtfrs$start_time <- format(dtfrs$start_time, "%H:%M")
} else {
  numeric_start <- suppressWarnings(as.numeric(dtfrs$start_time) * 86400)
  posixct_start <- suppressWarnings(as.POSIXct(numeric_start, origin = "1970-01-01", tz = "GMT"))
  dtfrs$start_time <- format(posixct_start, "%H:%M")
}

# Fixes finish time
if(inherits(dtfrs$finish_time, "POSIXct")){
  dtfrs$finish_time <- format(dtfrs$finish_time, "%H:%M")
} else {
  numeric_finish <- suppressWarnings(as.numeric(dtfrs$finish_time) * 86400)
  posixct_finish <- suppressWarnings(as.POSIXct(numeric_finish, origin = "1970-01-01", tz = "GMT"))
  dtfrs$finish_time <- format(posixct_finish, "%H:%M")
}
return(dtfrs)}
)

ixodessite <- do.call(rbind, sitespecieslist)

# Data cleaning
# Remove lone decimal points
ixodessite$startlatitude  <- gsub("^\\.$", "", ixodessite$startlatitude)
ixodessite$startlongitude <- gsub("^\\.$", "", ixodessite$startlongitude)
ixodessite$finishlatitude  <- gsub("^\\.$", "", ixodessite$finishlatitude)
ixodessite$finishlongitude <- gsub("^\\.$", "", ixodessite$finishlongitude)

# Remove anything that's not a digit, minus sign, or decimal point, then convert to numeric
ixodessite$startlatitude <- as.numeric(gsub("[^0-9.-]", "", ixodessite$startlatitude))
ixodessite$startlongitude <- as.numeric(gsub("[^0-9.-]", "", ixodessite$startlongitude))
ixodessite$finishlatitude <- as.numeric(gsub("[^0-9.-]", "", ixodessite$finishlatitude))
ixodessite$finishlongitude <- as.numeric(gsub("[^0-9.-]", "", ixodessite$finishlongitude))

View(ixodessite)

write.csv(ixodessite, "Data/Aggregated Data/IxodesSiteGU1924.csv")

