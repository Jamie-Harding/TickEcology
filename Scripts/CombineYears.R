library(readxl)
library(naniar)

listedfiles <- list.files(path = "data", pattern = "CaLSeN_\\d{4}\\.xlsx$", full.names = TRUE)

sitespecieslist <- lapply(listedfiles, function(filename){
  
dtfr <- read_excel(path = filename, sheet = "SiteSpecies")
dtfr$Date <- as.character(dtfr$Date)
return(dtfr)}
)

ixodessite <- do.call(rbind, sitespecieslist)

# Data cleaning
# Remove lone decimal points
ixodessite$Latitude  <- gsub("^\\.$", "", ixodessite$Latitude)
ixodessite$Longitude <- gsub("^\\.$", "", ixodessite$Longitude)

# Remove anything that's not a digit, minus sign, or decimal point, then convert to numeric
ixodessite$Latitude <- as.numeric(gsub("[^0-9.-]", "", ixodessite$Latitude))
ixodessite$Longitude <- as.numeric(gsub("[^0-9.-]", "", ixodessite$Longitude))

write.csv(ixodessite, "C:\Users\Queer\Documents\Work\Tick Ecology\TickRemoteSensing")
