library(readxl)
library(lme4)
library(pscl)
library(MASS)
library(boot)
library(GLMsData)
library(DHARMa)
library(naniar)
library(ggeffects)
library(ggplot2)

# list all Excel files
calsenfiles <- list.files(path = "data",
                          pattern = "CaLSeN_\\d{4}\\.xlsx$",
                          full.names = TRUE
)

# Read each SiteSpecies sheet into a data frame
sitespecieslist <- lapply(calsenfiles, function(filename){
  df <- read_excel(path = filename,
                   sheet = "SiteSpecies")

  return(df)
})

# Combine all dataframes into one
sitespecies <- do.call(rbind, sitespecieslist)
sitespecies 


# Data cleaning
names(sitespecies) <- tolower(names(sitespecies))
names(sitespecies) <- gsub(" ", "_", names(sitespecies))
sitespecies$region <- gsub(" ", "", sitespecies$region)
sitespecies <- replace_with_na_all(sitespecies, condition = ~ .x == "N/A")
sitespecies$forest_type <- gsub("Decidious","Deciduous", sitespecies$forest_type)
sitespecies$weather <- gsub("Partly cloudy", "Partly Cloudy", sitespecies$weather)
sitespecies$ixodes_present <- ifelse(sitespecies$ixodes_count > 0, 1, 0)
# Replace non-standard weather that includes partly cloudy with partly cloudy
sitespecies$weather <- gsub("Sunny/partly cloudy|Sunny, Partly Cloudy", "Partly Cloudy", sitespecies$weather)
# Remove C from temperatures
sitespecies$temperature <- gsub(" C", "", sitespecies$temperature)
# Average temperatures in ranges
sitespecies$temperature <- sapply(sitespecies$temperature, function(x) {
  if (is.na(x)) {
    return(NA)
  } else if (grepl("-", x)) {
    parts <- as.numeric(unlist(strsplit(x, "-")))
    return(mean(parts))
  } else {
    return(as.numeric(x))
  }
})

# Convert variables into factors and numeric
sitespecies$year <- as.factor(sitespecies$year) 
sitespecies$site_id <- as.factor(sitespecies$site_id)
sitespecies$temperature <- as.numeric(sitespecies$temperature)
sitespecies$forest_type <- as.factor(sitespecies$forest_type)
sitespecies$weather <- as.factor(sitespecies$weather)
sitespecies$canopy_cover <- as.numeric(sitespecies$canopy_cover)
sitespecies$leaf_litter <- as.numeric(sitespecies$leaf_litter)
sitespecies$soil_humidity <- as.numeric(sitespecies$soil_humidity)
sitespecies$ixodes_count <- as.numeric(sitespecies$ixodes_count)

# Create mixed model
# Poisson mixed model
calsenpoisson <- glmer(ixodes_count ~ year + temperature + forest_type + weather + canopy_cover + leaf_litter + soil_humidity + (1|site_id), family = poisson, data = sitespecies)
# Logistic mixed model
calsenlogistic <- glmer(ixodes_present ~ year + temperature + forest_type + weather + canopy_cover + leaf_litter + soil_humidity + (1|site_id), family = binomial(link = "logit"), data = sitespecies)
# Negative binomial mixed model
calsennb <- glmer.nb(ixodes_count ~ year + temperature + forest_type + weather + canopy_cover + leaf_litter + soil_humidity + (1|site_id), data = sitespecies)

## Plotting the data
# Plot logistic model for year
yearlog <- ggpredict(calsenlogistic, terms="year")
plot(yearlog) + ggtitle("Logistic Regression - Tick Presence by Year") + ylab("Predicted Probability of Tick Presence") + xlab("Year")
# Negative binomial for year
yearnb <- ggpredict(calsennb, terms="year")
plot(yearnb) + ggtitle("Negative Binomial - Tick Count by Year") + ylab("Predicted Tick Abundance") + xlab("Year")

# Plot logistic model for leaf litter
leaflog <- ggpredict(calsenlogistic, terms="leaf_litter")
plot(leaflog) + ggtitle("Logistic Regression - Tick Presence by Leaf Litter Depth") + ylab("Predicted Probability of Tick Presence") + xlab("Leaf Litter Depth (cm)")
# Negative binomial for leaf litter
leafnb <- ggpredict(calsennb, terms="leaf_litter")
plot(leafnb) + ggtitle("Negative Binomial - Tick Count by Leaf Litter Depth") + ylab("Predicted Tick Abundance") + xlab("Leaf Litter Depth (cm)")

# Plot logistic model for temperature
templog <- ggpredict(calsenlogistic, terms="temperature")
plot(templog) + ggtitle("Logistic Regression - Tick Presence by Temperature") + ylab("Predicted Probability of Tick Presence") + xlab("Temperature (C)")
# Negative binomial for temperature
tempnb <- ggpredict(calsennb, terms="temperature")
plot(tempnb) + ggtitle("Negative Binomial - Tick Count by Temperature") + ylab("Predicted Tick Abundance") + xlab("Temperature (C)")

# Simple temperature plot
plot(sitespecies$temperature, sitespecies$ixodes_count, main = "Plot of Temperature vs Tick Count", xlab = "Temperature (C)", ylab = "Tick Count")

# Simple leaf litter plot
plot(sitespecies$leaf_litter, sitespecies$ixodes_count, main = "Plot of Leaf Litter vs Tick Count", xlab = "Leaf Litter (C)", ylab = "Tick Count")
