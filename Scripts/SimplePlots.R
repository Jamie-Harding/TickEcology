library(ggplot2)
library(patchwork)

# Year
ggplot(subset(sitespecies, !is.na(year)), aes(x = year, y = ixodes_count, fill = year)) + geom_boxplot() + labs(title = "Ixodes Scapularis Count by Year", x = "Year", y = "Count of Sampled Ixodes Scapularis")

# Forest Type
ggplot(subset(sitespecies, !is.na(forest_type)), aes(x = forest_type, y = ixodes_count, fill = forest_type)) + stat_summary(fun = mean, geom = "bar") + labs(title = "Mean Ixodes Scapularis Count by Forest Type", x = "Forest Type", y = "Mean Sampled Ixodes Scapularis", fill = "Forest Type")

# Weather
ggplot(subset(sitespecies, !is.na(weather)), aes(x = weather, y = ixodes_count, fill = weather)) + stat_summary(fun = mean, geom = "bar") + labs(title = "Mean Ixodes Scapularis Count by Weather", x = "Weather", y = "Mean Sampled Ixodes Scapularis", fill = "Weather")

# Leaf Litter
ggplot(subset(sitespecies, !is.na(leaf_litter)), aes(x = cut_width(leaf_litter, width = 1), y = ixodes_count, fill = leaf_litter)) + geom_boxplot(varwidth = T, color = "blue", fill = "blue", outlier.color = "red") + labs(title = "Ixodes Scapularis Count by Leaf Litter Depth", x = "Leaf Litter Depth (cm)", y = "Sampled Ixodes Scapularis Count")

# Canopy Cover
ggplot(subset(sitespecies, !is.na(canopy_cover)), aes(x =cut_width(canopy_cover, width = 5, boundary = 0), y = ixodes_count)) + geom_boxplot(varwidth = T, color = "darkgreen", fill = "darkgreen", outlier.color = "red") + labs(title = "Ixodes Scapularis Count by Canopy Cover", x = "Canopy Cover (%)", y = "Sampled Ixodes Scapularis Count") + geom_jitter(width = 0.5, height = 0, alpha = 0.2, colour = "blue")
# Canopy Cover by Forest Type
ggplot(subset(sitespecies, !is.na(canopy_cover)), aes(x =cut_width(canopy_cover, width = 5, boundary = 0), y = ixodes_count)) + geom_boxplot(varwidth = T, color = "darkgreen", fill = "darkgreen", outlier.color = "red") + labs(title = "Ixodes Scapularis Count by Canopy Cover", x = "Canopy Cover (%)", y = "Sampled Ixodes Scapularis Count") + geom_jitter(width = 0.5, height = 0, alpha = 0.2, colour = "blue")+facet_wrap(~forest_type, ncol = 2)

# Soil Humidity
ggplot(subset(sitespecies, !is.na(soil_humidity)), aes(x =cut_width(soil_humidity, width = 5, boundary = 0), y = ixodes_count)) + geom_boxplot(varwidth = T, color = "brown", fill = "brown", outlier.color = "red") + labs(title = "Ixodes Scapularis Count by Soil Humidity", x = "Soil Humidity (%)", y = "Sampled Ixodes Scapularis Count") + geom_jitter(width = 0.5, height = 0, alpha = 0.2, colour = "blue")

# Temperature
ggplot(subset(sitespecies, !is.na(temperature)), aes(x =cut_width(temperature, width = 2, boundary = 0), y = ixodes_count)) + geom_boxplot(varwidth = T, color = "darkgreen", fill = "darkgreen", outlier.color = "red") + labs(title = "Ixodes Scapularis Count by Temperature", x = "Temperature (C)", y = "Sampled Ixodes Scapularis Count") + geom_jitter(width = 0.5, height = 0, alpha = 0.2, colour = "blue")

