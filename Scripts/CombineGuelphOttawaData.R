library(dplyr)
library(readr)

# Read both CSVs
gu <- read_csv("Data/Aggregated Data/IxodesWaypointGU1924.csv", show_col_types = FALSE)
ok <- read_csv("Data/Aggregated Data/IxodesWaypointOK1924.csv", show_col_types = FALSE)

# Check the column names for consistency (optional debug step)
cat("GU columns:", names(gu), "\n")
cat("OK columns:", names(ok), "\n")

common_cols <- intersect(names(gu), names(ok))
gu <- gu %>% select(all_of(common_cols))
ok <- ok %>% select(all_of(common_cols))

# Combine the two datasets
combined <- bind_rows(gu, ok)

# Save the result as a new CSV
write_csv(combined, "Data/Aggregated Data/IxodesWaypoint1924.csv", na = "")

print("Combined file written to IxodesWaypoint1924.csv\n")
