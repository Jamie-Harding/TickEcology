library(readxl)
library(dplyr)
library(here)

kingstonsites19 <- read_excel(here("Data", "Ottawa and Kingston CaLSeN 2019", "Kingston 2019", "15 Kingston sites.xlsx"))
ottawasites19 <- read_excel(here("Data", "Ottawa and Kingston CaLSeN 2019", "Ottawa 2019", "10 Ottawa-Gatineau sites.xlsx"))

intersect(names(kingstonsites19), names(ottawasites19))

ottawakingstonsites <- bind_rows(ottawasites19, kingstonsites19)

View(ottawakingstonsites)

write.csv(ottawakingstonsites, "Data/IxodesSites_OK2019.csv", row.names = F)
