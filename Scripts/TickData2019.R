require(ggplot2)
require(pscl)
require(MASS)
require(boot)
library(GLMsData)
library(DHARMa)
library(readxl)

## Data cleaning
tickraw19 <- read_excel("data/CaLSeN_2019.xlsx", sheet = "Ixodes spp. per waypoint")

names(tickraw19) <- tolower(names(tickraw19))
names(tickraw19) <- gsub(" ", "_", names(tickraw19))
tickraw19$site_name <- gsub(" ", "", tickraw19$site_name)
tickraw19$latitude <- is.numeric(ifelse(tickraw19$latitude == ".", NA, tickraw19$latitude))
tickraw19$longitude <- is.numeric(ifelse(tickraw19$longitude == ".", NA, tickraw19$longitude))

## Setting up that data
tickraw19$tick_count <- tickraw19$larva + tickraw19$nymphs + tickraw19$male + tickraw19$females
tickraw19$tick_present <- ifelse(tickraw19$tick_count > 0, 1, 0)


attach(tickraw19)

## Soil humidity model
tick19shnb <- zeroinfl(tick_count ~ soil_humidity, data = tickraw19, dist = "negbin")
tick19shps <- zeroinfl(tick_count ~ soil_humidity, dist = "poisson", data = tickraw19)
tick19shlg <- glm(tick_present ~ soil_humidity, data = tickraw19, family = binomial())

tick19shps
tick19shnb
tick19shlg


## Leaf litter depth model
tick19llnb <- zeroinfl(tick_count ~ litter_depth, data = tickraw19, dist = "negbin")
tick19llps <- zeroinfl(tick_count ~ litter_depth, dist = "poisson", data = tickraw19)
tick19lllg <- glm(tick_present ~ litter_depth, data = tickraw19, family = binomial())


tick19llps
tick19llnb
tick19lllg

## Canopy cover model
tick19ccnb <- zeroinfl(tick_count ~ canopy_cover, data = tickraw19, dist = "negbin")
tick19ccps <- zeroinfl(tick_count ~ canopy_cover, dist = "poisson", data = tickraw19)
tick19cclg <- glm(tick_present ~ canopy_cover, data = tickraw19, family = binomial())

tick19ccps
tick19ccnb
tick19cclg

## Plotting the data
## Canopy cover logistic regression
ggplot(tickraw19, aes(x = canopy_cover, y = tick_present)) +
  geom_jitter(height = 0.01, width = 0, alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "#45b8ff") +
  labs(title = "Tick Presence vs Canopy Cover Logistic Regression",
       x = "Canopy Cover (%)",
       y = "Tick Presence")

## Simple canopy cover plot
plot(x = tickraw19$tick_count, y = tickraw19$canopy_cover,
     xlab = "Tick Count", ylab = "Canopy Cover (%)", main = "Tick Count vs Canopy Cover")

