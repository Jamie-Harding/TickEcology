## Plot zero inflated data as a Poisson
tickraw19zip <- tickraw19[order(tickraw19$canopy_cover), ]

ggplot(tickraw19, aes(x = canopy_cover, y = tick_count)) +
  geom_jitter(height = 0.01, width = 0, alpha = 0.3) +
  geom_line(data = tickraw19zip, aes(y = predict(tick19ccps, newdata = tickraw19zip, type = "response")),
            color = "#45b8ff", size = 1) +
  labs(title = "Tick Count vs Canopy Cover Poisson",
       x = "Canopy Cover (%)",
       y = "Tick Count")