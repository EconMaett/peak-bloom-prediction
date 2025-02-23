# Load necessary libraries
library(ggplot2)
library(gganimate)
library(dplyr)

# Data for Nakatomi Plaza (basic skyscraper shape)
plaza_data <- data.frame(
  x = c(-2, 2, 2, -2), # Rectangle base
  y = c(0, 0, 10, 10) # Height
)

# Data for windows (rows of small rectangles across the skyscraper)
windows_data <- expand.grid(
  x = seq(-1.8, 1.8, by = 0.4), # X positions for window columns
  y = seq(0.5, 9.5, by = 0.5) # Y positions for window rows
) %>%
  mutate(width = 0.3, height = 0.2) # Set dimensions for each window

# Data for festive lights on the plaza
set.seed(123)

lights_data <- data.frame(
  x = runif(50, -2, 2), # Random positions along width
  y = runif(50, 0, 10), # Random positions along height
  color = sample(c("red", "yellow", "blue", "green"), 50, replace = TRUE),
  frame = rep(1:2, each = 25) # Two frames for flashing lights
)

# Data for Hans Gruber's fall
falling_hans <- data.frame(
  x = rep(0, 10), # Hans falls straight down
  y = seq(10, -2, length.out = 10), # Falling from the top to below the plaza
  frame = 1:10 # Frame for animation
)

# Add the sign
sign_data <- data.frame(
  x = 0, # Centered at the building's top
  y = 11, # Positioned slightly above the top of the building
  label = "Die Hard is a Christmas Movie!"
)

# Base plot: Nakatomi Plaza
plaza_plot <- ggplot() +
  # Add the plaza
  geom_polygon(data = plaza_data, aes(x = x, y = y), fill = "gray20", color = "black") +
  # Add windows
  geom_rect(data = windows_data, aes(
    xmin = x - width / 2, xmax = x + width / 2,
    ymin = y - height / 2, ymax = y + height / 2
  ), fill = "lightblue", color = "black", alpha = 0.8) +
  # Add festive lights
  geom_point(data = lights_data, aes(x = x, y = y, color = color, frame = frame), size = 3) +
  # Add Hans Gruber
  geom_point(data = falling_hans, aes(x = x, y = y, frame = frame), size = 5, shape = 21, fill = "white", color = "black") +
  # Add the sign
  geom_text(data = sign_data, aes(x = x, y = y, label = label), size = 6, fontface = "bold", color = "white", hjust = 0.5) +
  # Title and theme
  labs(title = "Nakatomi Plaza - A Die Hard Christmas") +
  scale_color_manual(values = c("red" = "red", "yellow" = "yellow", "blue" = "blue", "green" = "green")) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "white"),
    plot.background = element_rect(fill = "black")
  )

# Add animation for lights and Hans Gruber's fall
animated_plaza <- plaza_plot +
  transition_states(frame, transition_length = 1, state_length = 1) +
  enter_fade() +
  exit_fade()

# Save the animation
anim <- animate(animated_plaza, width = 600, height = 800, nframes = 20, fps = 2)
anim_save("nakatomi_plaza_sign_hans_falling.gif", animation = anim)
