################################################################################
# 2.1
################################################################################
# Load necessary libraries
library(palmerpenguins)
library(tidyverse)

# Build a scatter plot of bill depth vs. bill length for Adelie penguins.
# Map the point colors to the island that the Adelie penguin was observed on.
# Add axes labels and a main title.
ggplot(data = penguins %>% filter(species == "Adelie")) + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = island)) +
  xlab("Bill Length (mm)") +
  ylab("Bill Depth (mm)") +
  ggtitle("Bill Depth vs. Bill Length for Adelie Penguins") +
  labs(color = "Island") +  
  theme_bw()

################################################################################
# 2.2
################################################################################
# Build another scatter plot of bill depth vs. bill length for all three penguin
### species.
# Map the point colors to the penguin's sex.
# Use facet_wrap() to plot each species in a separate panel.
# Try plotting with the parameter scales = "free".
# Choose a different pre-packaged theme (theme_bw() or theme_classic()).
# Save the plot.
plot_penguin_bills <- ggplot(data = penguins) + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = sex)) +
  facet_wrap(~species, scales = "free") +
  xlab("Bill Length (mm)") +
  ylab("Bill Depth (mm)") +
  ggtitle("Bill Depth vs. Bill Length Across Penguin Species") +
  labs(color = "Island") +
  theme_bw()

# Without free scales
plot_penguin_bills_nfs <- ggplot(data = penguins) + 
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = sex)) +
  facet_wrap(~species) +
  xlab("Bill Length (mm)") +
  ylab("Bill Depth (mm)") +
  ggtitle("Bill Depth vs. Bill Length Across Penguin Species") +
  labs(color = "Island") +
  theme_bw()

# Print plots
print(plot_penguin_bills)
print(plot_penguin_bills_nfs)

# Save the free scale plot
ggsave(filename = "figures/penguin_bill_scatter.png", plot = plot_penguin_bills, 
       device = "png", width = 8, height = 4, units = "in", dpi = 300)
