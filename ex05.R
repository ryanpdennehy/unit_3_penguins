################################################################################
# 5.1
################################################################################
# Load required packages
library(tidyverse)
library(palmerpenguins)

# 1. Filter only Gentoo penguins
gentoo <- penguins %>%
  filter(species == "Gentoo")

# 2. Build the linear model: bill_depth ~ flipper_length
lm_gentoo_flipper <- lm(bill_depth_mm ~ flipper_length_mm, data = gentoo)

# 3. View model summary (coefficients, p-values, R-squared, etc.)
summary(lm_gentoo_flipper)

# 4. Plot the data and the fitted regression line
ggplot(data = gentoo, aes(x = flipper_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Gentoo: Bill depth vs. Flipper length", 
       x = "Flipper length (mm)", 
       y = "Bill depth (mm)") +
  theme_classic()

# 5. Compare to the earlier model using bill_length_mm as predictor:
lm_gentoo_bill <- lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
summary(lm_gentoo_bill)
# Flipper length seems to be a better predictor of bill depth, as the R2 value
### slightly higher for that model than for the one using bill length as a
### predictor.
