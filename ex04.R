################################################################################
# 4.1
################################################################################
# Load necessary libraries
library(tidyverse)
library(palmerpenguins)
library(rstatix)  # for cor_test()
library(ggplot2)

# Filter dataset to include only observations with non-missing bill length & depth
penguin_data <- penguins %>%
  filter(!is.na(bill_length_mm), 
         !is.na(bill_depth_mm))

# Exploratory data analysis
glimpse(penguin_data)
summary(penguin_data)

# Scatter plot to visualize bill length vs. bill depth across species
ggplot(data = penguin_data, aes(x = bill_length_mm, 
                                y = bill_depth_mm, 
                                color = species)) +
  geom_point(alpha = 0.7) +
  labs(title = "Bill Length vs. Bill Depth (All Species)", 
       x = "Bill Length (mm)", 
       y = "Bill Depth (mm)") +
  theme_classic()

# Check normality assumption with Q-Q plots
ggplot(penguin_data) +
  stat_qq(aes(sample = bill_length_mm)) +
  labs(title = "Q-Q Plot for Bill Length")
ggplot(penguin_data) +
  stat_qq(aes(sample = bill_depth_mm)) +
  labs(title = "Q-Q Plot for Bill Depth")

# Pearson correlation between bill length and bill depth for all species combined
cor_all <- cor(x = penguin_data$bill_length_mm, 
               y = penguin_data$bill_depth_mm, 
               use = "complete.obs")
cor_test_all <- cor.test(x = penguin_data$bill_length_mm, 
                         y = penguin_data$bill_depth_mm, 
                         use = "complete.obs")

# Pipe-friendly version from rstatix
cor_test_all_rstatix <- penguin_data %>%
  cor_test(bill_length_mm, bill_depth_mm)

# Display results
cor_all
cor_test_all
cor_test_all_rstatix

# The correlation between bill length and bill depth across all penguin species  
# was negative (r = -0.235, p < 0.001), indicating a weak inverse relationship.
# This may not be true within penguin species

# Filter data for Gentoo penguins only
gentoo_data <- penguin_data %>%
  filter(species == "Gentoo")

# Pearson correlation for Gentoo penguins
cor_gentoo <- cor(x = gentoo_data$bill_length_mm, 
                  y = gentoo_data$bill_depth_mm, 
                  use = "complete.obs")

cor_test_gentoo <- cor.test(x = gentoo_data$bill_length_mm, 
                            y = gentoo_data$bill_depth_mm, 
                            use = "complete.obs")

# Pipe-friendly version from rstatix
cor_test_gentoo_rstatix <- gentoo_data %>%
  cor_test(bill_length_mm, bill_depth_mm)

# Display results
cor_gentoo
cor_test_gentoo
cor_test_gentoo_rstatix

# Prior results contrasts with the strong positive correlation seen in Gentoos  
# alone (r = 0.64). The shift from positive to negative correlation suggests  
# that species differences confound the overall trend. Different species have  
# varying bill proportions, which likely influences the relationship between  
# length and depth. This is an example of Simpson’s Paradox, where trends seen  
# within individual groups disappear or reverse when combined. 
