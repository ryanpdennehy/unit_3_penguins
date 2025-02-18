################################################################################
# 1.1
################################################################################
# Load required packages
library(tidyverse)
library(palmerpenguins)

# (A) Build a dataset with only Chinstrap penguins
chinstrap <- penguins %>%
  filter(species == "Chinstrap")

# (B) Build another dataset with only Chinstrap penguins with flipper length > 200 mm
chinstrap_flipper200 <- penguins %>%
  filter(species == "Chinstrap", 
         flipper_length_mm > 200)

# (C) Examine sex ratios for each subset
summary(chinstrap$sex)
summary(chinstrap_flipper200$sex)

# (D) Interpretation:
# It appears that the vast majority of Chinstrap penguins with a flipper length 
### > 200 mm are male. Very cool!

################################################################################
# 1.2
################################################################################
# (A) Count the number of Chinstrap penguins by sex
chinstrap_count <- penguins %>%
  filter(species == "Chinstrap") %>%
  group_by(sex) %>%
  summarize(count = n())

chinstrap_count

# (B) Count the number of Chinstrap penguins by sex with flipper length > 200 mm
chinstrap_flipper200_count <- penguins %>%
  filter(species == "Chinstrap", 
         flipper_length_mm > 200) %>%
  group_by(sex) %>%
  summarize(count = n())

chinstrap_flipper200_count

# (C) Interpretation:
# Same as before, but streamlined. Also very cool!

################################################################################
# 1.3
################################################################################
# (A) Mean bill length (in inches) of Adelie penguins on Dream or Biscoe islands
### and its standard deviation.
# NOTE: 1 inch is approx. 25.4 mm, so we convert mm to inches by dividing by 25.4
adelie_dream_biscoe <- penguins %>%
  filter(species == "Adelie", 
         island %in% c("Dream", "Biscoe")) %>%
  summarize(
    mean_bill_length_in = mean(bill_length_mm / 25.4, na.rm = TRUE),
    sd_bill_length_in   = sd(bill_length_mm / 25.4, na.rm = TRUE)
  )

adelie_dream_biscoe

# (B) Compare that mean to Adelie penguins on Torgersen Island
adelie_torgersen <- penguins %>%
  filter(species == "Adelie",
         island == "Torgersen") %>%
  summarize(
    mean_bill_length_in = mean(bill_length_mm / 25.4, na.rm = TRUE),
    sd_bill_length_in   = sd(bill_length_mm / 25.4, na.rm = TRUE)
  )

adelie_torgersen

# (C) Interpretation:
# These bill lengths are very comparable across the Dream and Torgersen islands.
### Very cool!