getwd()
setwd("C:/Users/ggaur/Documents/Analysis/Conjoint analysis")

library(AlgDesign)
set.seed(123)
full <- expand.grid(Brand = 1:4, Price = 1:3, Battery = 1:3, Camera = 1:3)
design <- optFederov(data = full, nTrials = 16, approximate = TRUE)
design$rows  # Returns 16 profile indices

# Load necessary library
library(AlgDesign)

# Set seed for reproducibility
set.seed(123)

# Define the full factorial design with actual levels
full <- expand.grid(
  Brand = c("Apple", "Samsung", "Google", "Xiaomi"),
  Price = c("$400", "$700", "$1000"),
  Battery = c("15 hours", "20 hours", "25 hours"),
  Camera = c("16 MP", "64 MP", "128 MP")
)

# Generate an optimized fractional factorial design with 16 profiles
design <- optFederov(data = full, nTrials = 16, approximate = F)

# Extract the 16 selected profiles
selected_profiles <- full[design$rows, ]

# Add a ProfileID column for clarity
selected_profiles$ProfileID <- 1:16

# Reorder columns to put ProfileID first
selected_profiles <- selected_profiles[, c("ProfileID", "Brand", "Price", "Battery", "Camera")]

# Display the resulting profiles
print(selected_profiles)

purrr::map(selected_profiles,table)
purrr::map(full,table)
