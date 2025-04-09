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

# Generate the optimized design with 16 profiles
design <- optFederov(data = full, nTrials = 16, approximate = TRUE)

# Extract the selected profiles
selected_profiles <- full[design$rows, ]
selected_profiles$ProfileID <- 1:16
selected_profiles <- selected_profiles[, c("ProfileID", "Brand", "Price", "Battery", "Camera")]

# 1. Check D-efficiency
d_efficiency <- design$D
cat("D-efficiency:", d_efficiency, "\n")

# 2. Check balance (frequency of each level)
library(purrr)
level_counts <- purrr::map(selected_profiles[, 2:5], table)
print("Level Frequencies:")
print(level_counts)

# 3. Check orthogonality (correlation between attributes)
# Convert levels to numeric codes for correlation analysis
design_matrix <- data.frame(
  Brand = as.numeric(factor(selected_profiles$Brand)),
  Price = as.numeric(factor(selected_profiles$Price)),
  Battery = as.numeric(factor(selected_profiles$Battery)),
  Camera = as.numeric(factor(selected_profiles$Camera))
)

# Compute correlation matrix
cor_matrix <- cor(design_matrix)
print("Correlation Matrix:")
print(round(cor_matrix, 3))