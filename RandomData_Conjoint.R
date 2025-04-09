# Load necessary libraries
library(dplyr)

# Set seed for reproducibility
set.seed(101)

# Define the 16 profiles
profiles <- data.frame(
  ProfileID = 1:16,
  Brand = c("Apple", "Apple", "Apple", "Apple", "Samsung", "Samsung", "Samsung", "Samsung",
            "Google", "Google", "Google", "Google", "Xiaomi", "Xiaomi", "Xiaomi", "Xiaomi"),
  Price = c(400, 700, 1000, 700, 400, 700, 1000, 400, 400, 700, 1000, 1000, 400, 700, 1000, 400),
  BatteryLife = c(15, 20, 25, 15, 20, 25, 15, 25, 15, 20, 25, 20, 25, 15, 20, 15),
  CameraQuality = c(16, 64, 128, 128, 128, 16, 64, 64, 64, 128, 16, 64, 128, 64, 16, 16)
)

# Number of respondents and tasks
n_respondents <- 100
n_tasks <- 6

# Function to generate a choice task (4 random profiles)
generate_task <- function(profiles) {
  sample_profiles <- sample_n(profiles, 4)
  return(sample_profiles$ProfileID)
}

# Simulate choices (random for now)
simulate_choice <- function(task_profiles) {
  choice <- sample(c(task_profiles, 0), 1)  # 0 represents "None"
  return(choice)
}

# Generate data
data <- expand.grid(RespondentID = 1:n_respondents, TaskID = 1:n_tasks) %>%
  group_by(RespondentID, TaskID) %>%
  mutate(
    ProfilesShown = list(generate_task(profiles)),
    Choice = simulate_choice(ProfilesShown[[1]])
  ) %>%
  ungroup()

# Add demographics
data <- data %>%
  group_by(RespondentID) %>%
  mutate(
    AgeGroup = sample(c("18-25", "26-35", "36-45", "46+"), 1, prob = c(0.25, 0.3, 0.25, 0.2)),
    IncomeLevel = sample(c("Under $30K", "$30K-$60K", "$60K-$100K", "Over $100K"), 1, prob = c(0.2, 0.3, 0.3, 0.2)),
    Gender = sample(c("Male", "Female", "Other/Prefer not to say"), 1, prob = c(0.45, 0.45, 0.1))
  ) %>%
  ungroup()

# Expand ProfilesShown into separate columns
data <- data %>%
  mutate(
    Profile1 = sapply(ProfilesShown, `[`, 1),
    Profile2 = sapply(ProfilesShown, `[`, 2),
    Profile3 = sapply(ProfilesShown, `[`, 3),
    Profile4 = sapply(ProfilesShown, `[`, 4)
  ) %>%
  select(-ProfilesShown)

# View the first few rows
head(data)
