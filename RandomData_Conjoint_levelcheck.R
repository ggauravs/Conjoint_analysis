# Load necessary libraries
library(dplyr)
library(AlgDesign)

# Set seed for reproducibility
set.seed(123)

# Define the 16 profiles
full <- expand.grid(
  Brand = c("Apple", "Samsung", "Google", "Xiaomi"),
  Price = c("$400", "$700", "$1000"),
  Battery = c("15 hours", "20 hours", "25 hours"),
  Camera = c("16 MP", "64 MP", "128 MP")
)
design <- optFederov(data = full, nTrials = 16, approximate = TRUE)
selected_profiles <- full[design$rows, ]
selected_profiles$ProfileID <- 1:16

# Number of respondents and tasks
n_respondents <- 100
n_tasks <- 6

# Function to generate a choice task (4 random profiles)
generate_task <- function(profiles) {
  sample_n(profiles, 4)$ProfileID
}

# Generate initial data structure
data <- expand.grid(RespondentID = 1:n_respondents, TaskID = 1:n_tasks) %>%
  group_by(RespondentID, TaskID) %>%
  mutate(
    ProfilesShown = list(generate_task(selected_profiles))
  ) %>%
  ungroup() %>%
  mutate(
    Profile1 = sapply(ProfilesShown, `[`, 1),
    Profile2 = sapply(ProfilesShown, `[`, 2),
    Profile3 = sapply(ProfilesShown, `[`, 3),
    Profile4 = sapply(ProfilesShown, `[`, 4),
    Choice = NA_integer_  # Initialize Choice as NA
  ) %>%
  select(-ProfilesShown)

# Add demographics
data <- data %>%
  group_by(RespondentID) %>%
  mutate(
    AgeGroup = sample(c("18-25", "26-35", "36-45", "46+"), 1, prob = c(0.25, 0.3, 0.25, 0.2)),
    IncomeLevel = sample(c("Under $30K", "$30K-$60K", "$60K-$100K", "Over $100K"), 1, prob = c(0.2, 0.3, 0.3, 0.2)),
    Gender = sample(c("Male", "Female", "Other/Prefer not to say"), 1, prob = c(0.45, 0.45, 0.1))
  ) %>%
  ungroup()

# Corrected function to assign choices with coverage
assign_choices_with_coverage <- function(df) {
  # Total number of choices to assign (600 tasks)
  n_choices <- nrow(df)

  # Ensure each level is chosen at least once
  all_levels <- expand.grid(
    Brand = c("Apple", "Samsung", "Google", "Xiaomi"),
    Price = c("$400", "$700", "$1000"),
    Battery = c("15 hours", "20 hours", "25 hours"),
    Camera = c("16 MP", "64 MP", "128 MP")
  )

  # Merge with profiles to get ProfileIDs for each level combination
  level_coverage <- all_levels %>%
    left_join(selected_profiles, by = c("Brand", "Price", "Battery", "Camera")) %>%
    filter(!is.na(ProfileID))

  # Randomly select one ProfileID for each level to ensure coverage
  initial_choices <- level_coverage %>%
    group_by(Brand) %>%
    sample_n(1) %>%
    ungroup() %>%
    bind_rows(
      level_coverage %>%
        group_by(Price) %>%
        sample_n(1) %>%
        ungroup(),
      level_coverage %>%
        group_by(Battery) %>%
        sample_n(1) %>%
        ungroup(),
      level_coverage %>%
        group_by(Camera) %>%
        sample_n(1) %>%
        ungroup()
    ) %>%
    distinct(ProfileID)  # Ensure no duplicates

  # Assign these as initial choices to random rows
  initial_choice_df <- data.frame(
    RespondentID = sample(df$RespondentID, nrow(initial_choices), replace = FALSE),
    TaskID = sample(df$TaskID, nrow(initial_choices), replace = FALSE),
    Choice = initial_choices$ProfileID
  )

  # Update df with initial choices
  df <- df %>%
    left_join(initial_choice_df, by = c("RespondentID", "TaskID")) %>%
    mutate(Choice = coalesce(Choice.y, Choice.x)) %>%
    select(-Choice.x, -Choice.y)

  # Assign remaining choices randomly
  remaining_rows <- df %>%
    filter(is.na(Choice))
  n_remaining <- nrow(remaining_rows)
  if (n_remaining > 0) {
    remaining_choices <- sample(c(0, 1:16), n_remaining, replace = TRUE, prob = c(0.1, rep(0.9/16, 16)))
    remaining_rows$Choice <- remaining_choices
    df <- df %>%
      filter(!is.na(Choice)) %>%
      bind_rows(remaining_rows) %>%
      arrange(RespondentID, TaskID)
  }

  return(df)
}
