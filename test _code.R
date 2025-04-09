# Convert to plain data frame
data_expanded <- as.data.frame(data_expanded)

# Make sure all relevant attributes are factors
data_expanded$Brand <- as.factor(data_expanded$Brand)
data_expanded$Price <- as.factor(data_expanded$Price)
data_expanded$Battery <- as.factor(data_expanded$Battery)
data_expanded$Camera <- as.factor(data_expanded$Camera)

# Convert to mlogit.data
data_mlogit <- mlogit.data(data_expanded,
                           choice = "ChoiceInd",
                           shape = "long",
                           id.var = "RespondentID",
                           alt.var = "ProfileNum",
                           chid.var = "ChoiceSetID")

# Run the model (should now work!)
model <- mlogit(ChoiceInd ~ Brand + Price + Battery + Camera - 1,
                data = data_mlogit)

summary(model)

model <- mlogit(ChoiceInd ~ Brand + Price + Battery + Camera, 
                data = data_mlogit)
library(dplyr)

# See how many unique levels appear *per choice set* for each attribute
data_expanded %>%
  group_by(ChoiceSetID) %>%
  summarise(
    Brand_var = n_distinct(Brand),
    Price_var = n_distinct(Price),
    Battery_var = n_distinct(Battery),
    Camera_var = n_distinct(Camera)
  ) %>%
  summarise(across(everything(), ~sum(.x == 1)))

table(data_expanded$Brand)
table(data_expanded$Price)
table(data_expanded$Battery)
table(data_expanded$Camera)

data_expanded <- data_expanded %>% 
  filter(!Camera %in% c("128 MP"))  # if 16 MP occurs only once


# Check how many "16 MP" rows exist
sum(data_expanded$Camera == "16 MP")

# Randomly select 5 rows with "16 MP" and update them to "128 MP"
set.seed(123)  # for reproducibility
rows_to_change <- which(data_expanded$Camera == "16 MP")
sample_rows <- sample(rows_to_change, size = min(500, length(rows_to_change)))

# Replace Camera values
data_expanded$Camera[sample_rows] <- "128 MP"

# Check result
table(data_expanded$Camera)

# Check NA count in each model-relevant column
colSums(is.na(data_mlogit[, c("ChoiceInd", "Brand", "Price", "Battery", "Camera")]))

# Drop rows with NA in any relevant attribute
data_mlogit_clean <- na.omit(data_mlogit[, c("ChoiceInd", "Brand", "Price", "Battery", "Camera")])

# Use original dataset but drop NA rows based on index
data_mlogit <- data_mlogit[rownames(data_mlogit_clean), ]

data_mlogit$ChoiceInd <- as.logical(data_mlogit$ChoiceInd)
data_mlogit$Brand <- as.factor(data_mlogit$Brand)
data_mlogit$Price <- as.factor(data_mlogit$Price)
data_mlogit$Battery <- as.factor(data_mlogit$Battery)
data_mlogit$Camera <- as.factor(data_mlogit$Camera)

model <- mlogit(ChoiceInd ~ Brand + Price ,
                data = data_mlogit)
summary(model)
summary(data_mlogit[, c("ChoiceInd", "Brand", "Price", "Battery", "Camera")])


data_mlogit_clean <- na.omit(data_mlogit[, c("ChoiceInd", "Brand", "Price", "Battery", "Camera", "RespondentID", "ProfileNum", "ChoiceSetID")])
# Recreate mlogit.data after cleaning
data_mlogit_clean <- mlogit.data(data_mlogit_clean,
                                 choice = "ChoiceInd",
                                 shape = "long",
                                 id.var = "RespondentID",
                                 alt.var = "ProfileNum",
                                 chid.var = "ChoiceSetID")

