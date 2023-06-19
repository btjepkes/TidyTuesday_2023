
library(tidytuesdayR)
library(tidyverse)

# See what data are available for this week
tidytuesdayR::tt_available()

# Load in Project FeederWatch data
rawdata <- tidytuesdayR::tt_load(x = "2023-01-10")

obsdata <- rawdata$PFW_2021_public

sitedata <- rawdata$PFW_count_site_data_public_2021

# Load in species names to work from species codes
species_codes <- read.csv("https://feederwatch.org/wp-content/uploads/2022/08/PFW-species-translation-table.csv") %>%
  janitor::clean_names() %>%
  rename(species_code = i_species_code)

# SIDEBAR
library(ggplot2)

# Create some sample data
df <- data.frame(
  group = c("A", "B", "C", "D"),
  value1 = c(10, 20, 30, 40),
  value2 = c(-10, -20, -30, -40)
)

# Plot the data using ggplot
ggplot(df, aes(x = group, y = value1)) +
  geom_bar(stat = "identity", aes(fill = "Positive"), width = 0.5) +
  geom_bar(data = df, stat = "identity", aes(x = group, y = value2, fill = "Negative"), width = 0.5) +

  scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red")) +
  theme_minimal()


# Create new column with the absolute value of value2 column
df$abs_value2 <- abs(df$value2)

# Plot the data using ggplot
ggplot(df, aes(x = group, y = value1)) +
  geom_bar(stat = "identity", aes(fill = "Positive"), width = 0.5) +
  geom_bar(data = df, stat = "identity", aes(x = group, y = abs_value2, fill = "Negative"), width = 0.5, position = "stack") +
  scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red")) +
  theme_minimal()
