library("lintr")
library("dplyr")

# Import the data
shootings <- read.csv(file = "data/shootings-2018.csv",
                      stringsAsFactors = FALSE)
is.data.frame(shootings)

# Some basic stats
column_names <- colnames(shootings)
num_shootings <- nrow(shootings)
tot_killed <- sum(shootings$num_killed)

# Create some new columns we'll use later
shootings <- shootings %>%
  mutate(date = as.Date(date, "%B %d, %Y"),
         month_number = format(date, "%m"),
         month_text = format(date, "%B"),
         num_casualties = num_injured + num_killed)

# Define reusable functions
get_top_n_casualties <- function(group_by_col, n, df) {
  df %>%
    group_by(!!as.name(group_by_col)) %>%
    summarize(tot_casualties = sum(num_casualties)) %>%
    arrange(desc(tot_casualties)) %>%
    head(n)
}

get_highest_casualty_group <- function(group_by_col, df) {
  get_top_n_casualties(group_by_col, 1, df) %>%
    pull(!!as.name(group_by_col))
}

# Below we analyze highest impact by different variables
# Where highest "impact" means most casuaties

# Get most impacted city
most_impacted_city <- get_highest_casualty_group("city", shootings)

# Get most impacted state
most_impacted_state <- get_highest_casualty_group("state", shootings)

# Get most impacted month
# We need to create the month variable first
# by extracting the short month name as the first 3 chars of date
most_impacted_month <- get_highest_casualty_group("month_text", shootings)


top_5_cities <- get_top_n_casualties("city", 5, shootings) %>%
  mutate(diff = tot_casualties - lead(tot_casualties, 1)) %>%
  mutate(factor = round(tot_casualties / lead(tot_casualties, 1), 2))

top_diff <- top_5_cities %>%
  slice(1) %>%
  pull(diff)

top_factor <- top_5_cities %>%
  slice(1) %>%
  pull(factor)


# Seattle (Skyway) / Renton incident on April 29
selected_incident <- shootings %>% slice(266)
