library("lintr")
library("dplyr")

# Import the data
shootings <- read.csv(file = "data/shootings-2018.csv",
                      stringsAsFactors = FALSE)
is.data.frame(shootings)

# Create some new columns we'll use later
shootings <- shootings %>%
  mutate(date = as.Date(date, "%B %d, %Y"),
         month_number = format(date, "%m"),
         month_text = format(date, "%B"),
         num_casualties = num_injured + num_killed)


## Define reusable functions

# This function returns the top n groups by total casualties
# when grouped by the group_by_col
get_top_n_casualties <- function(group_by_col, n, df) {
  df %>%
    group_by(!!as.name(group_by_col)) %>%
    summarize(tot_casualties = sum(num_casualties)) %>%
    arrange(desc(tot_casualties)) %>%
    head(n)
}

# This function returns the top group name by casualty
get_highest_casualty_group <- function(group_by_col, df) {
  get_top_n_casualties(group_by_col, 1, df) %>%
    pull(!!as.name(group_by_col))
}


## Analysis

# Some basic stats
column_names <- colnames(shootings)
num_shootings <- nrow(shootings)
tot_killed <- sum(shootings$num_killed)

# Below we analyze highest impact by different variables
# Where highest "impact" means most casuaties

# Get most impacted city
most_impacted_city <- get_highest_casualty_group("city", shootings)

# Get most impacted state
most_impacted_state <- get_highest_casualty_group("state", shootings)

# Get most impacted month
most_impacted_month <- get_highest_casualty_group("month_text", shootings)

# Get top 5 cities with highest casualties
# Also calculate the difference in casualties between the city
# and the next highest casualty city, and by what factor it's bigger
top_5_cities <- get_top_n_casualties("city", 5, shootings) %>%
  mutate(diff = tot_casualties - lead(tot_casualties, 1)) %>%
  mutate(factor = round(tot_casualties / lead(tot_casualties, 1), 2))

# Extract just the difference
top_diff <- top_5_cities %>%
  slice(1) %>%
  pull(diff)

# Extract just the factor
top_factor <- top_5_cities %>%
  slice(1) %>%
  pull(factor)


# Get data for just the Seattle (Skyway) / Renton incident on April 29
selected_incident <- shootings %>% slice(266)
