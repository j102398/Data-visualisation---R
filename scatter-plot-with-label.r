# Load the ggplot2 library
library(ggplot2)
library(RSQLite)

# Connect to the SQLITE database
connection <- dbConnect(RSQLite::SQLite(), dbname = "stats.db")

# Define a SQL query to retrieve all columns from the 'shooting_for' table
query <- "SELECT * FROM shooting_for"

# Execute the SQL query and store the result in the 'data' data frame
data <- dbGetQuery(connection, query)

# Retrieve the 'abbreviation' and 'team_name' columns from the 'team_info' table
abbreviation_and_team <- dbGetQuery(connection, "SELECT abbreviation, team_name FROM team_info")

# Retrieve the 'colour_code' and 'team_name' columns from the 'team_info' table
colour_code_and_team <- dbGetQuery(connection, "SELECT colour_code, team_name FROM team_info")

# Assuming 'team_name' is the common column in all data frames

# Merge the 'data' data frame with 'abbreviation_and_team' based on the 'team_name' column
merged_data <- merge(data, abbreviation_and_team, by = "team_name", all.x = TRUE)

# Merge the 'merged_data' data frame with 'colour_code_and_team' based on the 'team_name' column
merged_data <- merge(merged_data, colour_code_and_team, by = "team_name", all.x = TRUE)

# Determine the common limits for both x and y axes
common_limits <- range(c(merged_data$xg, merged_data$goals))

# Create a scatter plot using ggplot2
ggplot(merged_data, aes(x = xg, y = goals, label = abbreviation, color = colour_code)) +
  geom_point(size = 5) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Add y = x line
  scale_color_identity() +  # Specify direct color values
  
  # Set the same limits for both x and y axes
  lims(x = common_limits, y = common_limits) +
  
  # Add title and axis labels
  labs(
    title = "Expected Goals vs Actual Goals",
    x = "Expected Goals (xG)",
    y = "Actual Goals"
  )
