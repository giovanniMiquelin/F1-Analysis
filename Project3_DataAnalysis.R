# SCRIPT FOR PROJECT DATA ANALYSIS 3 - F1 DATA

getwd()

f1_data = read.csv("f1_master_data.csv")
View(f1_data)



# Install the required package if it is not already installed
# install.packages("dplyr")

# Load the package
library(dplyr)

# 1. Load the dataset
f1_data <- read.csv("f1_master_data.csv")

# 2. Filter for the Classic Era (1950 - 1980) and categorize the finish outcomes
classic_era_summary <- f1_data %>%
  filter(year >= 1950 & year <= 1980) %>%
  mutate(
    Finish_Outcome = case_when(
      race_status == "Finished" ~ "Finished on the Lead Lap",
      race_status %in% c("+1 Lap", "+2 Laps", "+3 Laps") ~ "Finished 1 to 3 Laps Down",
      grepl("^\\+([4-9]|[1-9][0-9]+) Laps?", race_status) ~ "Finished 4+ Laps Down",
      race_status %in% c("Did not qualify", "Excluded", "Not pre-qualified") ~ "Failed to Qualify (DNQ) / Excluded",
      TRUE ~ "Other (Mechanical DNF, Crash, etc.)"
    )
  ) %>%
  # 3. Calculate counts and percentages for the table
  group_by(Finish_Outcome) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = paste0(round((Count / sum(Count)) * 100, 1), "%")) %>%
  arrange(desc(Count))

# 4. Print the final table
print(classic_era_summary)



# Install the ggplot2 package if you don't have it already
# install.packages("ggplot2")

# Load the package
library(ggplot2)

# 1. Create the dataset manually based on our previous summary
f1_teams_data <- data.frame(
  Era = factor(
    c("Classic (1950–1980)", "Aero (1981–2011)", "Hybrid (2012–2024)"),
    levels = c("Classic (1950–1980)", "Aero (1981–2011)", "Hybrid (2012–2024)")
  ),
  Total_Teams = c(161, 60, 21)
)

# 2. Create a simple, clean bar graph
teams_plot <- ggplot(f1_teams_data, aes(x = Era, y = Total_Teams, fill = Era)) +
  geom_col(width = 0.6) + 
  geom_text(aes(label = Total_Teams), vjust = -0.5, size = 5, fontface = "bold") + # Adds numbers on top of bars
  theme_minimal() + # Keeps the background clean and simple
  labs(
    title = "Total Unique Constructors (Teams) by F1 Era",
    x = "F1 Era",
    y = "Total Teams"
  ) +
  theme(
    legend.position = "none", # Hides the legend since the x-axis already labels the eras
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text = element_text(size = 11)
  )

# 3. Display the graph
print(teams_plot)



# Install the ggplot2 package if you don't have it already
# install.packages("ggplot2")

library(ggplot2)

# 1. Create the dataset manually
grid_data <- data.frame(
  Grid_Position = factor(
    c("1st (Pole)", "2nd", "3rd", "4th", "5th", "6th to 10th"),
    levels = c("1st (Pole)", "2nd", "3rd", "4th", "5th", "6th to 10th") # Ensures correct ordering on the x-axis
  ),
  Total_Wins = c(481, 268, 137, 68, 49, 97),
  Win_Percentage = c(42.6, 23.8, 12.1, 6.0, 4.3, 8.6)
)

# 2. Create a clean, visually appealing bar graph plotting Win Percentage
grid_plot <- ggplot(grid_data, aes(x = Grid_Position, y = Win_Percentage, fill = Grid_Position)) +
  geom_col(width = 0.7) + 
  
  # Add percentage labels directly on top of the bars
  geom_text(aes(label = paste0(Win_Percentage, "%")), 
            vjust = -0.8, size = 5, fontface = "bold") +
  
  # Use a built-in color palette that differentiates the positions
  scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
  
  theme_minimal() +
  labs(
    title = "F1 Win Probability by Starting Grid Position",
    subtitle = "Based on historical race data",
    x = "Starting Grid Position",
    y = "All-Time Win Percentage (%)"
  ) +
  theme(
    legend.position = "none", # Hide the legend since the x-axis is already labeled
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold")
  ) +
  # Expand the y-axis slightly so the top label (42.6%) isn't cut off
  coord_cartesian(ylim = c(0, 50))

# 3. Display the graph
print(grid_plot)







# Install ggplot2 if you don't have it
# install.packages("ggplot2")

library(ggplot2)

# 1. Create the dataset for Pole Sitters who LOST
pole_loss_data <- data.frame(
  Outcome = factor(
    c("Mechanical DNF / Breakdowns", "Finished but Lost Pace", "Accidents / Crashes"),
    levels = c("Mechanical DNF / Breakdowns", "Finished but Lost Pace", "Accidents / Crashes") # Order by highest percentage
  ),
  Percentage = c(47.8, 44.2, 8.0)
)

# 2. Define the custom dark theme (consistent with previous charts)
dark_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major = element_line(color = "#333333", size = 0.5),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#b3b3b3", size = 12),
    axis.title = element_text(color = "white", size = 14, face = "bold"),
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "#ff6666", size = 12, face = "italic", hjust = 0.5, margin = margin(b = 20)),
    legend.position = "none"
  )

# 3. Create the Bar Chart
loss_plot <- ggplot(pole_loss_data, aes(x = Outcome, y = Percentage, fill = Outcome)) +
  geom_col(width = 0.6) +
  
  # Add percentage labels directly on top of the bars
  geom_text(aes(label = paste0(Percentage, "%")), 
            vjust = -0.8, color = "white", size = 5, fontface = "bold") +
  
  # Custom colors matching the severity of the outcome
  scale_fill_manual(values = c("#d9534f", "#f0ad4e", "#5bc0de")) +
  
  labs(
    title = "Why the Fastest Car Lost (Classic Era: 1950–1980)",
    subtitle = "Overall Pole Sitter Win Rate: Only 36.3% (129 wins out of 355 starts)",
    x = "Outcome for Pole Sitters Who Did Not Win",
    y = "Percentage of Losses (%)"
  ) +
  
  # Expand the y-axis so the top labels fit nicely
  coord_cartesian(ylim = c(0, 55)) +
  dark_theme

# 4. Display the plot
print(loss_plot)



# Install ggplot2 if needed
# install.packages("ggplot2")

library(ggplot2)

# 1. Create the dataset for Classic Era repeat probabilities
repeat_data <- data.frame(
  Finish_Group = factor(
    c("Repeat Win (1st)", "Repeat Top 3", "Repeat Top 5", "Repeat Top 8", "Repeat Top 10"),
    levels = c("Repeat Win (1st)", "Repeat Top 3", "Repeat Top 5", "Repeat Top 8", "Repeat Top 10")
  ),
  Probability = c(22.4, 33.2, 37.2, 40.9, 43.8)
)

# 2. Define the matching custom dark theme
dark_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#121212", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major = element_line(color = "#333333", size = 0.5),
    panel.grid.minor = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "#b3b3b3", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1), # Tilt labels for spacing
    axis.title = element_text(color = "white", size = 14, face = "bold"),
    plot.title = element_text(color = "white", size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    legend.position = "none"
  )

# 3. Create the Bar Chart
repeat_plot <- ggplot(repeat_data, aes(x = Finish_Group, y = Probability, fill = Finish_Group)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(Probability, "%")), 
            vjust = -0.8, color = "white", size = 4.5, fontface = "bold") +
  
  # A cool continuous color scale
  scale_fill_viridis_d(option = "magma", begin = 0.4, end = 0.9) +
  
  labs(
    title = "Probability of Repeating a Result in the Next Race",
    subtitle = "Classic Era (1950–1980)",
    x = "Finish Target",
    y = "Repeat Probability (%)"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  dark_theme

# 4. Display the plot
print(repeat_plot)


 ##------------------

# Install required packages if you don't have them
# install.packages(c("dplyr", "ggplot2", "scales"))

library(dplyr)
library(ggplot2)
library(scales)

# 1. Load your dataset
f1_data <- read.csv("f1_master_data.csv")

# 2. Filter for the Classic Era and calculate Win Probabilities by Grid
classic_grid_stats <- f1_data %>%
  filter(year >= 1950 & year <= 1980) %>%
  # Make sure columns are numeric
  mutate(
    grid = as.numeric(grid),
    finish_position_order = as.numeric(finish_position_order)
  ) %>%
  # Filter out pit lane starts or missing grid data (grid 0 usually means pit lane)
  filter(grid > 0) %>%
  # Group the grid positions to include everyone outside the Top 10
  mutate(
    Grid_Group = case_when(
      grid == 1 ~ "1st (Pole)",
      grid == 2 ~ "2nd",
      grid == 3 ~ "3rd",
      grid == 4 ~ "4th",
      grid == 5 ~ "5th",
      grid >= 6 & grid <= 10 ~ "6th to 10th",
      grid > 10 ~ "11th+"
    )
  ) %>%
  group_by(Grid_Group) %>%
  summarise(
    Total_Starts = n(),
    Total_Wins = sum(finish_position_order == 1, na.rm = TRUE),
    Win_Percentage = (Total_Wins / Total_Starts) * 100
  ) %>%
  # Set the exact order of the categories for the x-axis
  mutate(
    Grid_Group = factor(Grid_Group, levels = c("1st (Pole)", "2nd", "3rd", "4th", "5th", "6th to 10th", "11th+"))
  )

# 3. Create the bar chart matching the image's visual style
classic_grid_plot <- ggplot(classic_grid_stats, aes(x = Grid_Group, y = Win_Percentage, fill = Grid_Group)) +
  geom_col(width = 0.7) +
  
  # Add the percentage labels on top of the bars
  geom_text(aes(label = sprintf("%.1f%%", Win_Percentage)), 
            vjust = -0.8, size = 4.5, fontface = "bold", color = "black") +
  
  # Custom colors including a new slate/grey for the 11th+ category
  scale_fill_manual(values = c(
    "1st (Pole)" = "#4a69bd",    # Blue
    "2nd" = "#38ada9",           # Cyan/Teal
    "3rd" = "#78e08f",           # Green
    "4th" = "#b8e994",           # Lime
    "5th" = "#fa983a",           # Orange
    "6th to 10th" = "#b71540",   # Dark Red
    "11th+" = "#7f8c8d"          # Slate Grey
  )) +
  
  # Light theme to match the image background
  theme_minimal() +
  labs(
    title = "F1 Win Probability by Starting Grid Position",
    subtitle = "Classic Era (1950–1980) Race Data",
    x = "Starting Grid Position",
    y = "Win Percentage (%)"
  ) +
  
  # Styling adjustments to make it look clean and professional
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#666666", margin = margin(b = 15)),
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "#e1e1e1"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Ensure there is enough space at the top for the labels
  coord_cartesian(ylim = c(0, max(classic_grid_stats$Win_Percentage) + 8))

# 4. Display the plot
print(classic_grid_plot)


