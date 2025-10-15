# ======================================
# Peet's Coffee Workflow Optimization
# by Jake Riina
# ======================================
setwd("/Users/riinajake/Desktop/PeetsWorkflowOptimization")



# Load libraries
library(tidyverse)
library(simEd)
library(lubridate)

# Read time study data (replace with your file path later)
# Example: "data/peets_time_study.csv"
time_data <- read_csv("data/peets_time_study.csv")

# Preview data
print(head(time_data))

# Summary statistics
time_summary <- time_data %>%
  group_by(Task) %>%
  summarise(
    AvgTime = mean(Duration_sec, na.rm = TRUE),
    MedianTime = median(Duration_sec, na.rm = TRUE),
    Observations = n()
  )

print(time_summary)

# Visualization
ggplot(time_summary, aes(x = reorder(Task, AvgTime), y = AvgTime, fill = Task)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Average Task Time at Peet's Coffee (UM)",
       x = "Task",
       y = "Average Duration (seconds)") +
  theme_minimal()

# Simulation example: reduce espresso-making time by 10%
sim_data <- time_data %>%
  mutate(Duration_sec = ifelse(Task == "Make espresso",
                               Duration_sec * 0.9,
                               Duration_sec))

cat("\n--- Simulation Results ---\n")
cat("Original avg duration: ", mean(time_data$Duration_sec, na.rm = TRUE), "\n")
cat("Simulated avg duration: ", mean(sim_data$Duration_sec, na.rm = TRUE), "\n")

# Save simulation output
write_csv(sim_data, "outputs/simulated_time_data.csv")

dirs <- c("data","data/processed","R","outputs","docs")
sapply(dirs, function(d) if(!dir.exists(d)) dir.create(d, recursive=TRUE))

readme <- "
# Peet's Workflow Optimization (University of Miami)

This repo contains time-study templates, R scripts for analysis/simulation,
and outputs for the Peet's (Archivist Café) workflow optimization project.
"
writeLines(readme, "README.md")

gitignore <- c(".Rhistory", ".RData", ".Rproj.user", "data/*.csv", "outputs/*")
writeLines(gitignore, ".gitignore")

writeLines("obs_id,timestamp,observer_id,station,activity,shift,notes\n", "data/work_sampling_template.csv")
writeLines("order_id,operator_id,drink_type,element,start_time,end_time,duration_s\n", "data/cts_template.csv")
writeLines("event_id,operator_id,cause,interrupt_start,interrupt_end,resume_start,time_to_resume_s,order_id\n",
           "data/interruptions_template.csv")

# create lightweight R script placeholders
r_files <- c("R/01_read_data_and_clean.R", "R/02_work_sampling.R", "R/03_continuous_time.R", "R/04_simulation.R")
for (f in r_files) {
  if (!file.exists(f)) writeLines(c("# Placeholder - add code here", "# e.g. library(tidyverse)"), f)
}

message("Folders and templates created. Now run `git` commands in terminal to commit & push.")
snapback_data <- read_csv("data/snapback.csv")
head(snapback_data)
str(snapback_data)

library(tidyverse)  # if not already loaded

# Read the snapback CSV
snapback_data <- read_csv("data/snapback.csv")

# Preview first 6 rows
head(snapback_data)

# Check structure & data types
str(snapback_data)

getwd()


list.files()

# Summarize average and median duration per task
snap_summary <- snapback_data %>%
  group_by(Task) %>%
  summarise(
    AvgTime = mean(Duration_sec, na.rm = TRUE),
    MedianTime = median(Duration_sec, na.rm = TRUE),
    Observations = n()
  )

# Print summary
print(snap_summary)

library(ggplot2)

ggplot(snap_summary, aes(x = reorder(Task, AvgTime), y = AvgTime, fill = Task)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Average Snapback Times at Peet's Coffee (UM)",
    x = "Task",
    y = "Average Duration (seconds)"
  ) +
  theme_minimal()


# --- Simulation Example ---
# Reduce "WaitTimeOrder" by 10% to see the effect

sim_data <- snapback_data %>%
  mutate(Duration_sec = ifelse(Task == "WaitTimeOrder",
                               Duration_sec * 0.9,   # reduce by 10%
                               Duration_sec))

# Compare averages
cat("\n--- Simulation Results ---\n")
cat("Original average duration:", mean(snapback_data$Duration_sec, na.rm = TRUE), "\n")
cat("Simulated average duration:", mean(sim_data$Duration_sec, na.rm = TRUE), "\n")

# ======================================
# Workflow Simulation for Peet's Coffee
# ======================================

# Load libraries
library(tidyverse)

# --- Step 1: Define simulation adjustments ---
# You can reduce times for multiple tasks here
# Format: TaskName = reduction_factor
adjustments <- c(
  "WaitTimeOrder" = 0.9,    # reduce by 10%
  "OrderingTime" = 0.85,    # reduce by 15%
  "TimeToPrepareDrink" = 0.8 # reduce by 20%
)

# --- Step 2: Apply simulation ---
sim_data <- snapback_data %>%
  mutate(Duration_sec = ifelse(Task %in% names(adjustments),
                               Duration_sec * adjustments[Task],
                               Duration_sec))

# --- Step 3: Calculate totals per observation (if needed) ---
totals <- sim_data %>%
  group_by(obs_id) %>%
  summarise(TotalDuration_sec = sum(Duration_sec, na.rm = TRUE))

# --- Step 4: Summarize before and after ---
cat("\n--- Summary ---\n")
cat("Original avg duration per task:\n")
snapback_data %>%
  group_by(Task) %>%
  summarise(Avg = mean(Duration_sec, na.rm = TRUE)) %>%
  print()

cat("\nSimulated avg duration per task:\n")
sim_data %>%
  group_by(Task) %>%
  summarise(Avg = mean(Duration_sec, na.rm = TRUE)) %>%
  print()

cat("\nOriginal avg total per observation:", mean(totals$TotalDuration_sec, na.rm = TRUE), "\n")

# --- Step 5: Save simulated dataset ---
write_csv(sim_data, "outputs/simulated_snapback.csv")
write_csv(totals, "outputs/simulated_totals.csv")

# Load libraries
library(dplyr)
library(ggplot2)

# 1. Read CSV
df <- read.csv("data/work_sampling.csv")

# 2. Calculate total observations and proportions
df <- df %>%
  mutate(
    TotalObs = Manager + FoodAssembly + DrinkMaker,
    Prop_Manager = Manager / TotalObs,
    Prop_FoodAssembly = FoodAssembly / TotalObs,
    Prop_DrinkMaker = DrinkMaker / TotalObs
  )

# 3. Set observation time (seconds per observation)
obs_time <- 10

# 4. Calculate Normal Time per worker
df <- df %>%
  mutate(
    NormalTime_Manager = Prop_Manager * TotalObs * obs_time,
    NormalTime_FoodAssembly = Prop_FoodAssembly * TotalObs * obs_time,
    NormalTime_DrinkMaker = Prop_DrinkMaker * TotalObs * obs_time
  )

# 5. Set performance rate and allowance
performance_rate <- 1.0  # 100%
allowance <- 0.15         # 15%

# 6. Calculate Standard Time
df <- df %>%
  mutate(
    StandardTime_Manager = NormalTime_Manager / performance_rate * (1 + allowance),
    StandardTime_FoodAssembly = NormalTime_FoodAssembly / performance_rate * (1 + allowance),
    StandardTime_DrinkMaker = NormalTime_DrinkMaker / performance_rate * (1 + allowance)
  )

# 7. Summarize total times
total_normal <- sum(df$NormalTime_Manager + df$NormalTime_FoodAssembly + df$NormalTime_DrinkMaker)
total_standard <- sum(df$StandardTime_Manager + df$StandardTime_FoodAssembly + df$StandardTime_DrinkMaker)

cat("Total Normal Time (seconds):", total_normal, "\n")
cat("Total Standard Time (seconds):", total_standard, "\n")

# 8. Example Workflow Simulation: reduce food prep by 10%
df$Scenario_FoodPrep <- ifelse(df$Task == "Prepare Food",
                               df$NormalTime_FoodAssembly * 0.9,
                               df$NormalTime_FoodAssembly)

total_scenario <- sum(df$NormalTime_Manager + df$Scenario_FoodPrep + df$NormalTime_DrinkMaker)
cat("Total Time with 10% faster Food Prep:", total_scenario, "\n")

# 9. Bar Chart of Normal Times per Task (stacked by worker)
df_long <- df %>%
  select(Task, NormalTime_Manager, NormalTime_FoodAssembly, NormalTime_DrinkMaker) %>%
  tidyr::pivot_longer(cols = -Task, names_to = "Worker", values_to = "Time")

ggplot(df_long, aes(x=Task, y=Time, fill=Worker)) +
  geom_bar(stat="identity") +
  labs(title="Normal Time per Task by Worker", y="Time (seconds)", x="Task") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Load libraries
library(dplyr)
library(ggplot2)

# 1. Read CSV
df <- read.csv("data/work_sampling.csv")

# 2. Calculate total observations and proportions
df <- df %>%
  mutate(
    TotalObs = Manager + FoodAssembly + DrinkMaker,
    Prop_Manager = Manager / TotalObs,
    Prop_FoodAssembly = FoodAssembly / TotalObs,
    Prop_DrinkMaker = DrinkMaker / TotalObs
  )

# 3. Set observation time (seconds per observation)
obs_time <- 10

# 4. Calculate Normal Time per worker
df <- df %>%
  mutate(
    NormalTime_Manager = Prop_Manager * TotalObs * obs_time,
    NormalTime_FoodAssembly = Prop_FoodAssembly * TotalObs * obs_time,
    NormalTime_DrinkMaker = Prop_DrinkMaker * TotalObs * obs_time
  )

# 5. Set performance rate and allowance
performance_rate <- 1.0  # 100%
allowance <- 0.15         # 15%

# 6. Calculate Standard Time
df <- df %>%
  mutate(
    StandardTime_Manager = NormalTime_Manager / performance_rate * (1 + allowance),
    StandardTime_FoodAssembly = NormalTime_FoodAssembly / performance_rate * (1 + allowance),
    StandardTime_DrinkMaker = NormalTime_DrinkMaker / performance_rate * (1 + allowance)
  )

# 7. Summarize total times
total_normal <- sum(df$NormalTime_Manager + df$NormalTime_FoodAssembly + df$NormalTime_DrinkMaker)
total_standard <- sum(df$StandardTime_Manager + df$StandardTime_FoodAssembly + df$StandardTime_DrinkMaker)

cat("Total Normal Time (seconds):", total_normal, "\n")
cat("Total Standard Time (seconds):", total_standard, "\n")

# 8. Example Workflow Simulation: reduce food prep by 10%
df$Scenario_FoodPrep <- ifelse(df$Task == "Prepare Food",
                               df$NormalTime_FoodAssembly * 0.9,
                               df$NormalTime_FoodAssembly)

total_scenario <- sum(df$NormalTime_Manager + df$Scenario_FoodPrep + df$NormalTime_DrinkMaker)
cat("Total Time with 10% faster Food Prep:", total_scenario, "\n")

# 9. Bar Chart of Normal Times per Task (stacked by worker)
df_long <- df %>%
  select(Task, NormalTime_Manager, NormalTime_FoodAssembly, NormalTime_DrinkMaker) %>%
  tidyr::pivot_longer(cols = -Task, names_to = "Worker", values_to = "Time")

ggplot(df_long, aes(x=Task, y=Time, fill=Worker)) +
  geom_bar(stat="identity") +
  labs(title="Normal Time per Task by Worker", y="Time (seconds)", x="Task") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Load libraries
library(dplyr)
library(ggplot2)

# 1. Read CSV
df <- read.csv("data/work_sampling.csv")

# 2. Calculate total observations and proportions
df <- df %>%
  mutate(
    TotalObs = Manager + FoodAssembly + DrinkMaker,
    Prop_Manager = Manager / TotalObs,
    Prop_FoodAssembly = FoodAssembly / TotalObs,
    Prop_DrinkMaker = DrinkMaker / TotalObs
  )

# 3. Set observation time (seconds per observation)
obs_time <- 10

# 4. Calculate Normal Time per worker
df <- df %>%
  mutate(
    NormalTime_Manager = Prop_Manager * TotalObs * obs_time,
    NormalTime_FoodAssembly = Prop_FoodAssembly * TotalObs * obs_time,
    NormalTime_DrinkMaker = Prop_DrinkMaker * TotalObs * obs_time
  )

# 5. Set performance rate and allowance
performance_rate <- 1.0  # 100%
allowance <- 0.15         # 15%

# 6. Calculate Standard Time
df <- df %>%
  mutate(
    StandardTime_Manager = NormalTime_Manager / performance_rate * (1 + allowance),
    StandardTime_FoodAssembly = NormalTime_FoodAssembly / performance_rate * (1 + allowance),
    StandardTime_DrinkMaker = NormalTime_DrinkMaker / performance_rate * (1 + allowance)
  )

# 7. Summarize total times
total_normal <- sum(df$NormalTime_Manager + df$NormalTime_FoodAssembly + df$NormalTime_DrinkMaker)
total_standard <- sum(df$StandardTime_Manager + df$StandardTime_FoodAssembly + df$StandardTime_DrinkMaker)

cat("Total Normal Time (seconds):", total_normal, "\n")
cat("Total Standard Time (seconds):", total_standard, "\n")

# 8. Example Workflow Simulation: reduce food prep by 10%
df$Scenario_FoodPrep <- ifelse(df$Task == "Prepare Food",
                               df$NormalTime_FoodAssembly * 0.9,
                               df$NormalTime_FoodAssembly)

total_scenario <- sum(df$NormalTime_Manager + df$Scenario_FoodPrep + df$NormalTime_DrinkMaker)
cat("Total Time with 10% faster Food Prep:", total_scenario, "\n")

# 9. Bar Chart of Normal Times per Task (stacked by worker)
df_long <- df %>%
  select(Task, NormalTime_Manager, NormalTime_FoodAssembly, NormalTime_DrinkMaker) %>%
  tidyr::pivot_longer(cols = -Task, names_to = "Worker", values_to = "Time")

ggplot(df_long, aes(x=Task, y=Time, fill=Worker)) +
  geom_bar(stat="identity") +
  labs(title="Normal Time per Task by Worker", y="Time (seconds)", x="Task") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# Load libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Read CSV
df <- read.csv("data/work_sampling.csv", stringsAsFactors = FALSE)

# 2. Clean column names (remove spaces, make consistent)
colnames(df) <- gsub(" ", "", colnames(df))  # remove spaces
colnames(df) <- gsub("-", "", colnames(df))  # remove hyphens if any
colnames(df) <- gsub("\\.", "", colnames(df)) # remove dots

# Expected columns now: Task, Manager, FoodAssembly, DrinkMaker
print(colnames(df))  # check column names

# 3. Calculate total observations and proportions
df <- df %>%
  mutate(
    TotalObs = Manager + FoodAssembly + DrinkMaker,
    Prop_Manager = Manager / TotalObs,
    Prop_FoodAssembly = FoodAssembly / TotalObs,
    Prop_DrinkMaker = DrinkMaker / TotalObs
  )

# 4. Set observation time (seconds per observation)
obs_time <- 10

# 5. Calculate Normal Time per worker
df <- df %>%
  mutate(
    NormalTime_Manager = Prop_Manager * TotalObs * obs_time,
    NormalTime_FoodAssembly = Prop_FoodAssembly * TotalObs * obs_time,
    NormalTime_DrinkMaker = Prop_DrinkMaker * TotalObs * obs_time
  )

# 6. Set performance rate and allowance
performance_rate <- 1.0  # 100%
allowance <- 0.15         # 15%

# 7. Calculate Standard Time
df <- df %>%
  mutate(
    StandardTime_Manager = NormalTime_Manager / performance_rate * (1 + allowance),
    StandardTime_FoodAssembly = NormalTime_FoodAssembly / performance_rate * (1 + allowance),
    StandardTime_DrinkMaker = NormalTime_DrinkMaker / performance_rate * (1 + allowance)
  )

# 8. Summarize total times
total_normal <- sum(df$NormalTime_Manager + df$NormalTime_FoodAssembly + df$NormalTime_DrinkMaker)
total_standard <- sum(df$StandardTime_Manager + df$StandardTime_FoodAssembly + df$StandardTime_DrinkMaker)

cat("Total Normal Time (seconds):", total_normal, "\n")
cat("Total Standard Time (seconds):", total_standard, "\n")

# 9. Example Workflow Simulation: reduce food prep by 10%
df$Scenario_FoodPrep <- ifelse(df$Task == "PrepareFood",
                               df$NormalTime_FoodAssembly * 0.9,
                               df$NormalTime_FoodAssembly)

total_scenario <- sum(df$NormalTime_Manager + df$Scenario_FoodPrep + df$NormalTime_DrinkMaker)
cat("Total Time with 10% faster Food Prep:", total_scenario, "\n")

# 10. Bar Chart of Normal Times per Task (stacked by worker)
df_long <- df %>%
  select(Task, NormalTime_Manager, NormalTime_FoodAssemb
         
df
         
         
         