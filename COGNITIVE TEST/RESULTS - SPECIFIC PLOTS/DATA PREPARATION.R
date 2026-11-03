# DATA PREPARATION FOR SPECIFIC PLOTS
library(dplyr)
library(stringr)
library(ggplot2)
library(gt)
library(tidyr)

df <- read.csv("eval_dataset_final_scored.csv", stringsAsFactors = FALSE)

df_clean <- df %>%
  mutate(
    score_system1 = as.numeric(score_system1),
    score_system2 = as.numeric(score_system2),
    # Error calculation (0 = Perfect, 1 = Completely wrong)
    error_sys1 = 1 - score_system1,
    error_sys2 = 1 - score_system2,
    # Condition extraction: "Clean" vs "Perturbed"
    condition = if_else(str_detect(category, "_perturbed"), "Perturbed", "Clean"),
    # Extraction of the clean base task name
    base_task = str_remove(category, "_perturbed")
  )