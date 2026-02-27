#Question 1
library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(janitor)
read.csv("data/sample_data.csv")
students <- read_csv("data/sample_data.csv", na = c("N/A", ""))
students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )
students |>
  janitor::clean_names() |>
  mutate(meal_plan = factor(meal_plan))
students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )
students

#Question 2
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
colloquium <- read_csv(
  "data/colloquium_assessment.csv",
  show_col_types = FALSE
) %>%
  clean_names()
colloquium_clean <- colloquium %>%
  filter(!str_detect(as.character(start_date), "^\\{"))
colloquium_long <- colloquium_clean %>%
  pivot_longer(
    cols = q7:q10,
    names_to = "assessment_item",
    values_to = "score"
  ) %>%
  filter(str_detect(score, "^[1-5]$")) %>%
  mutate(score = as.numeric(score))

#Question 3
student_avg <- colloquium_long %>%
  group_by(response_id) %>%
  summarise(
    avg_q7_q10 = mean(score),
    .groups = "drop"
  )
student_avg
