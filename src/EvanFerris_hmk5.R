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
read.csv("data/colloquium_assessment.csv")
colloquium <- read_csv(
  "data/colloquium_assessment.csv",
  skip = 3,                      
  na = c("N/A", "", "NA"),
  show_col_types = FALSE
) %>%
  clean_names()
names(colloquium)
colloquium_long <- colloquium %>%
  pivot_longer(
    cols = x4:x11,
    names_to = "assessment_item",
    values_to = "score"
  )
glimpse(colloquium_long)

#Question 3
library(dplyr)

x7_10_avg <- colloquium_long %>%
  filter(assessment_item %in% c("x7", "x8", "x9", "x10")) %>%
  group_by(assessment_item) %>%
  summarise(
    average_score = mean(score, na.rm = TRUE)
  )
x7_10_avg
