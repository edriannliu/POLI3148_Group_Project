# Title: Data Cleaning
# Contributor: Edrian, Jonny, Aurora
# Date: 2023-11-30
# Description:
# The following code visualize the data

library(ggplot2)
library(dplyr)
library(ggpie)

d <- read.csv("gpt_output/batch1.csv")

# time line ----

d_avg_sen <- d |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))

d_avg_sen |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line()

# pie chart ----

ggpie(d, sentiment)

# stacked bar chart ----

d |> mutate(sentiment = as.factor(sentiment)) |>
  ggplot(aes(x = created_at, fill = sentiment)) +
  geom_bar(position = "stack")

# interaction count scatter plot ----

# actual plot
d |> mutate(sentiment = as.factor(sentiment)) |>
  ggplot(aes(y = sentiment, x = total_interactions_count, color = sentiment)) +
  geom_jitter()
