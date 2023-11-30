# Title: Data Cleaning
# Contributor: Edrian, Jonny, Aurora
# Date: 2023-11-30
# Description:
# The following code visualize the data

library(ggplot2)
library(dplyr)
library(ggpie)

d <- read.csv("output/visualization_experiment.csv")

# time line ----

d_avg_sen <- d |>
  group_by(created_at) |>
  summarise(avg_sen = mean(Advanced_Sentiment)) |>
  filter(!created_at == 7)

d_avg_sen |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line()

# pie chart ----

ggpie(d, Advanced_Sentiment)

# stacked bar chart ----

d |> mutate(sentiment = as.factor(Advanced_Sentiment)) |>
  ggplot(aes(x = created_at, fill = sentiment)) +
  geom_bar(position = "stack")

# interaction count scatter plot ----

# create fake column of interaction count
d_interaction <- d |>
  mutate(interaction = sample(1:1000, nrow(d), replace = TRUE))

# actual plot
d_interaction |> mutate(sentiment = as.factor(Advanced_Sentiment)) |>
  ggplot(aes(y = sentiment, x = interaction, color = sentiment)) +
  geom_jitter()