# Title: Data Cleaning
# Contributor: Edrian, Jonny, Aurora
# Date: 2023-11-30
# Description:
# The following code visualize the data

library(ggplot2)
library(dplyr)
library(ggpie)
library(cowplot)

d <- read.csv("gpt_output/batch_all.csv")

# time line ----

d_avg_sen <- d |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))

d_avg_sen |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed") +
  theme_classic()

# pie chart ----

ggpie(d, sentiment)

# stacked bar chart ----

d |> mutate(sentiment = as.factor(sentiment)) |>
  ggplot(aes(x = created_at, fill = sentiment)) +
  geom_bar(position = "stack") +
  theme_classic()

# interaction count scatter plot ----

d |> mutate(sentiment = as.factor(sentiment)) |>
  ggplot(aes(y = sentiment, x = total_interactions_count, color = sentiment)) +
  geom_jitter() +
  theme_classic()






# keywords plotting ----

d_keywords <- read.csv("gpt_output/batch_all_keywords.csv")

d_is <- d_keywords |> filter(keyword_israel == 1)
d_pa <- d_keywords |> filter(keyword_palestine == 1)
d_co <- d_keywords |> filter(keyword_conflict == 1)
d_ha <- d_keywords |> filter(keyword_hamas == 1)
d_ga <- d_keywords |> filter(keyword_gaza == 1)

# all
d_avg_sen <- d_keywords |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))
p <- d_avg_sen |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed") +
  labs(title = "All", x = "Date (Oct)", y = "Average Sentiment") +
  theme_classic()

# israel
d_avg_sen_is <- d_is |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))
p_is <- d_avg_sen_is |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed") +
  labs(title = "Israel", x = "Date (Oct)", y = "Average Sentiment") +
  theme_classic()

# palestine
d_avg_sen_pa <- d_pa |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))
p_pa <- d_avg_sen_pa |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed") +
  labs(title = "Palestine", x = "Date (Oct)", y = "Average Sentiment") +
  theme_classic()

# conflict
d_avg_sen_co <- d_co |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))
p_co <- d_avg_sen_co |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed") +
  labs(title = "Conflict", x = "Date (Oct)", y = "Average Sentiment") +
  theme_classic()

# hamas
d_avg_sen_ha <- d_ha |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))
p_ha <- d_avg_sen_ha |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed") +
  labs(title = "Hamas", x = "Date (Oct)", y = "Average Sentiment") +
  theme_classic()

# gaza
d_avg_sen_ga <- d_ga |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))
p_ga <- d_avg_sen_ga |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed") +
  labs(title = "Gaza", x = "Date (Oct)", y = "Average Sentiment") +
  theme_classic()

plot_grid(p, p_is, p_pa, p_co, p_ha, p_ga)
ggsave("gpt_output/Sentiment_Keyword_Grid.png", width = 8, height = 6)

ggplot() +
  geom_point(data = d_avg_sen, mapping = aes(x = created_at, y = avg_sen), color = "red", linetype = "dashed") +
  geom_line(data = d_avg_sen, mapping = aes(x = created_at, y = avg_sen), color = "red", linetype = "dashed") +
  geom_text(aes(x = 20.5, y = -0.28, label = "All"), hjust = 0, color = "red") +
  geom_point(data = d_avg_sen_co, mapping = aes(x = created_at, y = avg_sen), color = "gold") +
  geom_line(data = d_avg_sen_co, mapping = aes(x = created_at, y = avg_sen), color = "gold") +
  geom_text(aes(x = 19.9, y = -0.05, label = "Conflict"), hjust = 0, color = "gold") +
  geom_point(data = d_avg_sen_ga, mapping = aes(x = created_at, y = avg_sen), color = "orange") +
  geom_line(data = d_avg_sen_ga, mapping = aes(x = created_at, y = avg_sen), color = "orange") +
  geom_text(aes(x = 20.3, y = -0.6, label = "Gaza"), hjust = 0, color = "orange") +
  geom_point(data = d_avg_sen_ha, mapping = aes(x = created_at, y = avg_sen), color = "darkgreen") +
  geom_line(data = d_avg_sen_ha, mapping = aes(x = created_at, y = avg_sen), color = "darkgreen") +
  geom_text(aes(x = 20, y = 0.2, label = "Hamas"), hjust = 0, color = "darkgreen") +
  geom_point(data = d_avg_sen_is, mapping = aes(x = created_at, y = avg_sen), color = "blue") +
  geom_line(data = d_avg_sen_is, mapping = aes(x = created_at, y = avg_sen), color = "blue") +
  geom_text(aes(x = 20.3, y = -0.42, label = "Israel"), hjust = 0, color = "blue") +
  geom_point(data = d_avg_sen_pa, mapping = aes(x = created_at, y = avg_sen), color = "darkred") +
  geom_line(data = d_avg_sen_pa, mapping = aes(x = created_at, y = avg_sen), color = "darkred") +
  geom_text(aes(x = 18, y = -0.8, label = "Palestine"), hjust = 0, color = "darkred") +
  labs(title = "Average Sentiments", x = "Dates (October)", y = "Average Sentiment") +
  theme_bw()
ggsave("gpt_output/Sentiment_Keyword_Colors.png", width = 8, height = 6)


  
