# Title: Data Cleaning
# Contributor: Edrian, Jonny
# Date: 2023-11-30
# Description:
# The following code visualize the data

library(ggplot2)
library(dplyr)
library(ggpie)
library(cowplot)

d <- read.csv("gpt_output/batch_all.csv")

# entire dataset ----

## pie chart ----
d_pie <- d |>
  mutate(sentiment = case_when(
    sentiment == 1 ~ "Pro-Israel",
    sentiment == 0 ~ "Neutral",
    sentiment == -1 ~ "Pro-Palestine",
    TRUE ~ as.character(sentiment)
  ))

ggpie(d_pie, sentiment) +
  scale_fill_manual(values=c("#D3D3D3", "#486078FF", "#B7e4C7")) +
  labs(title = "Sentiment Distribution")

## stacked bar chart ----
d_bar <- d |>
  mutate(sentiment = case_when(
    sentiment == 1 ~ "Pro-Israel",
    sentiment == 0 ~ "Neutral",
    sentiment == -1 ~ "Pro-Palestine",
    is.na(sentiment) ~ "NA",
    TRUE ~ as.character(sentiment)
  ))

d_bar |> mutate(sentiment = as.factor(sentiment)) |>
  ggplot(aes(x = created_at, fill = sentiment)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("Pro-Israel" = "#486078FF", "Neutral" = "#D3D3D3", "Pro-Palestine" = "#B7e4C7", "NA" = "darkgray")) +
  theme_classic() +
  labs(title = "Interaction Count Per Day", x = "Date (Oct)", y = "Count") +
  guides(fill = guide_legend(override.aes = list(color = c("#486078FF", "darkgray", "#B7e4C7", "white"))))
  


## interaction count scatter plot ----
d_interaction <- d |>
  mutate(sentiment = case_when(
    sentiment == 1 ~ "Pro-Israel",
    sentiment == 0 ~ "Neutral",
    sentiment == -1 ~ "Pro-Palestine",
    TRUE ~ as.character(sentiment)
  ))

d_interaction <- d_interaction |>
  mutate(sentiment = as.factor(sentiment))

ggplot(d_interaction, aes(y = sentiment, x = total_interactions_count, color = sentiment)) +
  geom_jitter(size = 0.5) +
  scale_color_manual(values = c("Pro-Israel" = "#486078FF", "Neutral" = "#D3D3D3", "Pro-Palestine" = "#B7e4C7")) +
  theme_bw() +
  labs(title = "Total Interactions Per Tweet", x = "Interaction Count", y = "Sentiment") +
  theme(panel.background = element_rect(fill = "white")) +
  guides(color = FALSE)

# keywords plotting ----

d_keywords <- read.csv("gpt_output/batch_all_keywords.csv")

d_is <- d_keywords |> filter(keyword_israel == 1)
d_pa <- d_keywords |> filter(keyword_palestine == 1)
d_co <- d_keywords |> filter(keyword_conflict == 1)
d_ha <- d_keywords |> filter(keyword_hamas == 1)
d_ga <- d_keywords |> filter(keyword_gaza == 1)

## grid -----

# all
d_avg_sen <- d_keywords |>
  mutate(sentiment = as.numeric(sentiment)) |>
  filter(!is.na(sentiment)) |>
  group_by(created_at) |>
  summarise(avg_sen = mean(sentiment, na.rm = TRUE))
p <- d_avg_sen |>
  ggplot(aes(x = created_at, y = avg_sen)) +
  geom_point(color = "darkgray", size = 1.5) +
  geom_line(color = "#486078FF", size = 0.7) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", color = "#B7e4C7", size = 0.7) +
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
  geom_point(color = "darkgray", size = 1.5) +
  geom_line(color = "#486078FF", size = 0.7) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", color = "#B7e4C7", size = 0.7) +
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
  geom_point(color = "darkgray", size = 1.5) +
  geom_line(color = "#486078FF", size = 0.7) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", color = "#B7e4C7", size = 0.7) +
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
  geom_point(color = "darkgray", size = 1.5) +
  geom_line(color = "#486078FF", size = 0.7) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", color = "#B7e4C7", size = 0.7) +
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
  geom_point(color = "darkgray", size = 1.5) +
  geom_line(color = "#486078FF", size = 0.7) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", color = "#B7e4C7", size = 0.7) +
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
  geom_point(color = "darkgray", size = 1.5) +
  geom_line(color = "#486078FF", size = 0.7) +
  geom_smooth(method = "gam", se = FALSE, linetype = "dashed", color = "#B7e4C7", size = 0.7) +
  labs(title = "Gaza", x = "Date (Oct)", y = "Average Sentiment") +
  theme_classic()

plot_grid(p, p_is, p_pa, p_co, p_ha, p_ga)
ggsave("gpt_output/Sentiment_Keyword_Grid.png", width = 8, height = 6)

## color ----

ggplot() +
  geom_point(data = d_avg_sen, mapping = aes(x = created_at, y = avg_sen), color = "darkred", linetype = "dashed") +
  geom_line(data = d_avg_sen, mapping = aes(x = created_at, y = avg_sen), color = "darkred", linetype = "dashed") +
  geom_text(aes(x = 20.5, y = -0.28, label = "All"), hjust = 0, color = "darkred") +
  geom_point(data = d_avg_sen_co, mapping = aes(x = created_at, y = avg_sen), color = "darkgray") +
  geom_line(data = d_avg_sen_co, mapping = aes(x = created_at, y = avg_sen), color = "darkgray") +
  geom_text(aes(x = 19, y = -0.05, label = "Conflict"), hjust = 0, color = "darkgray") +
  geom_point(data = d_avg_sen_ga, mapping = aes(x = created_at, y = avg_sen), color = "#009999") +
  geom_line(data = d_avg_sen_ga, mapping = aes(x = created_at, y = avg_sen), color = "#009999") +
  geom_text(aes(x = 20, y = -0.6, label = "Gaza"), hjust = 0, color = "#009999") +
  geom_point(data = d_avg_sen_ha, mapping = aes(x = created_at, y = avg_sen), color = "black") +
  geom_line(data = d_avg_sen_ha, mapping = aes(x = created_at, y = avg_sen), color = "black") +
  geom_text(aes(x = 19.7, y = 0.2, label = "Hamas"), hjust = 0, color = "black") +
  geom_point(data = d_avg_sen_is, mapping = aes(x = created_at, y = avg_sen), color = "#486078FF") +
  geom_line(data = d_avg_sen_is, mapping = aes(x = created_at, y = avg_sen), color = "#486078FF") +
  geom_text(aes(x = 20.2, y = -0.42, label = "Israel"), hjust = 0, color = "#486078FF") +
  geom_point(data = d_avg_sen_pa, mapping = aes(x = created_at, y = avg_sen), color = "#B7e4C7") +
  geom_line(data = d_avg_sen_pa, mapping = aes(x = created_at, y = avg_sen), color = "#B7e4C7") +
  geom_text(aes(x = 18, y = -0.8, label = "Palestine"), hjust = 0, color = "#B7e4C7") +
  labs(title = "Average Sentiments", x = "Dates (October)", y = "Average Sentiment") +
  theme_bw()
ggsave("gpt_output/Sentiment_Keyword_Colors.png", width = 8, height = 6)


  
