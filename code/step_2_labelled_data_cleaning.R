# Title: Labelled Data Cleaning
# Contributor: Edrian
# Description: the following code make the GPT labelled data ready for visualization and analysis

library(tidyverse)

# assign ID to original data ----
d_all <- read.csv("output/combined_clean.csv")
d_all_id <- d_all |>
  mutate(id_of_tweet = seq(1000, length.out = nrow(d_all), by = 1))
write.csv(d_all_id, "gpt_labelled_data/all_data.csv")

# GPT dataset get date and interaction column ----

# load GPT labelled data
d_b1 <- readRDS("gpt_labelled_data/Batch_1.rds")
d_b2 <- readRDS("gpt_labelled_data/Batch_2.rds")
d_b3 <- readRDS("gpt_labelled_data/Batch_3.rds")
d_b4 <- readRDS("gpt_labelled_data/Batch_4.rds")

# read data with ID
d_id <- read.csv("gpt_labelled_data/all_data.csv")

# add "created_at" and "interaction_count" columns
d1 <- inner_join(d_b1, d_id, by = "id_of_tweet") |>
  select(-cleaned_text.y, -X)
d2 <- inner_join(d_b2, d_id, by = "id_of_tweet") |>
  select(-cleaned_text.y, -X)
d3 <- inner_join(d_b3, d_id, by = "id_of_tweet") |>
  select(-cleaned_text.y, -X)
d4 <- inner_join(d_b4, d_id, by = "id_of_tweet") |>
  select(-cleaned_text.y, -X)
d <- bind_rows(d1, d2, d3, d4)

# get rid of error
d <- d |> filter(!grepl("1\\n1", sentiment))

write.csv(d1, "gpt_output/batch1.csv")
write.csv(d2, "gpt_output/batch2.csv")
write.csv(d3, "gpt_output/batch3.csv")
write.csv(d4, "gpt_output/batch4.csv")
write.csv(d, "gpt_output/batch_all.csv")


# add keyword columns ----
# all
d_id$cleaned_text <- tolower(d_id$cleaned_text)

d_keywords <- d_id |>
  mutate(cleaned_text = tolower(cleaned_text)) |>
  mutate(keyword_israel = ifelse(str_detect(cleaned_text, paste0("israel")), 1, 0)) |>
  mutate(keyword_gaza = ifelse(str_detect(cleaned_text, paste0("gaza")), 1, 0)) |>
  mutate(keyword_palestine = ifelse(str_detect(cleaned_text, paste0("palestine")), 1, 0)) |>
  mutate(keyword_hamas = ifelse(str_detect(cleaned_text, paste0("hamas")), 1, 0)) |>
  mutate(keyword_conflict = ifelse(str_detect(cleaned_text, paste0("conflict")), 1, 0))

write.csv(d_keywords, "gpt_output/all_keywords.csv")

# batches
d_batch <- read.csv("gpt_output/batch_all.csv")

d_b_keywords <- d_batch |>
  rename("cleaned_text" = "cleaned_text.x") |>
  mutate(cleaned_text = tolower(cleaned_text)) |>
  mutate(keyword_israel = ifelse(str_detect(cleaned_text, paste0("israel")), 1, 0)) |>
  mutate(keyword_gaza = ifelse(str_detect(cleaned_text, paste0("gaza")), 1, 0)) |>
  mutate(keyword_palestine = ifelse(str_detect(cleaned_text, paste0("palestine")), 1, 0)) |>
  mutate(keyword_hamas = ifelse(str_detect(cleaned_text, paste0("hamas")), 1, 0)) |>
  mutate(keyword_conflict = ifelse(str_detect(cleaned_text, paste0("conflict")), 1, 0))

write.csv(d_b_keywords, "gpt_output/batch_all_keywords.csv")
