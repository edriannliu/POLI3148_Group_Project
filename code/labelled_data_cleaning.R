# Title: Labelled Data Cleaning
# Contributor: Edrian
# Description: the following code make the GPT labelled data ready for visualization and analysis

library(tidyverse)

# assign ID
d_all <- read.csv("output/combined_clean.csv")
d_all_id <- d_all |>
  mutate(id_of_tweet = seq(1000, length.out = nrow(d_all), by = 1))
write.csv(d_all_id, "gpt_labelled_data/all_data.csv")

# load GPT labelled data
d_gpt <- read.csv("gpt_labelled_data/batch_1_raw.csv")

# add "created_at" and "interaction_count" columns
d <- inner_join(d_gpt, d_all_id, by = "id_of_tweet") |>
  select(-cleaned_text.y)

# get rid of error
d <- d |> filter(!grepl("1\\n1", sentiment))

write.csv(d, "gpt_output/batch1.csv")
