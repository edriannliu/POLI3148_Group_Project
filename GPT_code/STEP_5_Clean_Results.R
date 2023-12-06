# =================================================================
# Gather GPT labeling results and clean
# =================================================================

rm(list=ls())

# Load packages ----
library(tidyverse)

# Load tweet text ----
data_to_label <- read_csv("/cloud/project/data/data_to_label_GPT.csv") |>
  select(-sentiment)

# Add a "batch" indicator to tweets ----
set.seed(5)
data_to_label$batch <- sample(1:20, nrow(data_to_label), replace = TRUE)


# Get batch directory ----
current_batch <- 4 # Set Batch number

dir_output <- file.path("output_unlabeled", current_batch)
data_batch <- data_to_label |> filter(batch == current_batch)

fnames_response <- list.files(dir_output)

d_gptlabel_raw <- list()

pb <- txtProgressBar(min = 0, max = 1, style = 3)
for (i in seq_along(fnames_response)){
  x <- fnames_response[i]
  path_file <- file.path(dir_output, x)
  response <- read_rds(path_file)
  gpt_label <- content(response)$choices[[1]]$message$content
  error_msg <- ifelse(is.null(gpt_label), content(response)$error$message, "")
  d_gptlabel_raw[[i]] <- c(filename = x, sentiment = gpt_label, error = error_msg)
  setTxtProgressBar(pb, i)
}

d_gptlabel_raw <- bind_rows(d_gptlabel_raw)

d_gptlabel <- d_gptlabel_raw |> 
  mutate(id_of_tweet = as.numeric(str_remove(filename, "\\.rds$")), .after = "filename") |>
  select(id_of_tweet, sentiment, error)

d_gptlabel <- data_batch |> inner_join(d_gptlabel, by = "id_of_tweet")

write_rds(d_gptlabel, file.path("output_unlabeled_clean/", str_c("Batch_", current_batch, ".rds")))

# Check causes of error
d_gptlabel |> select(error) |> group_by(error) |> count()
