# load packages
library(tidyverse)
library(httr)
library(jsonlite)

## Step 1: Create instructions and a "training" set
instructions <- readLines("/cloud/project/data/Twitter_Sentiment_Analysis_Instructions(GPT4 rewritten)_v5.txt") |>
  str_c(collapse = "\n") 
cat(instructions)

human_label_data <- read_csv("/cloud/project/data/converged_labeled_data.csv")

# set.seed(55)
# d_train <- human_label_data |> 
# select(id_of_tweet, cleaned_text, sentiment) |>
# filter(!is.na(sentiment)) |>
# group_by(sentiment) |>
# sample_n(10) |> 
# ungroup() |>
# arrange(id_of_tweet)

d_train <- human_label_data

table(d_train$sentiment)


## Step 2: Apply ChatGPT to the "training" set
# GPT4 
api_key <- readLines("/cloud/project/data/api_info/api_key.txt")
api_url <- readLines("/cloud/project/data/api_info/api_url_gpt4.txt")

# Make API calls
for (i in 1:nrow(d_train)) 
{
  to_annotate_id <- d_train$id_of_tweet[i]
  to_annotate_text <- d_train$cleaned_text[i]
  # Above I do a small string operation -- replacing line breaks by spaces
  to_annotate_raLabel <- d_train$sentiment[i]
  
  response <- POST(
    url = api_url, 
    add_headers(`Content-Type` = "application/json", `api-key` = api_key),
    encode = "json",
    body = list(
      temperature = 0, 
      messages = list(
        list(role = "system", content = instructions),
        list(role = "user", content = to_annotate_text))
    )
  )
  to_annotate_gptLabel <- content(response)$choices[[1]]$message$content
  
  write_rds(response, str_c("/cloud/project/output/human_GPT_benchmark/", to_annotate_id, ".rds")) 
  Sys.sleep(1) # Sleep for 0.5 seconds after finishing each doc.
  message(i, " of ", nrow(d_train))
  # Optional below: Print results to get a "live update"
  message("status_id: ", to_annotate_id, "\n", "text: ", to_annotate_text)
  message("Human: ", to_annotate_raLabel, "\t", "ChatGPT: ", to_annotate_gptLabel, "\n")
}

# Cleaning
file_names <- list.files("/cloud/project/output/human_GPT_benchmark/")
gpt_labels <- rep(NA, length(file_names))

for (i in seq_along(file_names))
{
  response <- read_rds(file.path("/cloud/project/output/human_GPT_benchmark/", file_names[i]))
  gpt_labels[i] <- 
    ifelse(
      is.null(content(response)$choices[[1]]$message$content),
      NA, content(response)$choices[[1]]$message$content)
  # The above ifelse() function handles the situation when the output is "content-moderated" by Microsoft!
}
d_gptLabel <- tibble(
  id_of_tweet = str_remove(file_names, "\\.rds$") |> as.numeric(), 
  gpt = gpt_labels)
d_gptLabel |> print(n = 5)

d_train_merge <- d_train |> rename("human" = "sentiment") |> inner_join(d_gptLabel, by = "id_of_tweet")

with(d_train_merge, table(human, gpt, useNA = "ifany"))



