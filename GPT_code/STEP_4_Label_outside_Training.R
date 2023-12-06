# =================================================================
# Label tweetst that are not annotated by human coders 
# Running this script makes costly API calls. Treat it carefully
# =================================================================


## Step 4: Sentiment labeling of the tweets collected by GPT-4 (through API)
rm(list=ls())

# Load packages ----
library(tidyverse)

# Load tweet text ----
data_to_label <- read_csv("/cloud/project/data/data_to_label_GPT.csv") |>
  select(-sentiment)

# Add a "batch" indicator to tweets ----
set.seed(5)
data_to_label$batch <- sample(1:20, nrow(data_to_label), replace = TRUE)
table(data_to_label$batch)

# Get batch directory ----
current_batch <- 4
dir_output <- file.path("output_unlabeled", current_batch)
dir.create(dir_output)
data_batch <- data_to_label |> filter(batch == current_batch)

# Load instructions ----
instructions <- readLines("/cloud/project/data/Twitter_Sentiment_Analysis_Instructions(GPT4 rewritten)_v5.txt") |>
  str_c(collapse = "\n") 
cat(instructions)

 
# Load API key and url ----
api_key <- readLines("/cloud/project/data/api_info/api_key.txt")
api_url <- readLines("/cloud/project/data/api_info/api_url_gpt4.txt")


# Make API calls ----
i <- 1

for (i in 1:nrow(data_batch)) {
  to_annotate_id <- data_batch$id_of_tweet[i]
  to_annotate_text <- data_batch$cleaned_text[i]
  
  file_path <- file.path(dir_output, str_c(to_annotate_id, ".rds"))
  # If the tweet has been classified by ChatGPT, skip.
  if (file.exists(file_path)){
    response <- read_rds(file_path)
    gpt_label <- content(response)$choices[[1]]$message$content
    error_msg <- ifelse(is.null(gpt_label), content(response)$error$code, "")
    if (error_msg %in% c("", "content_filter")){
      message(i, " of ", nrow(data_batch), " label: ", gpt_label, error_msg, " SKIPPED!")
      next
    }
  }
  
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
  gpt_label <- content(response)$choices[[1]]$message$content
  error_msg <- ifelse(is.null(gpt_label), content(response)$error$code, "")
  
  write_rds(response, file_path)
  Sys.sleep(1)
  message(i, " of ", nrow(data_batch), " label: ", gpt_label, error_msg)
}
