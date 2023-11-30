# Title: clean and synthesize two manually tagged datasets (by Luc and Jonny)
# Contributor: Edrian
# Date: 2023-11-16
# Description:
# The following code synthesizes the two manually tagged datasets in "data"
# folder, cleans the data, and produce a combined dataset in "output" folder.
# Note: -1 (Pro-Palestine), 0 (Neutral), 1 (Pro-Israel)

# Load data

library(tidyverse)
d_hamas_0721 <- read_csv("data/Hamas_0721_sentiment.csv")
d_israel_0708 <- read_csv("data/Israel_0708_sentiment.csv")

# clean israel data

d_israel_0708 <- d_israel_0708 |>
  select(Tweet, Sentiment) |>
  rename("text" = "Tweet", "sentiment" = "Sentiment")
d_israel_0708 <- d_israel_0708[complete.cases(d_israel_0708), ]
d_israel_0708$sentiment <- as.numeric(d_israel_0708$sentiment)
  
# combine two datasets

d_tagged <- bind_rows(d_hamas_0721, d_israel_0708)

# clean emojis, etc.
clean = function(text) {
  text = iconv(text, "latin1", "ASCII", sub="") #change encoding
  text = gsub("(@)\\w+", "", text) #remove numbers, alphabets after "@" (username)
  text = gsub("(http|https)://.*", "", text) # remove links
  text = gsub("[ \t]{2,}", "", text) #remove two blank spaces and tab
  text = gsub("\\n"," ",text) # remove newline
  text = gsub("\\s+"," ",text) # remove blank spaces
  text = gsub("^\\s+|\\s+$","",text) # remove blank spaces
  text = gsub("&.*;","",text) # remove special symbols and html
  text = gsub("[^a-zA-Z0-9?!. ']","",text) # remove emojis
}

d_tagged_clean <- d_tagged |>
  mutate(clean(d_tagged$text)) |>
  rename("cleaned_text" = "clean(d_tagged$text)") |>
  select(cleaned_text, sentiment) |>
  distinct(cleaned_text, .keep_all = TRUE)

write.csv(d_tagged_clean, file = "output/manually_tagged_sentiment.csv",
          row.names = FALSE)

d_tagged_count <- d_tagged_clean |>
  group_by(sentiment) |>
  count()
print(d_tagged_count)
