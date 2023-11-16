# Title: Data Cleaning
# Contributor: Edrian
# Date: 2023-11-16
# Description:
# The following code cleans and combines 14 datasets for the keyword "Israel"

# Load libraries ----

library(tidyverse)

# Load data
file_names <- c("Israel_0708.csv",
                "Israel_0809.csv",
                "Israel_0910.csv",
                "Israel_1011.csv",
                "Israel_1112.csv",
                "Israel_1213.csv",
                "Israel_1314.csv",
                "Israel_1415.csv",
                "Israel_1516.csv",
                "Israel_1617.csv",
                "Israel_1718.csv",
                "Israel_1819.csv",
                "Israel_1920.csv",
                "Israel_2021.csv")

df_list <- list() # create empty list to store data frame

for (file in file_names) {
  file_path <- paste0("data/israel/", file)
  df <- read.csv(file_path)
  df_list[[file]] <- df
}

# create function for filtering rows for twitter, and
# leaving only day (no year, month, and time) of the tweet

filter_twitter <- function(df) {
  twitter_df <- df[df$platform == "twitter",
                   c("text", "created_at")]
  twitter_df$created_at <- substr(twitter_df$created_at,9,10)
  return(twitter_df)
}

filtered_list <- lapply(df_list, filter_twitter)

# combine datasets into a big one

israel_combined <- bind_rows(filtered_list)

# arrange by date

israel_combined <- israel_combined |>
  arrange(created_at)

# filter English language

library(textcat)

israel_combined$language <- textcat(israel_combined$text)
israel_combined_eng <- israel_combined |>
  filter(language %in% c("english", "scots"))

# clean text (remove symbols, links, usernames, etc)

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

israel_combined_clean <- israel_combined_eng |>
  mutate(clean(israel_combined_eng$text)) |>
  rename("cleaned_text" = "clean(israel_combined_eng$text)") |>
  select(cleaned_text, created_at) |>
  distinct(cleaned_text, .keep_all = TRUE)

# count tweets per day and plot changes

israel_count <- israel_combined_clean |>
  distinct(cleaned_text, .keep_all = TRUE) |>
  group_by(created_at) |>
  count()

library(ggplot2)

ggplot(israel_count, aes(x = as.numeric(created_at),
                         y = n)) +
  geom_line()

# safe combined dataset, remove unneeded dataframes

write.csv(israel_combined_clean,
          file = "output/israel_clean.csv",
          row.names = FALSE)
write.csv(israel_count,
          file = "output/israel_count.csv",
          row.names = FALSE)

rm(df, df_list, filtered_list, file, file_names, file_path)