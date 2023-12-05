# Title: Data Cleaning
# Contributor: Edrian
# Date: 2023-11-16
# Description:
# The following code cleans and combines 14 datasets for the five keywords

# Load libraries ----

library(tidyverse) # data wrangling
library(textcat) # filtering language

# Create Functions ----

# 1. create function for filtering rows for twitter, and
#    leaving only day (no year, month, and time) of the tweet

filter_twitter <- function(df) {
  twitter_df <- df[df$platform == "twitter",
                   c("text", "created_at", "total_interactions_count")]
  twitter_df$created_at <- substr(twitter_df$created_at,9,10)
  return(twitter_df)
}

# 2. clean text (remove symbols, links, usernames, etc)
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

# Israel ----

# Load data
file_names <- c("Israel_0607.csv",
                "Israel_0708.csv",
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
                "Israel_2021.csv",
                "Israel_2122.csv")

df_list <- list() # create empty list to store data frame

for (file in file_names) {
  file_path <- paste0("data/israel/", file)
  df <- read.csv(file_path)
  df_list[[file]] <- df
}

filtered_list <- lapply(df_list, filter_twitter)

# combine datasets into a big one

israel_combined <- bind_rows(filtered_list)

# arrange by date, get rid of Oct 06 and Oct 22

israel_combined <- israel_combined |>
  arrange(created_at) |>
  filter(!(created_at %in% c("06", "22")))

# filter English language

israel_combined$language <- textcat(israel_combined$text)
israel_combined_eng <- israel_combined |>
  filter(language %in% c("english", "scots"))

# delete replicate rows

israel_combined_clean <- israel_combined_eng |>
  mutate(clean(israel_combined_eng$text)) |>
  rename("cleaned_text" = "clean(israel_combined_eng$text)") |>
  select(cleaned_text, created_at, total_interactions_count) |>
  distinct(cleaned_text, .keep_all = TRUE)

# count tweets per day and plot changes

israel_count <- israel_combined_clean |>
  distinct(cleaned_text, .keep_all = TRUE) |>
  group_by(created_at) |>
  count()

# safe combined dataset, remove unneeded dataframes

write.csv(israel_combined_clean,
          file = "output/israel_clean.csv",
          row.names = FALSE)
write.csv(israel_count,
          file = "output/israel_count.csv",
          row.names = FALSE)

rm(df, df_list, filtered_list, file, file_names, file_path)


# Hamas ----

# Load data
file_names <- c("Hamas_0607.csv",
                "Hamas_0708.csv",
                "Hamas_0809.csv",
                "Hamas_0910.csv",
                "Hamas_1011.csv",
                "Hamas_1112.csv",
                "Hamas_1213.csv",
                "Hamas_1314.csv",
                "Hamas_1415.csv",
                "Hamas_1516.csv",
                "Hamas_1617.csv",
                "Hamas_1718.csv",
                "Hamas_1819.csv",
                "Hamas_1920.csv",
                "Hamas_2021.csv",
                "Hamas_2122.csv")

df_list <- list() # create empty list to store data frame

for (file in file_names) {
  file_path <- paste0("data/Hamas/", file)
  df <- read.csv(file_path)
  df_list[[file]] <- df
}

filtered_list <- lapply(df_list, filter_twitter)

# combine datasets into a big one

hamas_combined <- bind_rows(filtered_list)

# arrange by date, get rid of Oct 06 and Oct 22

hamas_combined <- hamas_combined |>
  arrange(created_at) |>
  filter(!(created_at %in% c("06", "22")))

# filter English language

hamas_combined$language <- textcat(hamas_combined$text)
hamas_combined_eng <- hamas_combined |>
  filter(language %in% c("english", "scots"))

# delete replicate rows

hamas_combined_clean <- hamas_combined_eng |>
  mutate(clean(hamas_combined_eng$text)) |>
  rename("cleaned_text" = "clean(hamas_combined_eng$text)") |>
  select(cleaned_text, created_at, total_interactions_count) |>
  distinct(cleaned_text, .keep_all = TRUE)

# count tweets per day and plot changes

hamas_count <- hamas_combined_clean |>
  distinct(cleaned_text, .keep_all = TRUE) |>
  group_by(created_at) |>
  count()

# safe combined dataset, remove unneeded dataframes

write.csv(hamas_combined_clean,
          file = "output/hamas_clean.csv",
          row.names = FALSE)
write.csv(hamas_count,
          file = "output/hamas_count.csv",
          row.names = FALSE)

rm(df, df_list, filtered_list, file, file_names, file_path)


# Conflict ----

# Load data
file_names <- c("Conflict_0607.csv",
                "Conflict_0708.csv",
                "Conflict_0809.csv",
                "Conflict_0910.csv",
                "Conflict_1011.csv",
                "Conflict_1112.csv",
                "Conflict_1213.csv",
                "Conflict_1314.csv",
                "Conflict_1415.csv",
                "Conflict_1516.csv",
                "Conflict_1617.csv",
                "Conflict_1718.csv",
                "Conflict_1819.csv",
                "Conflict_1920.csv",
                "Conflict_2021.csv",
                "Conflict_2122.csv")

df_list <- list() # create empty list to store data frame

for (file in file_names) {
  file_path <- paste0("data/Conflict/", file)
  df <- read.csv(file_path)
  df_list[[file]] <- df
}

filtered_list <- lapply(df_list, filter_twitter)

# combine datasets into a big one

conflict_combined <- bind_rows(filtered_list)

# arrange by date, get rid of Oct 06 and Oct 22

conflict_combined <- conflict_combined |>
  arrange(created_at) |>
  filter(!(created_at %in% c("06", "22")))

# filter English language

conflict_combined$language <- textcat(conflict_combined$text)
conflict_combined_eng <- conflict_combined |>
  filter(language %in% c("english", "scots"))

# delete replicate rows

conflict_combined_clean <- conflict_combined_eng |>
  mutate(clean(conflict_combined_eng$text)) |>
  rename("cleaned_text" = "clean(conflict_combined_eng$text)") |>
  select(cleaned_text, created_at, total_interactions_count) |>
  distinct(cleaned_text, .keep_all = TRUE)

# count tweets per day and plot changes

conflict_count <- conflict_combined_clean |>
  distinct(cleaned_text, .keep_all = TRUE) |>
  group_by(created_at) |>
  count()

# safe combined dataset, remove unneeded dataframes

write.csv(conflict_combined_clean,
          file = "output/conflict_clean.csv",
          row.names = FALSE)
write.csv(conflict_count,
          file = "output/conflict_count.csv",
          row.names = FALSE)

rm(df, df_list, filtered_list, file, file_names, file_path)

# Gaza ----

# Load data
file_names <- c("gaza_0607.csv",
                "gaza_0708.csv",
                "gaza_0809.csv",
                "gaza_0910.csv",
                "gaza_1011.csv",
                "gaza_1112.csv",
                "gaza_1213.csv",
                "gaza_1314.csv",
                "gaza_1415.csv",
                "gaza_1516.csv",
                "gaza_1617.csv",
                "gaza_1718.csv",
                "gaza_1819.csv",
                "gaza_1920.csv",
                "gaza_2021.csv",
                "gaza_2122.csv")

df_list <- list() # create empty list to store data frame

for (file in file_names) {
  file_path <- paste0("data/gaza/", file)
  df <- read.csv(file_path)
  df_list[[file]] <- df
}

# for "gaza_1920", the total interaction count is recognized as characters
# need to change to integer

df_list[["gaza_1920.csv"]][["total_interactions_count"]] <-
  df_list[["gaza_1920.csv"]][["total_interactions_count"]] |>
  as.integer()

filtered_list <- lapply(df_list, filter_twitter)

# combine datasets into a big one

gaza_combined <- bind_rows(filtered_list)

# arrange by date, get rid of Oct 06 and Oct 22

gaza_combined <- gaza_combined |>
  arrange(created_at) |>
  filter(!(created_at %in% c("06", "22")))

# filter English language

gaza_combined$language <- textcat(gaza_combined$text)
gaza_combined_eng <- gaza_combined |>
  filter(language %in% c("english", "scots"))

# delete replicate rows

gaza_combined_clean <- gaza_combined_eng |>
  mutate(clean(gaza_combined_eng$text)) |>
  rename("cleaned_text" = "clean(gaza_combined_eng$text)") |>
  select(cleaned_text, created_at, total_interactions_count) |>
  distinct(cleaned_text, .keep_all = TRUE)

# count tweets per day and plot changes

gaza_count <- gaza_combined_clean |>
  distinct(cleaned_text, .keep_all = TRUE) |>
  group_by(created_at) |>
  count()

# safe combined dataset, remove unneeded dataframes

write.csv(gaza_combined_clean,
          file = "output/gaza_clean.csv",
          row.names = FALSE)
write.csv(gaza_count,
          file = "output/gaza_count.csv",
          row.names = FALSE)

rm(df, df_list, filtered_list, file, file_names, file_path)

# Palestine ----

# Load data
file_names <- c("Palestine_0607.csv",
                "Palestine_0708.csv",
                "Palestine_0809.csv",
                "Palestine_0910.csv",
                "Palestine_1011.csv",
                "Palestine_1112.csv",
                "Palestine_1213.csv",
                "Palestine_1314.csv",
                "Palestine_1415.csv",
                "Palestine_1516.csv",
                "Palestine_1617.csv",
                "Palestine_1718.csv",
                "Palestine_1819.csv",
                "Palestine_1920.csv",
                "Palestine_2021.csv",
                "Palestine_2122.csv")

df_list <- list() # create empty list to store data frame

for (file in file_names) {
  file_path <- paste0("data/palestine/", file)
  df <- read.csv(file_path)
  df_list[[file]] <- df
}

filtered_list <- lapply(df_list, filter_twitter)

# combine datasets into a big one

palestine_combined <- bind_rows(filtered_list)

# arrange by date, get rid of Oct 06 and Oct 22

palestine_combined <- palestine_combined |>
  arrange(created_at) |>
  filter(!(created_at %in% c("06", "22")))

# filter English language

palestine_combined$language <- textcat(palestine_combined$text)
palestine_combined_eng <- palestine_combined |>
  filter(language %in% c("english", "scots"))

# delete replicate rows

palestine_combined_clean <- palestine_combined_eng |>
  mutate(clean(palestine_combined_eng$text)) |>
  rename("cleaned_text" = "clean(palestine_combined_eng$text)") |>
  select(cleaned_text, created_at, total_interactions_count) |>
  distinct(cleaned_text, .keep_all = TRUE)

# count tweets per day and plot changes

palestine_count <- palestine_combined_clean |>
  distinct(cleaned_text, .keep_all = TRUE) |>
  group_by(created_at) |>
  count()

# safe combined dataset, remove unneeded dataframes

write.csv(palestine_combined_clean,
          file = "output/palestine_clean.csv",
          row.names = FALSE)
write.csv(palestine_count,
          file = "output/palestine_count.csv",
          row.names = FALSE)

rm(df, df_list, filtered_list, file, file_names, file_path)



# Combining five datasets ----

d_israel <- read.csv("output/israel_clean.csv")
d_conflict <- read.csv("output/conflict_clean.csv")
d_hamas <- read.csv("output/hamas_clean.csv")
d_gaza <- read.csv("output/gaza_clean.csv")
d_palestine <- read.csv("output/palestine_clean.csv")

d_combined <- bind_rows(d_israel, d_conflict, d_hamas, d_gaza, d_palestine)
d_combined <- d_combined |>
  distinct(cleaned_text, .keep_all = TRUE) |>
  arrange(created_at)
d_combined_count <- d_combined |>
  group_by(created_at) |>
  count()

write.csv(d_combined,
          file = "output/combined_clean.csv",
          row.names = FALSE)
write.csv(d_combined_count,
          file = "output/combined_count.csv",
          row.names = FALSE)