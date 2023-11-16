# Title: Experimenting Text Mining Methods
# Contributor: Edrian
# Date: 2023-11-10
# Description:
# We did some very simple experiments using text mining method for sentiment
# analysis, and we think this method wouldn't work because there's too much
# nuance in the tweets (satire, double negation, etc).


# 1.1 loading data & identifying language ----

library(tidyverse)

d <- read.csv("data/Israel_2028_tm_experiment.csv", stringsAsFactors = FALSE)

library("textcat")

# I found this package through google which filters out language. It identifies
# English pretty accurately (low false-positive rate), but it leaves out a lot
# of English tweets as non-English (high false-negative rate).

d$language <- textcat(d$text)

d_english <- d |>
  filter(platform == "twitter") |>
  filter(language %in% c("english", "scots")) |> # scots english included
  select(text, created_at, language)

# for instance, lot of "middle_frisian" is English, but some are not
d_mf <- d |>
  filter(language == "middle_frisian") |>
  select(text, language)


# 1.2 cleaning data (removing unwanted things) ----

# I found this function on google when researching about sentiment analysis
# on twitter, which cleans unwanted elements in a tweet (emojis, @, links, etc.)

clean = function(text) {
  text = iconv(text, "latin1", "ASCII", sub="") #改變字的encoding
  text = gsub("(@)\\w+", "", text) #去除@或#後有數字,字母,底線 (標記人名或hashtag)
  text = gsub("(http|https)://.*", "", text) #去除網址(.:任意字元，*:0次以上)
  text = gsub("[ \t]{2,}", "", text) #去除兩個以上空格或tab
  text = gsub("\\n"," ",text) #去除換行
  text = gsub("\\s+"," ",text) #去除一個或多個空格(+:一次以上)
  text = gsub("^\\s+|\\s+$","",text) #去除開頭/結尾有一個或多個空格
  text = gsub("&.*;","",text) #去除html特殊字元編碼
  text = gsub("[^a-zA-Z0-9?!. ']","",text) #除了字母,數字空白?!.的都去掉(表情符號去掉)
  }


d_clean <- d_english |>
  mutate(clean(d_english$text)) |>
  rename("cleaned_text" = "clean(d_english$text)") |>
  select(cleaned_text, created_at)


# 1.3 sentiment analysis using sentimentr ----

# I tried out this package for sentiment analysis because it identifies not only
# positive and negative words, but also amplifiers, so it returns a more
# accurate sentiment prediction than other packages. However, the result
# is still extremely unreliable.

library(sentimentr)

d_attri <- sentiment_attributes(d_clean$cleaned_text)
d_terms <- extract_sentiment_terms(d_clean$cleaned_text)


d_sentiment <- d_clean |>
  mutate(sentiment_by(d_clean$cleaned_text))

d_positive <- d_sentiment |>
  filter(ave_sentiment > 0) |>
  select(cleaned_text, ave_sentiment)



# Below are some other things I experimented with but are not very useful / 
# relevant to this project.

# tokenizing & counting ----

library(tidytext)

d_token <- d_clean |>
  mutate(original_text = cleaned_text) |>
  unnest_tokens("word", cleaned_text)

d_token_top_words <- d_token |>
  anti_join(stop_words) |>
  count(word) |>
  arrange(desc(n))


# tokenization (machine learning) ----

library(tokenizers)
library(tidyverse)
library(tidytext)

# split by any characters not alphanumeric
d_en_tokenized <- strsplit(d_select_english$text,
                           "[^a-zA-Z0-9]+")
#transform list into dataframe
d_en_token_df <- as.data.frame(cbind(d_en_tokenized))
# issue: websites


# stop words ----
# remove not meaningful words like "a", "the"

d_en_token_df |>
  anti_join(get_stopwords(source = "snowball"))

d_en_token_df |>
  as_tibble()

# sentiment analysis using "bing" ----

library(textdata)
get_sentiments("bing")

d_sentiment <- d_clean |>
  inner_join(get_sentiments("bing")) |>
  count(original_text, sentiment)

# automated content analysis ----

library(tidyverse)
if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")}
if(!require("scales")) {install.packages("scales"); library("scales")}
if(!require("lubridate")) {install.packages("lubridate"); library("lubridate")}

get_sentiments("bing")
pos_words <- get_sentiments("bing") |>
  filter(sentiment == "positive")
neg_words <- get_sentiments("bing") |>
  filter(sentiment == "negative")

sentiment_dictionary <- dictionary(list(positive = pos_words,
                                        negative = neg_words))

str(sentiment_dictionary)

d_corp <- corpus(d_clean, text_field = "cleaned_text")

dfm_sentiment <- dfm(d_corp,
                     dictionary = sentiment_dictionary)
