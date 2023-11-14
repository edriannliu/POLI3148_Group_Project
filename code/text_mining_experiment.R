# loading data & identifying language ----

library(tidyverse)

d <- read.csv("data/israel_1020_1028.csv", stringsAsFactors = FALSE)

library("textcat")

d$language <- textcat(d$text)

d_select_english <- d |>
  filter(platform == "twitter",
         language == "english") |>
  select(text, created_at, language)

# removing unwanted stuff

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


d_clean <- d_select_english |>
  mutate(clean(d_select_english$text)) |>
  rename("cleaned_text" = "clean(d_select_english$text)") |>
  select(cleaned_text, created_at)
  #text套用資料清理

# tokenizing

library(tidytext)

d_clean <- d_clean |>
  mutate(original_text = cleaned_text) |>
  unnest_tokens("word", cleaned_text)

# counting most frequent words

d_clean_top_words <- d_clean |>
  anti_join(stop_words) |>
  count(word) |>
  arrange(desc(n))

# sentiment analysis

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

# sentimentr ----

library(sentimentr)

d_c_d <- d_clean |>
  mutate(sentiment_by(d_clean$cleaned_text))

d_c_d_p <- d_c_d |>
  filter(ave_sentiment > 0.5) |>
  select(cleaned_text, ave_sentiment)

d_se_a <- sentiment_attributes(d_clean$cleaned_text)

d_sen <- sentiment(d_clean$cleaned_text)

extract_sentiment_terms(d_clean$cleaned_text)

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
