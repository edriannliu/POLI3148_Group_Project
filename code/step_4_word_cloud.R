# Title: text mining and word cloud generation
# Contributor: Edrian
# Date: 12/4/2023

library(tidytext)
library(SnowballC)
library(ggwordcloud)

data("stop_words")

# israel ----

d_is <- d |> filter(sentiment == 1)

d_token_is = d_is |>
  select(id_of_tweet, cleaned_text.x, sentiment) |>
  unnest_tokens(word, cleaned_text.x) |>
  anti_join(stop_words, by = "word") |>
  mutate(stem = wordStem(word))
 
word_freq_is = d_token_is |>
  count(stem, sort = TRUE) |>
  rename("word" = "stem")

word_freq_top_is = word_freq_is |>
  arrange(desc(n)) |>
  slice(1:200) |>
  filter(!(word %in% c("israel", "hama", "conflict", "gaza", "palestin")))

word_freq_top_is |>
  slice(1:100) |>
  ggplot(aes(label = word, size = n)) +
  scale_size_area(max_size = 14) +
  geom_text_wordcloud() +
  theme_minimal()


# palestine ----

d_pa <- d |> filter(sentiment == -1)

d_token_pa = d_pa |>
  select(id_of_tweet, cleaned_text.x, sentiment) |>
  unnest_tokens(word, cleaned_text.x) |>
  anti_join(stop_words, by = "word") |>
  mutate(stem = wordStem(word))

word_freq_pa = d_token_pa |>
  count(stem, sort = TRUE) |>
  rename("word" = "stem")

word_freq_top_pa = word_freq_pa |>
  arrange(desc(n)) |>
  slice(1:200) |>
  filter(!(word %in% c("israel", "hama", "conflict", "gaza", "palestin")))

word_freq_top_pa |>
  slice(1:100) |>
  ggplot(aes(label = word, size = n)) +
  scale_size_area(max_size = 14) +
  geom_text_wordcloud() +
  theme_minimal()

# neutral ----

d_nu <- d |> filter(sentiment == 0)

d_token_nu = d_nu |>
  select(id_of_tweet, cleaned_text.x, sentiment) |>
  unnest_tokens(word, cleaned_text.x) |>
  anti_join(stop_words, by = "word") |>
  mutate(stem = wordStem(word))

word_freq_nu = d_token_nu |>
  count(stem, sort = TRUE) |>
  rename("word" = "stem")

word_freq_top_nu = word_freq_nu |>
  arrange(desc(n)) |>
  slice(1:200) |>
  filter(!(word %in% c("israel", "hama", "conflict", "gaza", "palestin")))

word_freq_top_nu |>
  slice(1:100) |>
  ggplot(aes(label = word, size = n)) +
  scale_size_area(max_size = 14) +
  geom_text_wordcloud() +
  theme_minimal()


# filtered ----

d_fi <- d[is.na(d$sentiment), ]

d_token_fi = d_fi |>
  select(id_of_tweet, cleaned_text.x, sentiment) |>
  unnest_tokens(word, cleaned_text.x) |>
  anti_join(stop_words, by = "word") |>
  mutate(stem = wordStem(word))

word_freq_fi = d_token_fi |>
  count(stem, sort = TRUE) |>
  rename("word" = "stem")

word_freq_top_fi = word_freq_fi |>
  arrange(desc(n)) |>
  slice(1:200) |>
  filter(!(word %in% c("israel", "hama", "conflict", "gaza", "palestin")))

word_freq_top_fi |>
  slice(1:100) |>
  ggplot(aes(label = word, size = n)) +
  scale_size_area(max_size = 14) +
  geom_text_wordcloud() +
  theme_minimal()
