library(tidyverse)
d_hamas_0721 <- read_csv("data/Hamas_0721_v3.csv")

d_hamas_0721_count <- d_hamas_0721 |>
  group_by(sentiment) |>
  count()
print(d_hamas_0721_count)

