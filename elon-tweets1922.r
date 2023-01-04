if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(tidyr)
library(scales)

dl <- tempfile()
download.file("https://raw.githubusercontent.com/jh-ronald/Elon-Musk/master/Elon%202019-2020.csv",
              destfile = dl)
elon_tweets_1920 <- read_csv(dl)

dl <- tempfile()
download.file("https://raw.githubusercontent.com/jh-ronald/Elon-Musk/master/Elon%202021-2022.csv",
              destfile = dl)
elon_tweets_2122 <- read_csv(dl)

t_1922 <- bind_rows(elon_tweets_1920 %>%
                      mutate(person = "EM1920"),
                    elon_tweets_2122 %>%
                      mutate(person = "EM2122"))

ggplot(t_1922, aes(x = Date, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)

remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets_1922 <- t_1922 %>%
  mutate(Tweet = str_remove_all(Tweet, remove_reg)) %>%
  unnest_tokens(word, Tweet) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

frequency <- tidy_tweets_1922 %>%
  count(person, word, sort = TRUE) %>%
  left_join(tidy_tweets_1922 %>%
              count(person, name = "total")) %>%
  mutate(freq = n / total)

frequency <- frequency %>%
  select(person, word, freq) %>%
  pivot_wider(names_from = person, values_from = freq) %>%
  arrange(EM1920, EM2122)

ggplot(frequency, aes(EM1920, EM2122)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")