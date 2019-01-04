
library(tidyverse)
library(tidytext)
library(extrafont)

font_import()
loadfonts(device = "win")

#reduce size of dataset

# tweets_raw <- read_rds("data/rstats_tweets.rds")
# 
# tweets_raw %>%
#   filter(lang == "en") %>%
#   select("status_id", "created_at", "text", "source") %>%
#   write_rds("data/tweets_raw2.rds", "gz")


tweets_raw2 <- read_rds("data/tweets_raw2.rds")

#View some summary stats
# summary(tweets_raw2)
# skimr::skim(tweets_raw2)
# glimpse(tweets_raw2)

#Let's see the top words. Code from https://github.com/jasonbaik94/rstats-2019-goals/find/master

# Get rid of all non-ASCII characters
# Get rid of 2019, #rstats, goals
# Get rid of \n
# Get rid of periods, numbers, https (urls), amp, tco

tweets <- tweets_raw2 %>% 
  mutate(text = str_replace_all(text, "[^\x01-\x7F]", ""),
         text = str_replace_all(text, "2019|goals|#rstats|#Rstats|#RStats", ""),
         text = str_replace_all(text, "\n", ""),
         text = str_replace_all(text, "\\.|[[:digit:]]+", ""),
         text = str_replace_all(text, "http|rt|Http|Rt|https|amp|tco", ""))

#just checking top 20 words
tweets %>%  
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>%
  head(20)
 
#filter for passion words
tweets %>%  
  unnest_tokens(word, text) %>% 
  filter(word %in% c("love", "Love", "hate", "Hate")) %>% 
  count(word, sort = TRUE)

#created Date-formatted dates
tweets$created_at <-  as.Date(tweets$created_at, 'GMT') 

#cumulative sum of passion words
passion_tweets <- tweets %>%  
  unnest_tokens(word, text) %>% 
  filter(word %in% c("love", "Love", "hate", "Hate")) %>% 
  arrange(created_at) %>% 
  mutate(hate_count = ifelse(word == "hate", 1, 0)) %>% 
  mutate(love_count = ifelse(word == "love", 1, 0)) %>% 
  mutate(hate = cumsum(hate_count)) %>% 
  mutate(love = cumsum(love_count)) %>% 
  gather(hate:love, key = passion_word, value = total_tweets)


ggplot(passion_tweets) + 
  geom_line(aes(created_at, total_tweets, color = passion_word), size = 2) +
  labs(title = "Come for the #Rstats, stay for the Love",
       subtitle = "Cumulative sum of words 'love' and 'hate' in #rstats tweets",
       x = "",
       y = "",
       caption = "TidyTuesday 2019-01-01\n Plot: @oscar_b123 \n Data: rwteet") +
  ylim(0, 5000) +
  geom_text(data = subset(passion_tweets, created_at == max(created_at)), 
            aes(x = max(created_at), y = total_tweets, label = passion_word,
                colour = passion_word), size = 6, hjust = 1, vjust = -0.2) +
  scale_colour_manual(values = c("#000000", "#e00fc8"))+
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Bahnschrift"),
        plot.background = element_rect(fill='#f5d59a', colour = "#f5d59a"),
        panel.background = element_rect(fill='#f5d59a', colour = "#f5d59a"),
        panel.grid.major = element_line(colour = "#f4ece1"),
        panel.grid.minor = element_line(colour = "#f4ece1"))
  

 


  
