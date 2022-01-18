library(rtweet)
library(glue)
library(tidyverse)
library(lubridate)
library(tidytext)
library(patchwork)

mon_friends <- get_friends("monjalexander")
mon_followers <- get_followers("monjalexander")

friends_followers <- bind_rows(mon_followers, mon_friends) %>% group_by(user_id) %>% slice(1)

tweets <- get_timeline_unlimited(friends_followers$user_id, n = 200)
wordles <- tweets %>% 
  filter(str_detect(text, "Wordle")) %>% 
  filter(str_detect(text, "[0-9,X]/6")) %>% 
  mutate(tries = str_extract(text, "[0-9,X]/6")) %>% 
  mutate(tries = str_sub(tries, 1,1)) %>% 
  select(user_id, created_at, text, tries)

# some time series

p1 <- wordles %>% 
  mutate(date = as.Date(created_at)) %>% 
  group_by(date) %>% 
  tally() %>% 
  filter(date> "2021-12-31") %>% 
  ggplot(aes(date, n)) + 
  geom_line(color = "#32a852", lwd = 1.2) + 
  geom_point(color = "#32a852", size = 2) + 
  labs(title = "People tweeting about Wordle",
       x = "number of tweets") + 
  theme_bw()


p2 <- wordles %>% 
  mutate(date = as.Date(created_at)) %>% 
  group_by(date) %>% 
  summarise(mean_tries = mean(as.numeric(tries), na.rm = TRUE)) %>%
  filter(date> "2021-12-31") %>% 
  ggplot(aes(date, mean_tries)) + 
  geom_line(color = "#32a852", lwd = 1.2) + 
  geom_point(color = "#32a852", size = 2) +
  labs(title = "Mean number of tries required",
       y = "mean",
       caption = "Based on Wordle tweets from @monjalexander's following and followers") + 
  theme_bw()

p1+p2
ggsave("wordle_stats.png")

### sentiment analysis

wordle_clean_text <- wordles %>% 
  mutate(tweet = row_number()) %>% 
  mutate(text = str_trim(str_replace_all(text, "@[A-Za-z0-9]+\\w+", ""))) %>% # remove twitter handles
  mutate(text = str_trim(str_replace_all(text, "#[A-Za-z0-9]+\\w+", ""))) %>% # remove hashtags
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z0-9]+\\w+", "")) %>% # remove websites
  mutate(text = str_replace_all(text, "\\w*[0-9]+\\w*\\s*", "")) %>% # remove numbers
  mutate(text = str_replace_all(text, "[^\x01-\x7F]+", "")) %>%  # remove non-english  characters
  mutate(text = str_replace_all(text, "Wordle .*/", "")) %>%  # remove non-english  characters
  mutate(text = str_trim(text))

# sum remaining?
wordle_clean_text %>% 
  mutate(not_empty = text!="") %>% 
  summarize(sum(not_empty))

tidy_tweets <- wordles %>% 
  mutate(tweet = row_number()) %>% 
  mutate(text = str_trim(str_replace_all(text, "@[A-Za-z0-9]+\\w+", ""))) %>% # remove twitter handles
  mutate(text = str_trim(str_replace_all(text, "#[A-Za-z0-9]+\\w+", ""))) %>% # remove hashtags
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z0-9]+\\w+", "")) %>% # remove websites
  mutate(text = str_replace_all(text, "\\w*[0-9]+\\w*\\s*", "")) %>% # remove numbers
  mutate(text = str_replace_all(text, "[^\x01-\x7F]+", "")) %>%  # remove non-english  characters
  mutate(text = str_replace_all(text, "Wordle .*/", "")) %>%  # remove non-english  characters
  mutate(text = str_trim(text)) %>% 
  select(created_at, tweet, tries, text) %>% 
  unnest_tokens(word, text)

bing <- get_sentiments("bing")

prop_positive_by_tries <- tidy_tweets %>% 
  inner_join(bing) %>% 
  count(tries, sentiment) %>% 
  arrange(tries, sentiment, -n ) %>% 
  group_by(tries) %>% 
  summarize(prop_positive_words = n[sentiment == "positive"]/sum(n))

prop_positive_by_tries %>% 
  ggplot(aes(tries, prop_positive_words)) + 
  geom_bar(stat = "identity", fill = "#32a852") + 
  labs(title = "Proportion of positive words used", 
       caption = "Based on Wordle tweets from @monjalexander's following and followers",
       x = "tries required",
       y = "proportion")+
  theme_bw()
ggsave("sentiment.png")

##### 

## HELPER FUNCTION from https://stackoverflow.com/questions/42025979/avoid-rate-limit-with-rtweet-get-timeline

get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

