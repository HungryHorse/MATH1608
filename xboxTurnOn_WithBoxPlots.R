library(Rfacebook)
citation("Rfacebook")
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tm)
library(wordcloud)
library(network)
library(curl)
library(httr) 
library(lubridate)
library(scales)
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, 
                     format = "%Y-%m-%dT%H:%M:%S+0000", 
                     tz = "GMT")
  date
}
remove_http_www <- function(some_txt){
  some_txt <- str_replace_all(some_txt, 
                              "http\\:\\/\\/[A-Za-z0-9\\.\\/\\?\\=]+(\\s?)", "")
  some_txt <- str_replace_all(some_txt, 
                              "https\\:\\/\\/[A-Za-z0-9\\.\\/\\?\\=]+(\\s?)", "")
  some_txt <- str_replace_all(some_txt, 
                              "www\\.[A-Za-z0-9\\.\\/\\?\\=]+(\\s?)", "")
  return(some_txt)
}
remove.numbers <- function(some_txt){
  some_txt <- str_replace_all(some_txt, "\\s\\d+", " ") #
  some_txt <- str_replace_all(some_txt, "\\(\\d+\\)", "") 
  some_txt <- str_replace_all(some_txt, "^\\d+\\s", "")
  some_txt <- str_replace_all(some_txt, "\\s\\d+$", "")
  some_txt
}
remove.spaces <- function(some_txt){
  some_txt <- str_replace_all(some_txt, "\\s{2,}", " ") 
  some_txt <-  str_replace_all(some_txt, "^\\s+|\\s+$", "")
  some_txt
}
match_with_negation <- function(phrase, word_table, consider_negation = TRUE,
                                negation_dictionary = c("not", "isn't","aren't", "haven't","won't", "doesn't")){
  individual_words <- unlist(str_split(phrase, '\\s+'))
  individual_words <- individual_words[!individual_words %in% c("a", "the")]
  match_positions_in_table <- match(individual_words, word_table)
  match_positions_in_phrase <- which(!is.na(match_positions_in_table))
  number_of_matches <- sum(!is.na(match_positions_in_table))
  if(consider_negation){
    previous_words_position <- match_positions_in_phrase - 1
    previous_words_position <- previous_words_position[previous_words_position > 0]
    previous_words <- individual_words[previous_words_position]
    negation_words <- previous_words[previous_words %in% negation_dictionary]
    number_of_matches <- number_of_matches - length(negation_words)
  }

  number_of_matches
}
############################################################################ u wot m8
setwd("C:\\Users\\kmazur\\Downloads\\MATH1608-master\\MATH1608-master")
#
token <- "EAACEdEose0cBAGNnx9Y9azPch59nhuXVdjkHpTUVnZAsdIrWb1405MbDvPT4uon9q9nHU1DOOMyqMPcY8ZBijRsVYO5V19J7qc4UHH5AzIMNyF9P68hBlyRfAQFtJgehY41pR26Dww39tRT6iUnZAhgZAkJzkQvIyD5ZCQgVZAdZBfHKiZBprXl8rDbew3xAWeyzI3xyiKU86gZDZD"
#------------------------------
# xbob
page_name <- "xbox"
number_required <- 1000
Sys.Date()
dates <- seq(as.Date("2017/01/31"), as.Date("2018/01/31"), by = "day")
n <- length(dates) - 1
df_daily <- list()
#
for (i in 1:n){
  cat(as.character(dates[i]), " ") 
  try(df_daily[[i]] <- getPage(page_name, token, 
                               n = number_required, 
                               since = dates[i], 
                               until = dates[i + 1], 
                               reactions = TRUE,
                               api = "v2.9"))
  cat("\n")
}
#
my_page <- do.call(rbind, df_daily)
#---------------------------
# steam
page_name <- "Steam"
number_required <- 1000
Sys.Date()
dates <- seq(as.Date("2017/01/31"), as.Date("2018/01/31"), by = "day")
n <- length(dates) - 1
df_daily_steam <- list()
#
for (i in 1:n){
  cat(as.character(dates[i]), " ") 
  try(df_daily_steam[[i]] <- getPage(page_name, token, 
                               n = number_required, 
                               since = dates[i], 
                               until = dates[i + 1], 
                               reactions = TRUE,
                               api = "v2.9"))
  cat("\n")
}
#
my_page_steam <- do.call(rbind, df_daily_steam)
#--------------------------
# playstation
page_name <- "PlayStation"
number_required <- 1000
Sys.Date()
dates <- seq(as.Date("2017/01/31"), as.Date("2018/01/31"), by = "day")
n <- length(dates) - 1
df_daily_ps<- list()
#
for (i in 1:n){
  cat(as.character(dates[i]), " ") 
  try(df_daily_ps[[i]] <- getPage(page_name, token, 
                               n = number_required, 
                               since = dates[i], 
                               until = dates[i + 1], 
                               reactions = TRUE,
                               api = "v2.9"))
  cat("\n")
}
#
my_page_ps <- do.call(rbind, df_daily_ps)
#--------------------------
#fb
my_page$likes_count
my_page$comments_count
my_page$shares_count
my_page$love_count
my_page$haha_count
my_page$wow_count
my_page$sad_count
my_page$angry_count
#
#------------------------------------
#xbox
my_page <- my_page %>% 
  mutate(datetime = format.facebook.date(created_time) )
head(my_page$datetime)
my_page <- my_page %>% 
  mutate(month = format(datetime, "%Y-%m"))
head(my_page$month)
monthly_summaries_wide <- my_page %>%
  group_by(month) %>%
  summarize("Average likes" = mean(likes_count),
            "Average comments" = mean(comments_count),
            "Average shares" = mean(shares_count),
            "Average love" = mean(love_count),
            "Average haha" = mean(haha_count),
            "Average wow" = mean(wow_count),
            "Average sad" = mean(sad_count), 
            "Average angry" = mean(angry_count),
            "Number of posts" = n())
monthly_summaries_wide 
monthly_summaries <- monthly_summaries_wide %>% 
  gather("Monthly_Quantity","Number", 2:10) 
monthly_summaries
#
#------------------
# steam
#
my_page_steam <- my_page_steam %>% 
  mutate(datetime = format.facebook.date(created_time) )
head(my_page_steam$datetime)
my_page_steam <- my_page_steam %>% 
  mutate(month = format(datetime, "%Y-%m"))
head(my_page_steam$month)
monthly_summaries_steam_wide <- my_page_steam %>%
  group_by(month) %>%
  summarize("Average likes" = mean(likes_count),
            "Average comments" = mean(comments_count),
            "Average shares" = mean(shares_count),
            "Average love" = mean(love_count),
            "Average haha" = mean(haha_count),
            "Average wow" = mean(wow_count),
            "Average sad" = mean(sad_count), 
            "Average angry" = mean(angry_count),
            "Number of posts" = n())
monthly_summaries_steam_wide 
#
#------------------
# playstation
#
my_page_ps <- my_page_ps %>% 
  mutate(datetime = format.facebook.date(created_time) )
head(my_page_ps$datetime)
my_page_ps <- my_page_ps %>% 
  mutate(month = format(datetime, "%Y-%m"))
head(my_page_ps$month)
monthly_summaries_ps_wide <- my_page_ps %>%
  group_by(month) %>%
  summarize("Average likes" = mean(likes_count),
            "Average comments" = mean(comments_count),
            "Average shares" = mean(shares_count),
            "Average love" = mean(love_count),
            "Average haha" = mean(haha_count),
            "Average wow" = mean(wow_count),
            "Average sad" = mean(sad_count), 
            "Average angry" = mean(angry_count),
            "Number of posts" = n())
monthly_summaries_ps_wide 
#
# Plots
#-------------------
#box plots
abc <- c(monthly_summaries_wide$`Average comments`, monthly_summaries_steam_wide$`Average comments`, monthly_summaries_ps_wide$`Average comments`)


namess <- c("xbox", "steam", "playstation")
namess_f <- factor(namess)

df <- data.frame(abc, namess_f)
df

ggplot(df , aes(x = namess_f, y = abc)) +
  geom_boxplot() +
  labs(x = "Company Name", y = "Number of Comments")

#-------------------
# Monthly Quantity
ggplot(monthly_summaries, 
       aes(x = month, 
           y = Number, 
           group = Monthly_Quantity, 
           colour = Monthly_Quantity)) +
  geom_line(lwd = 1.5) + # Specify line width 
  facet_wrap(~ Monthly_Quantity, scales = "free_y") + 
  theme(axis.text.x = element_text(size = 14, angle = 270, hjust = 0, vjust = 0.5), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16, angle = 270),
        strip.text.x = element_text(size = 14),
        title = element_text(size = 20), 
        legend.position = "none") + 
  labs(title = paste(page_name, "Facebook Page"), 
       subtitle = "Monthly Quantity Across Posts",
       x = "Month", 
       y = "Number") 
#----------------------
my_page_existing_messages <- my_page %>% 
  filter(!is.na(message))
my_page_existing_messages <- my_page_existing_messages %>% 
  mutate(message_cleaned = remove_http_www(message))
my_page_existing_messages <- my_page_existing_messages %>% 
  mutate(message_cleaned = str_replace_all(message_cleaned, "[^[:alnum:]]",  " ") )
my_page_existing_messages <- my_page_existing_messages %>% 
  mutate(message_cleaned = str_replace_all(message_cleaned, "[\u4E00-\u9FFF]+",  " ") )
my_page_existing_messages <- my_page_existing_messages %>% 
  mutate(message_cleaned = str_replace_all(message_cleaned, "[\u3400-\u4DBF]+",  " ") )
my_page_existing_messages <- my_page_existing_messages %>% 
  mutate(message_cleaned = remove.numbers(message_cleaned))
my_page_existing_messages <- my_page_existing_messages %>% 
  mutate(message_cleaned = remove.spaces(message_cleaned))
my_page_existing_messages <- my_page_existing_messages %>% 
  mutate(message_cleaned =  tolower(message_cleaned))
text_corpus <- Corpus(VectorSource(my_page_existing_messages$message_cleaned))
new.stopwords <- c("http", "https", "youtube", "xbox", "microsoft") 
tdm <- TermDocumentMatrix(text_corpus, 
                          control = list(stopwords = c(new.stopwords, 
                                                       stopwords(kind = "en"))))
tdm_matrix <- as.matrix(tdm)
tdm_matrix[1:25, 1:25]
word_count <- rowSums(tdm_matrix)
head(word_count)
word_count_df <- data.frame(word = names(word_count), freq = word_count)
word_count_ordered_df <- word_count_df %>% 
  arrange(desc(freq))
head(word_count_ordered_df)
N <- 8
words_freq_limited <- word_count_ordered_df[1:N,]
words_freq_limited <- words_freq_limited %>% 
  mutate(word = factor(word, levels = word))
words_freq_limited$word
#plots
#-----------------------------
#
ggplot(words_freq_limited, 
       aes(x = word, 
           y = freq)) +
  geom_col() + 
  labs(title = paste("Most common words in post to page", page_name),
       x = paste("Top", N, "Words"), 
       y = "Frequency") + 
  theme(axis.text = element_text(size = 18, color = "black"), 
        axis.title = element_text(size = 18, color = "black"),
        title = element_text(size = 20))
#
#----------------------------
# word clound
library(wordcloud2)
wordcloud2(word_count_ordered_df)
letterCloud(word_count_ordered_df, word = "DAB")
#---------------------
pos <- scan("positive-words.txt", what = "character", comment.char = ";")
neg <- scan("negative-words.txt", what = "character", comment.char = ";")
my_page_existing_messages_with_sentiments <- my_page_existing_messages %>% 
  rowwise() %>% 
  mutate(pos = match_with_negation(message_cleaned, pos), # Positive matches
         neg = match_with_negation(message_cleaned, neg), # Negative matches
         score = pos - neg) %>%
  ungroup()
my_page_existing_messages_with_sentiments %>% select(pos, neg, score) %>% head()
my_page_existing_messages_with_sentiments <- my_page_existing_messages_with_sentiments %>% 
  mutate(page_name = page_name)
my_page_existing_messages_with_sentiments %>% select(pos, neg, score, page_name) %>% head()
my_page_existing_messages_with_sentiments %>% count(score)
sentiment_means <- my_page_existing_messages_with_sentiments %>% 
  summarize(mean_score = mean(score)) 
#plots
#-----------------------------------
#
ggplot(my_page_existing_messages_with_sentiments, 
       aes(x = score)) +
  geom_bar() + 
  geom_vline(aes(xintercept = mean_score), data = sentiment_means) +
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)),
            vjust = 2, 
            data = sentiment_means) + 
  scale_x_continuous(breaks = -10:15,  
                     minor_breaks = NULL) + 
  labs(title = paste("Sentiments towards", page_name , "give a mean of", signif(sentiment_means$mean_score, 3)),
       x = "Sentiment Score (positive minus negative matches)" , 
       y = "Number of posts") 
#-------------------------------------

