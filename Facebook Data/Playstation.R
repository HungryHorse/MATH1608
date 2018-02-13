#---Packages---
library(Rfacebook)
citation("Rfacebook")
library(stringr) # Character manipulation
library(dplyr) # Data wrangling
library(ggplot2) # Graphics
library(tidyr) # Turning wide data into long data
library(tm) # Text mining
library(wordcloud) # Word cloud
library(network) # Network diagram
library(curl) # Needed to get information about me and my friends
library(httr) # Needed to get information about me and my friends
library(lubridate) # For date manipulation
library(scales) # To format axes
library(wordcloud2)

#---
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, 
                     format = "%Y-%m-%dT%H:%M:%S+0000", 
                     tz = "GMT")
  date
}

#---
setwd("E:\\MATH1608PP\\Playstation")
token <- "EAACEdEose0cBAKkUBh8dRgVO8UnZA2cm1OlV43SXDMJdqEZAC324cXUh5xmRzTIn8qDHxp2HQzjZA4pZALZB75eSi27coPWoQ7YNJp5ZBe1PM7MsXD3hHTLfzzeDWNVzqGyZAZAJEjT0upoNAVM8lAl9jnshDiT1ZAGpqwXI8HzIC7ZALAuuPRXRa8mBvZBriRiPVlmuoiu3VRZChQZDZD"

page_name <- "PlayStation"
number_required <- 1000
dates <- seq(as.Date("2017/01/31"), as.Date("2018/01/31"), by = "day")
n <- length(dates) - 1
df_daily <- list()
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
my_page <- do.call(rbind, df_daily)

dim(my_page)
names(my_page)
str(my_page)

#Counts
my_page$likes_count
my_page$comments_count
my_page$shares_count
my_page$love_count
my_page$haha_count
my_page$wow_count
my_page$sad_count
my_page$angry_count

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

ggplot(monthly_summaries, 
       aes(x = month, 
           y = Number, 
           group = Monthly_Quantity, 
           colour = Monthly_Quantity)) +
  geom_line(lwd = 1.5) + 
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

#Clean up comments
my_page$message
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
my_page_existing_messages$message_cleaned

#Word Counts/Frequency
text_corpus <- Corpus(VectorSource(my_page_existing_messages$message_cleaned))
text_corpus

new.stopwords <- c("http", "https", "ps4", "playstation", "sony")

tdm <- TermDocumentMatrix(text_corpus, 
                          control = list(stopwords = c(new.stopwords, 
                                                       stopwords(kind = "en"))))
tdm_matrix <- as.matrix(tdm)
dim(tdm_matrix)
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


g_w_b <- rep(c("darkblue", "white", "black"), length = N)
g_w_b
ggplot(words_freq_limited, 
       aes(x = word, 
           y = freq)) +
  geom_col(fill = g_w_b, colour = "lightgreen", alpha = 0.9) + 
  # colour refers to the edge of the bars        
  # values of alpha less than 1 cause transparency
  labs(title = paste("Most common words in post to page", page_name),
       x = paste("Top", N, "Words"), 
       y = "Frequency") + 
  theme(axis.text = element_text(size = 18, color = "black"), 
        axis.title = element_text(size = 18, color = "black"),
        title = element_text(size = 20))
     
wordcloud2(word_count_ordered_df)
letterCloud(word_count_ordered_df, word = "PS4")


