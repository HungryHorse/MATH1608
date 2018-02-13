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

#---
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, 
                     format = "%Y-%m-%dT%H:%M:%S+0000", 
                     tz = "GMT")
  date
}

#---
setwd("C:\\Users\ahoughton-vowles\\Downloads\\Tutorial 6 Facebook-20180212")
token <- "EAACEdEose0cBAEKsxjPZA6bLXe38r29c8Jtov8B7istOywoI6QUHKE2dIqgRrniP0mLArA0bP2KVQIA4Yga5KkPYGbOkAxazOtgZBcGqjRgcYlI0KQ1wQD8TIDdfq7xD00aPytM9QGX6RDxvFDm2tzkzTVx2dvwBBKd3KcSo8G570xrVoNMMjulWoxPX3sIpqpgHrlaAZDZD"

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