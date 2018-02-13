setwd("E:\\A1-Math Uni\\Facebook Data")


token <- "EAACEdEose0cBAGaFYxtwZAqnuvFBluyaevbRtHnTVfrwAwEKhEavHNO7SUaZAOh2j1dCXfwL3a0zhT6v7ZCJrs6gtZCErJNFlAN8EmF9PmMrtffg8R3cjFWWPlZBoCiXqOL3XhZBuEG0mP2DqchfNgJREnhyDiBdp01BAh3TUYsrY54cb9oXOGZCzCAsAVXGA88PS0X0uh9LgZDZD"

page_name <- "Steam"

number_required <- 1000 

dateToUse <- "2018/01/31"

dates <- seq(as.Date("2017/01/31"), as.Date(dateToUse), by = "day")
dates

n <- length(dates) - 1
n

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

my_page <- my_page %>% 
  mutate(datetime = format.facebook.date(created_time) )

head(my_page$datetime)

my_page <- my_page %>% 
  mutate(month = format(datetime, "%Y-%m"))

head(my_page$month)
my_page$likes_count
my_page$comments_count
my_page$shares_count
my_page$love_count
my_page$haha_count
my_page$wow_count
my_page$sad_count
my_page$angry_count

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

monthly_summaries <- monthly_summaries_wide %>% 
  gather("Monthly_Quantity","Number", 2:10) 

ggplot(monthly_summaries, 
       aes(x = month, 
           y = Number, 
           group = Monthly_Quantity, 
           colour = Monthly_Quantity)) +
  geom_line(lwd = 1.5) + # Specify line width 
  facet_wrap(~ Monthly_Quantity, scales = "free_y") + # Use a free scale on the y-axis
  theme(axis.text.x = element_text(size = 14, angle = 270, hjust = 0, vjust = 0.5), # For rotated labels
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16, angle = 270),
        strip.text.x = element_text(size = 14),
        title = element_text(size = 20), 
        legend.position = "none") + 
  labs(title = paste(page_name, "Facebook Page"), # Note the use of paste to create a title of the page name followed by some text (see below)
       subtitle = "Monthly Quantity Across Posts",
       x = "Month", 
       y = "Number") 

