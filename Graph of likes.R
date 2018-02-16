token <- "EAACEdEose0cBAIbZA1e6qMiBiLrBRgG3SCJ4DUFAvyF7VjJQPXZCm9yUWcfBB2i9lOZAkwOKZByPjregFrSz6alPZASNFY7e6ibpIYG5lYSPVH18xmZCzKoxZBefflQNiTBbjIIde4J9raeOqCt75CzJcmr8VFlnqEJ7MDxlWLKeMJdqZALhzZCET7xUuaZCaZCx3gbxHgKibyn5gZDZD"
#
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

xboxPage <- do.call(rbind, df_daily)

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

steamPage <- do.call(rbind, df_daily)


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

playstationPage <- do.call(rbind, df_daily)


playlikes <- playstationPage$likes_count
xboxlikes <- xboxPage$likes_count
steamlikes <- steamPage$likes_count

playstationPage <- playstationPage %>% 
  mutate(datetime = format.facebook.date(created_time) )

playstationPage <- playstationPage%>% 
  mutate(month = format(datetime, "%Y-%m"))

playstationPage <- select(playstationPage, likes_count, month)
playstationPage

xboxPage <- xboxPage %>% 
  mutate(datetime = format.facebook.date(created_time) )

xboxPage <- xboxPage%>% 
  mutate(month = format(datetime, "%Y-%m"))

xboxPage <- select(xboxPage, likes_count, month)
xboxPage

steamPage <- steamPage %>% 
  mutate(datetime = format.facebook.date(created_time) )

steamPage <- steamPage%>% 
  mutate(month = format(datetime, "%Y-%m"))

steamPage <- select(steamPage, likes_count, month)
steamPage

steamPage$tag <- 1
xboxPage$tag <- 2
playstationPage$tag <- 3


factor_1 <- function(x){
  factor(x,
         levels = c(1,2,3), labels = c("Steam", "Xbox", "Playstation"))
}

steamPage <- collect(steamPage)
playstationPage <- collect(playstationPage)
xboxPage <- collect(xboxPage)

monthly_summaries_wide_steam <- steamPage %>%
  group_by(month) %>%
  summarize("Average likes" = mean(likes_count))

monthly_summaries_wide_xbox <- xboxPage %>%
  group_by(month) %>%
  summarize("Average likes" = mean(likes_count))

monthly_summaries_wide_playstation <- playstationPage %>%
  group_by(month) %>%
  summarize("Average likes" = mean(likes_count))

monthly_summaries_wide_steam$tag <- 1
monthly_summaries_wide_xbox$tag <- 2
monthly_summaries_wide_playstation$tag <- 3

monthly_summaries_wide_steam <- monthly_summaries_wide_steam %>% mutate_at(.vars = vars(3), .funs = funs(factor_1))
monthly_summaries_wide_playstation <- monthly_summaries_wide_playstation %>% mutate_at(.vars = vars(3), .funs = funs(factor_1))
monthly_summaries_wide_xbox <- monthly_summaries_wide_xbox %>% mutate_at(.vars = vars(3), .funs = funs(factor_1))

combined <- data.frame(monthly_summaries_wide_steam)
combined <- rbind.data.frame(monthly_summaries_wide_playstation, monthly_summaries_wide_xbox, monthly_summaries_wide_steam)
combined



ggplot(combined, aes(x = month, y = `Average likes`, col = tag, group = tag)) +
  geom_point() +
  geom_line() +
  labs(x = "Month", y = "Average number of likes")
