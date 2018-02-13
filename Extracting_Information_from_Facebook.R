## ------------------------------------------
## ------------------------------------------
#
# Getting Started
#
# You will need to run the code in the file Functions_and_Packages_to_Load_First.R
# before going through the code in this file
#
# Set the working directory where the dictionaries of positive and negative words are saved
#
#
setwd("E:\\MATH1608PP\\Tutorial 6 Facebook-20180205")
#
## ------------------------------------------
## ------------------------------------------
#
# You'll need a token from 
#
# https://developers.facebook.com/tools/explorer/
#
# You will need to set things up before you get your token
# There's more information about this available in the file Setting_up_Facebook.pdf
#
# Put your token here
#
token <- "EAACEdEose0cBALerxYqnaVvB23NM7C0PqrDa8ZA7HfAiCYNoBvaJnXbXjbJv73D3TiPSZCCMDO0Uq9EbP2OntYORdvNityYWrGy9MPgWPDgHk5YRGfp1ubV2ui4joZAbH0gq8JBm49ZAcsgYRVwPax3gYbb2bfPZAp016N5YmPkQ5ymMdvDKZBBZAxpuqzqjtMA5UNzcuvDZAgZDZD"
#
## ------------------------------------------
#
# Simple example to make sure that we're connected
#
page_name <- "xbox"
#
# Download the posts
# 
my_page <- getPage(page_name, token, n = 10) 
my_page
#
# Size of what's been downloaded by getPage(page_name, token, n = 10) 
#
dim(my_page)
#
# Variables available
#
names(my_page)
#
# We can understanding what we're working with by looking and tabulating type
#
my_page$type
#
table(my_page$type)
#
#
# adding the arguments reactions = TRUE and api = "v2.9" to getPage returns the total count of reactions: love, haha, wow, sad, angry
#
my_page_more <- getPage(page_name, token, n = 10, reactions = TRUE, api = "v2.9") 
#
dim(my_page_more)
#
names(my_page_more)
#
## ------------------------------------------
#
# Last year, unfortunately,  R sometimes crashed on the University system when asked to 
# download a lot of posts.  This did not happen on personal machines.
# To get around this, we developed code to download posts in *daily* blocks
# and to "glue" these blocks together
# Here is an example based on Plymouth Argyle, the local football team
#
page_name <- "xbox"
#
number_required <- 1000 # Maximum number to be downloaded each day (here, much greater than the number of daily posts)
#
# Date range: from 1st September 2017 (as an example) to the start of today
#
Sys.Date() # Today
#
dates <- seq(as.Date("2017/01/31"), as.Date("2018/01/31"), by = "day")
dates
#
n <- length(dates) - 1 # Number of dates minus 1 (i.e. number of daily blocks)
n
#
#
# Set up space to hold the results
#
df_daily <- list() # Inefficient as far as memory is concerned
#
# Download posts for each day
#
for (i in 1:n){
    #
    # Print out date, so that we can monitor progress
    #
    cat(as.character(dates[i]), " ") 
    #
    # Download posts for that day, but use try to avoid crash if there's an error
    #
    try(df_daily[[i]] <- getPage(page_name, token, 
                                 n = number_required, 
                                 since = dates[i], 
                                 until = dates[i + 1], 
                                 reactions = TRUE,
                                 api = "v2.9")) # Without this, code fails if reactions = TRUE
    # Setting the argument reactions to TRUE, i.e. reactions = TRUE,  returns  the total count of reactions: love, haha, wow, sad, angry
    #
    # Print a return character to move onto the next line
    #
    cat("\n")
}
#
# Bind all the results together
#
my_page <- do.call(rbind, df_daily)
#
## ------------------------------------------
#
# *** Now let's work with the results ***
#
# Size of what's been downloaded
#
dim(my_page)
#
# Variables available
#
names(my_page)
#
# Structure
#
str(my_page) # Can experience problems with messages that contain unusual characters
#
# Most recent post, i.e. last one in my_page
#
my_page[nrow(my_page), ]
##
#
# Examples
#
# Posts
#
my_page$message[nrow(my_page):(nrow(my_page) - 1)]
my_page$message[1:2] # 1st and 2nd September
#
# Counts: 
# likes
#
my_page$likes_count
#
# other counts
#
my_page$comments_count
my_page$shares_count
my_page$love_count
my_page$haha_count
my_page$wow_count
my_page$sad_count
my_page$angry_count
#
# Times
#
my_page$created_time[1:2]
#
#
## ------------------------------------------
## ------------------------------------------
#
# ***** Monitoring average counts across posts in each month *****
#
# Format the date and time
#
# Uses dplyr, loaded in Functions_and_Packages_to_Load_First.R 
# The function format.facebook.date is also there
#
my_page <- my_page %>% 
             mutate(datetime = format.facebook.date(created_time) )
#
head(my_page$datetime)
#
# Record the year and month
#
my_page <- my_page %>% 
             mutate(month = format(datetime, "%Y-%m"))
#
head(my_page$month)
#
# Monthly summaries
#
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
#
monthly_summaries_wide 
#
# Change into long form for plotting
#
# Uses tidyr, loaded in Functions_and_Packages_to_Load_First.R
#
monthly_summaries <- monthly_summaries_wide %>% 
                      gather("Monthly_Quantity","Number", 2:10) 
#
monthly_summaries
#
# Plots
#
# Uses ggplot2, loaded in Functions_and_Packages_to_Load_First.R
#
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
#
#
# Example of the use of paste, here to create the title
#
page_name
paste(page_name, "Facebook Page")
#
# Note that paste0 does not put a space
#
paste0(page_name, "Facebook Page") # Useful in some circumstances
#
#
## ------------------------------------------
## ------------------------------------------
#
# ***** Cleaning posts for word counts, word clouds and Sentiment Analysis *****
#
# This section uses many of the functions in Functions_and_Packages_to_Load_First.R
#
# Here are all the posts
#
my_page$message
#
# Some of the posts are NA.  We'll reduce the data set to remove data concerning these posts
#
my_page_existing_messages <- my_page %>% 
                              filter(!is.na(message)) # Only rows that contain real messages
#
# How many posts have we lost?
#
dim(my_page)
dim(my_page_existing_messages)
#
#
# Remove web addresses
#
my_page_existing_messages <- my_page_existing_messages %>% 
                              mutate(message_cleaned = remove_http_www(message))
#
# Remove non alpha-numeric characters
#
my_page_existing_messages <- my_page_existing_messages %>% 
                              mutate(message_cleaned = str_replace_all(message_cleaned, "[^[:alnum:]]",  " ") )
#
# Remove emoticons and similar characters (if they remain)
#
my_page_existing_messages <- my_page_existing_messages %>% 
                              mutate(message_cleaned = str_replace_all(message_cleaned, "[\u4E00-\u9FFF]+",  " ") )
my_page_existing_messages <- my_page_existing_messages %>% 
                              mutate(message_cleaned = str_replace_all(message_cleaned, "[\u3400-\u4DBF]+",  " ") )
#
# Remove numbers
#
my_page_existing_messages <- my_page_existing_messages %>% 
                              mutate(message_cleaned = remove.numbers(message_cleaned))
#
# Remove unnecessary spaces
#
my_page_existing_messages <- my_page_existing_messages %>% 
                              mutate(message_cleaned = remove.spaces(message_cleaned))
#
# Convert to lower case
#
my_page_existing_messages <- my_page_existing_messages %>% 
                              mutate(message_cleaned =  tolower(message_cleaned))
#
# Look at some results
#
my_page_existing_messages$message_cleaned
#
#
## ------------------------------------------
## ------------------------------------------
#
# Text mining to get * word counts *
#
# This needs the tm package, loaded in Functions_and_Packages_to_Load_First.R
#
# Put the posts into a Corpus
#
text_corpus <- Corpus(VectorSource(my_page_existing_messages$message_cleaned))
text_corpus
#
# Define stopwords that we don't want to include in our counts
#
new.stopwords <- c("http", "https", "youtube", "itunes", "vevo", 
                   "Home", "Park","Plymouth", "Argyle", 
                   "home", "park", "plymouth","argyle", "PAFC", 
                   "sat", "saturday", "tue", "wed", "thur") 
#
# Produce a Document Term Matrix
# This has words as rows and posts as columns
#
# We remove the above stopwords and standard English 
#
tdm <- TermDocumentMatrix(text_corpus, 
            control = list(stopwords = c(new.stopwords, 
                                         stopwords(kind = "en")))) # Standard English stop words
#
# Turn it into an actual matrix
#
tdm_matrix <- as.matrix(tdm)
# 
# Size
#
dim(tdm_matrix)
#
tdm_matrix[1:25, 1:25] # A lot of zeros
#
# The row sums give us the word counds
#
word_count <- rowSums(tdm_matrix)
head(word_count)
#
# Convert to a data frame
#
word_count_df <- data.frame(word = names(word_count), freq = word_count)
#
# Sort by decreasing order of frequency
#
word_count_ordered_df <- word_count_df %>% 
                          arrange(desc(freq))
#
head(word_count_ordered_df)
#
## ------------------------------------------
#
# Graphical displays
#
# Specify N to limit the plot to the N most popular words
#
N <- 8
#
words_freq_limited <- word_count_ordered_df[1:N,]
words_freq_limited
#
# Define word as factor so that the levels can be specified
# Otherwise, alphabetical order would be used
#
words_freq_limited <- words_freq_limited %>% 
                       mutate(word = factor(word, levels = word))

#
words_freq_limited$word
#
# Now for the plot
#
# This uses the ggplot2 package, installed above
#
ggplot(words_freq_limited, 
         aes(x = word, 
             y = freq)) +
    geom_col() + # This is the same as geom_bar(stat = "identity") + 
    # and plots the y values as they are; no tabulation is performed
    labs(title = paste("Most common words in post to page", page_name),
         x = paste("Top", N, "Words"), 
         y = "Frequency") + 
    theme(axis.text = element_text(size = 18, color = "black"), 
      axis.title = element_text(size = 18, color = "black"),
      title = element_text(size = 20))
#
#
# Plymouth Argyle colours
#
g_w_b <- rep(c("green", "white", "black"), length = N)
g_w_b
#
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
      title = element_text(size = 20),
      panel.background = element_rect(fill = 'darkgrey')) # Change the panel background
#
#
## ------------------------------------------
#
# *** Wordcloud ***, needs the wordcloud package, loaded in Functions_and_Packages_to_Load_First.R
#
# Let's remind ourselves about the contents of word_count_ordered_df
#
head(word_count_ordered_df)
#
# Now for the wordcloud
#
# Read the help file of wordcloud so that you can see what the arguments do
#
with(word_count_ordered_df, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 50,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2"), 
               scale = c(4.5, 0.5)))
#
# We are using the brewer colour palettes, as we like the colours
# We have discussed colour palettes briefly elsewhere
#
# Add a title
#
title(main = paste("Wordcloud for Facebook page", page_name), 
      cex.main = 2) # Controls the sizer of the title
#
# Again, note the use of paste
#
paste("Wordcloud for Facebook page", page_name)
#
# Alternative wordclouds: please see https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
#
library(wordcloud2)
#
wordcloud2(word_count_ordered_df)

letterCloud(word_count_ordered_df, word = "DAB") # May need running twice 
#
# Note that these appear in the Viewer and so are not ordinary plots
#
#
# ****************************************************
# ****************************************************
#
# ------------------------------------------
# ------------------------------------------
#
# Sentiment analysis
#
# Now read in the dictionaries, saved in the working directory, loaded above
#
pos <- scan("positive-words.txt", what = "character", comment.char = ";")
# We are reading in characters. All lines starting with ; are discarded
#
neg <- scan("negative-words.txt", what = "character", comment.char = ";")
#
## ------------------------------------------
#
#
# Count up the number of positive and negative words in all the posts (details are not important)
#
my_page_existing_messages_with_sentiments <- my_page_existing_messages %>% 
                                              rowwise() %>% # Needed because match_with_negation is not a vectorized function
                                              mutate(pos = match_with_negation(message_cleaned, pos), # Positive matches
                                                     neg = match_with_negation(message_cleaned, neg), # Negative matches
                                                     score = pos - neg) %>%
                                              ungroup() # Remove rowise grouping
#
my_page_existing_messages_with_sentiments %>% select(pos, neg, score) %>% head()
#
# Add a variable to indicate the page
# This is useful when we have data from more than one page
#
my_page_existing_messages_with_sentiments <- my_page_existing_messages_with_sentiments %>% 
                                               mutate(page_name = page_name)
#
my_page_existing_messages_with_sentiments %>% select(pos, neg, score, page_name) %>% head()
#
#
# Tabulate the scores
#
my_page_existing_messages_with_sentiments %>% count(score)
#
# Let's work out the mean score so that it can be added to the graph
#
# We'll include it as a line and as a numerical value
#
sentiment_means <- my_page_existing_messages_with_sentiments %>% 
                           summarize(mean_score = mean(score)) 
sentiment_means
#
# Barplot
#
ggplot(my_page_existing_messages_with_sentiments, 
        aes(x = score)) + # Sentiment score on x-axis
    geom_bar() + # geom_bar will do the tabulation for you :-)
    geom_vline(aes(xintercept = mean_score), data = sentiment_means) +
      # Add a vertical line at the mean score, calculated and stored in sentiment_mean above
    geom_text(aes(x = mean_score, 
                  y = Inf, 
                  label = signif(mean_score, 3)), # Show to three significant figures
                  vjust = 2, 
                  data = sentiment_means) + 
      # Add the mean as a number; vjust moves it down from the top of the plot
    scale_x_continuous(breaks = -10:15,  # Specify a suitable integer range for the x-axis
                       minor_breaks = NULL) + # Show integers; set this to a suitably large range
    labs(title = paste("Sentiments towards", page_name , "give a mean of", signif(sentiment_means$mean_score, 3)),
                  # Title that gives page name and mean sentiment score, to three significant figures
         x = "Sentiment Score (positive minus negative matches)" , 
         y = "Number of posts") 
#
#
# We can extend the above code to more than one page
# Here is an example
#
# ----------------------------------------------------------------------------------
#
page_name_2 <- "exetercityfc"
#
# Set up space to hold the results
#
df_daily_2 <- list() # Inefficient as far as memory is concerned
#
# Download posts for each day
#
for (i in 1:n){
  #
  # Print out date, so that we can monitor progress
  #
  cat(as.character(dates[i]), " ") 
  #
  # Download posts for that day, but use try to avoid crash if there's an error
  #
  try(df_daily_2[[i]] <- getPage(page_name_2, token, 
                               n = number_required, 
                               since = dates[i], 
                               until = dates[i + 1], 
                               reactions = TRUE,
                               api = "v2.9")) # Without this, code fails if reactions = TRUE
  # Setting the argument reactions to TRUE, i.e. reactions = TRUE,  returns  the total count of reactions: love, haha, wow, sad, angry
  #
  # Print a return character to move onto the next line
  #
  cat("\n")
}
#
# Bind all the results together
#
my_page_2 <- do.call(rbind, df_daily_2)
#
# Sort out the date, year and month
#
my_page_2 <- my_page_2 %>% 
  mutate(datetime = format.facebook.date(created_time))
#
# Record the year and month
#
my_page_2 <- my_page_2 %>% 
  mutate(month = format(datetime, "%Y-%m"))
#
#
# Cleaning
#
my_page_existing_messages_2 <- my_page_2 %>% filter(!is.na(message)) 
my_page_existing_messages_2 <- my_page_existing_messages_2 %>% mutate(message_cleaned = remove_http_www(message))
my_page_existing_messages_2 <- my_page_existing_messages_2 %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[^[:alnum:]]",  " ") )
my_page_existing_messages_2 <- my_page_existing_messages_2 %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[\u4E00-\u9FFF]+",  " ") )
my_page_existing_messages_2 <- my_page_existing_messages_2 %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[\u3400-\u4DBF]+",  " ") )
my_page_existing_messages_2 <- my_page_existing_messages_2 %>% mutate(message_cleaned = remove.numbers(message_cleaned))
my_page_existing_messages_2 <- my_page_existing_messages_2 %>% mutate(message_cleaned = remove.spaces(message_cleaned))
my_page_existing_messages_2 <- my_page_existing_messages_2 %>% mutate(message_cleaned =  tolower(message_cleaned))
#
# Sentiment analysis
#
my_page_existing_messages_with_sentiments_2 <- my_page_existing_messages_2 %>% 
  rowwise() %>% # Needed because match_with_negation is not a vectorized function
  mutate(pos = match_with_negation(message_cleaned, pos), # Positive matches
         neg = match_with_negation(message_cleaned, neg), # Negative matches
         score = pos - neg) %>%
  ungroup() # Remove rowise grouping
#
# Add a variable to indicate the page
#
my_page_existing_messages_with_sentiments_2 <- my_page_existing_messages_with_sentiments_2 %>% 
                                                 mutate(page_name = page_name_2)
#
#
# Put the sentiment information from both pages together
#
my_page_existing_messages_with_sentiments_both <- rbind(my_page_existing_messages_with_sentiments, 
                                                        my_page_existing_messages_with_sentiments_2) # rbind binds the rows of data frames
#

# Work out the means for each page
# so that these can be added to the graph for each page
# as a line and as a numerical value
#
sentiment_means_both <- my_page_existing_messages_with_sentiments_both %>% 
                         group_by(page_name) %>% 
                         summarize(mean_score = mean(score)) 
sentiment_means_both
#
# Perform the plot
#
ggplot(my_page_existing_messages_with_sentiments_both, 
       aes(x = score, # Sentiment score on x-axis
           fill = page_name)) + # Fill bars with a colour according to the page name
  geom_bar() + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), 
             data = sentiment_means_both) +
  # Add a vertical line at the mean scores, calculated and stored in sentiment_mean_both above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), 
            vjust = 2, 
            data = sentiment_means_both) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -15:15, 
                     minor_breaks = NULL) + # Show integers; set this to a suitably large range
  scale_fill_manual(values = c("exetercityfc" = "red", 
                               "plymouthargylefc" = "green")) + # Specify your own colours
  labs(x = "Sentiment Score (positive minus negative matches)" , 
       y = "Number of posts", 
       fill = "Page") +
  facet_grid(page_name ~ .) + # One row for each page
  theme(legend.position = "bottom") # Legend on the bottom
#
# ----------------------------------------------------------------------------------
#
#
#
#############################################################################
#############################################################################
#
# Dynamic Sentiment Analysis: 
# please see https://www.statslife.org.uk/politics/2889-what-information-can-we-extract-from-social-media-about-the-uk-s-eu-referendum
# for a topical example
#
# Let's see how sentiments change month by month
#
my_page_existing_messages_with_sentiments$month
#
#
# Average score for each month for first example (Plymouth Argyle)
#
average_monthly_score <- my_page_existing_messages_with_sentiments %>% 
                          group_by(month) %>% 
                          summarise(ave_score = mean(score), 
                                    sd_score = sd(score), 
                                    n_score = length(score))
#
average_monthly_score
#
# Turn month into a proper date, so that we can control how it appears on the x-axis
# The details are not important, except to say that we add a day to the month so that it's a full date
#
average_monthly_score <- average_monthly_score %>% 
                            mutate(month = ymd(paste0(month, "-01")))
#
average_monthly_score$month
#
# Plot them
#
ggplot(average_monthly_score,
       aes(x = month, y = ave_score)) + 
  geom_point(size = 5) + 
  geom_errorbar(aes(ymin = ave_score - 2*sd_score/sqrt(n_score), 
                    ymax = ave_score + 2*sd_score/sqrt(n_score)), 
                    width = 0.2) + 
  geom_hline(yintercept = 0, col = "black", size = 2) + # Line at 0 for neutrality
  labs(title = paste("Average Sentiment towards", page_name, "over time"),
       x = "Month", 
       y = "Average Sentiment Score") + 
  scale_x_date(labels = date_format("%B %Y"), # Control how the date are formatted
               date_breaks = "1 month", # Label the axis at every month
               minor_breaks = NULL) +
  theme(axis.text.x = element_text(size = 14, angle = 270, hjust = 0, vjust = 0.5), # For rotated labels
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 16)) 

#
#
# ---------------------------------------------------------------------------------
#
# Both pages
#
average_monthly_score_both <- my_page_existing_messages_with_sentiments_both %>% 
                                group_by(page_name, month) %>% 
                                summarise(ave_score = mean(score), 
                                sd_score = sd(score), 
                                n_score = length(score))
#
#
average_monthly_score_both <- average_monthly_score_both %>% 
                                mutate(month = ymd(paste0(month, "-01")))
#
#
ggplot(average_monthly_score_both,
       aes(x = month, y = ave_score, colour = page_name)) + 
  geom_point(size = 5) + 
  geom_errorbar(aes(ymin = ave_score - 2*sd_score/sqrt(n_score), 
                    ymax = ave_score + 2*sd_score/sqrt(n_score)), 
                width = 0.2) + 
  geom_hline(yintercept = 0, col = "black", size = 2) + # Line at 0 for neutrality
  labs(title = paste("Average Sentiment towards", page_name, "and", page_name_2, "over time"),
       x = "Month", 
       y = "Average Sentiment Score",
       colour = "Page Name") + 
  scale_x_date(labels = date_format("%B %Y"), # Control how the date are formatted
               date_breaks = "1 month", # Label the axis at every month
               minor_breaks = NULL) +
  scale_colour_manual(values = c("exetercityfc" = "red", 
                               "plymouthargylefc" = "green")) + # Specify your own colours
  theme(axis.text.x = element_text(size = 14, angle = 270, hjust = 0, vjust = 0.5), # For rotated labels
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 16),
        legend.position = "bottom") +
  facet_grid(. ~ page_name)
#
# ---------------------------------------------------------------------------------
#
#############################################################################
#############################################################################
#
#
# We can also get *** information from individual posts *** by using the getPost function
# This function needs to have the post id
#

my_page$message
#
# **** HERE WE HAVE TO MAKE SURE THAT THERE ARE POSTS THAT HAVE COMMENTS WHICH HAVE REPLIES.  
# The post "Gregg Wylde has joined Morecambe on loan for the rest of the season." is post 358, made recently on 30/1/2018
#
post_information <- getPost(my_page$id[358],  
                            token = token, 
                            n = 500, # Default value
                            reactions = TRUE,
                            api = "v2.9")
#
# This is a list of lists
#
names(post_information)
#
# Information about posts
#
post_information$post # Standard information about the post
#
# Information about reactions
#
post_information$reactions # The reactions and information about them
#
# Information about comments
#
post_information$comments # The comments and information about them
names(post_information$comments) 
post_information$comments$message
#
# There are *** replies *** to the 1st one.  We can *** dig deeper ***
#
replies_to_1st <- getCommentReplies(post_information$comments$id[1],
                  token = token,
                  n = 500,
                  replies = TRUE,
                  likes = TRUE, 
                  api = "v2.9")
#
replies_to_1st
replies_to_1st$replies$message
#
#
#############################################################################################
#############################################################################################
# ------------------------------------------
# ------------------------------------------
#
# Information about me
#
me <- getUsers("me", token, private_info = TRUE)
names(me)
me$name
me$gender
me$locale
me$hometown
#
# ------------------------------------------
#
#
# Information about friends who have given permission
#
# Who are my friends?
#
my_friends <- getFriends(token, simplify = TRUE)
my_friends
#
# Information about them
#
my_friends_info <- getUsers(my_friends$id, token, private_info = TRUE)
names(my_friends_info)
my_friends_info
#
# Some summaries
#
my_friends_info %>% count(gender)
#
# ------------------------------------------
#
# Who of my friends is friends with who?
#
friends_mat <- getNetwork(token, format = "adj.matrix")
friends_mat
friends_mat[1:4, 1:4]
#
# ------------------------------------------
# ------------------------------------------
#
# A network diagram
# Needs the package network, loaded in Functions_and_Packages_to_Load_First.R
#
# Extract just the initials
#
# The details are not important
#
my_friends_names <- my_friends_info$name
my_friends_separate_words <- str_split(my_friends_names, " ")
#
initials <- function(x){str_sub(x, 1, 1)}
#
my_friends_initials <- lapply(my_friends_separate_words, initials)
my_friends_initials <- sapply(my_friends_initials, paste, collapse = "")
my_friends_initials
#
# ------------------------------------------
#
# Turn the information in friends_mat into a a network
#
friends_network <- as.network(friends_mat)
#
# Define the vertex colours according to gender (red for female, blue for male)
#
vertex_colour <- with(my_friends_info, 
                      ifelse(gender == "female", "red", 
                             ifelse(gender == "male", "blue", gender)))
vertex_colour
#
# Now for the network plot
#
# Set up appropriate margins
#
p <- par(mar = c(3.1, 2.1, 2.1, 0.1))
#
plot(friends_network, label = my_friends_initials, vertex.col = vertex_colour, 
     label.cex = 1.5, vertex.cex = 1.5, arrowhead.cex = 1.5, 
     edge.lwd = 1.5, jitter = FALSE)
#
# Please see the help file for more details
#
# ?plot.network
#
# Restore graphical parameters
#
par(p)
#
#
#############################################################################################
##
##
## Working in practice on your presentations
##
## Downloading a lot of posts each time that you revise your presentation makes 
## the development process rather slow.
## One way around this is to work with a reduced number of posts (i.e. from a limited date range) 
## during development, and then run with the full number of posts when the presentation is finalized.
##
## Another approach is to:
##
## 1. download the posts into the R object my_page as described above;
##
## 2. write the R object my_page to an ASCII file in your working directory using 
# dput(my_page, "plymouth_argyle.txt") # For example
##
## 3. then, during the development phase of your presentation, 
## load the Facebook information from this file using
##
# my_page <- dget("plymouth_argyle.txt")
##
## and not directly from Facebook.
##
## 4. Finally, when the presentation is finalized, return to the usual way of downloading posts.
##
##
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#
# ***** Additional Material Related To Facebook for Self-Study *****
#
# ****** An Example of Some Deeper Analysis Based on the VolkswagenUK Page ******
# In particular, we'll work with the ***** comments **** to the post on 
# 30 August at 09:23 about the T-Roc.
#
# We suggest that you try to find this post on Facebook!
# This will help you to understand what's going on.
#
#
# Download post
#
VW_UK_30_31_08_2017 <- getPage("VolkswagenUK", token, 
                   n = number_required, # Defined above
                   since = "2017-08-30", 
                   until = "2017-08-31", # Cannot give the same date
                   reactions = TRUE,
                   api = "v2.9")
#
# Look at it
#
VW_UK_30_31_08_2017
#
# The id is
#
VW_UK_30_31_08_2017$id
#
# Let's *** dig deeper and download information associated with the post itself ***
#
T_Roc <- getPost(VW_UK_30_31_08_2017$id,
                 token = token, 
                 n = 500, 
                 reactions = TRUE,
                 api = "v2.9")

#
# Look at the post again
#
T_Roc$post
#
#
# Now the comments
#
T_Roc$comments
head(T_Roc$comments$message)
#
# Let's see the *** replies to the comment ***
# "Vw never again DSG gearbox blown at 60,000 no good will and a 3 grand bill,stay away from DSG folks,ticking timebomd"
#
# This is the first comment that we can see on Facebook itself
#
T_Roc$comments$message[10]
#
replies_1 <- getCommentReplies(T_Roc$comments$id[10],
                  token = token,
                  n = 500,
                  replies = TRUE,
                  likes = TRUE, 
                  api = "v2.9")

replies_1$replies
#
#
# --------------------------------------------
#
# Let's produce *** word counts and a wordcloud of the comments ***
#
T_Roc_comments <- T_Roc$comments
#
names(T_Roc_comments)
#
# Sort out the date, year and month
#
T_Roc_comments <- T_Roc_comments %>% 
  mutate(datetime = format.facebook.date(created_time))
#
# Record the year and month
#
T_Roc_comments <- T_Roc_comments %>% 
  mutate(month = format(datetime, "%Y-%m"))
#
#
# Cleaning
#
T_Roc_comments_existing_messages <- T_Roc_comments %>% filter(!is.na(message)) 
T_Roc_comments_existing_messages <- T_Roc_comments_existing_messages %>% mutate(message_cleaned = remove_http_www(message))
T_Roc_comments_existing_messages <- T_Roc_comments_existing_messages %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[^[:alnum:]]",  " ") )
T_Roc_comments_existing_messages <- T_Roc_comments_existing_messages %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[\u4E00-\u9FFF]+",  " ") )
T_Roc_comments_existing_messages <- T_Roc_comments_existing_messages %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[\u3400-\u4DBF]+",  " ") )
T_Roc_comments_existing_messages <- T_Roc_comments_existing_messages %>% mutate(message_cleaned = remove.numbers(message_cleaned))
T_Roc_comments_existing_messages <- T_Roc_comments_existing_messages %>% mutate(message_cleaned = remove.spaces(message_cleaned))
T_Roc_comments_existing_messages <- T_Roc_comments_existing_messages %>% mutate(message_cleaned =  tolower(message_cleaned))
#
# Look at some results
#
T_Roc_comments_existing_messages$message_cleaned[1:5]
#
# Put the posts into a Corpus
#
text_corpus <- Corpus(VectorSource(T_Roc_comments_existing_messages$message_cleaned))
text_corpus
#
# Define stopwords that we don't want to include in our counts
#
new.stopwords <- c("http", "https", "youtube", "itunes", "vevo") 
#
# Produce a Document Term Matrix
# This has words as rows and posts as columns
#
# We remove standard English and the above stopwords
#
tdm <- TermDocumentMatrix(text_corpus, 
                          control = list(stopwords = c(new.stopwords, stopwords(kind = "en")))) 
#
# Turn it into an actual matrix
#
tdm_matrix <- as.matrix(tdm)
#
# The row sums give us the word counds
#
word_count <- rowSums(tdm_matrix)
head(word_count)
#
# Store these in decreasing order
#
#
# Convert to a data frame
#
word_count_df <- data.frame(word = names(word_count), freq = word_count)
#
# Sort by decreasing order of frequency
#
word_count_ordered_df <- word_count_df %>% arrange(desc(freq))
#
## ------------------------------------------
#
# Graphical displays
#
# Specify N to limit the plot to the N most popular words
#
N <- 8
#
words_freq_limited <- word_count_ordered_df[1:N,]
words_freq_limited
#
# Define word as factor so that the levels can be specified
# Otherwise, alphabetical order would be used
#
words_freq_limited <- words_freq_limited %>% 
  mutate(word = factor(word, levels = word))

#
words_freq_limited$word
#
# Define word as factor so that the levels can be specified
# Otherwise, alphabetical order would be used
#
words_freq_limited <- words_freq_limited %>% mutate(word = factor(word, levels = word))
#
# Now for the plot
#
# This uses the ggplot2 package, installed above
#
ggplot(words_freq_limited, 
        aes(x = word, y = freq)) +
  geom_col() + # This is the same as geom_bar(stat = "identity") + 
  # and plots the y values as they are; no tabulation is performed
  labs(title = "Most common words in comments", 
       subtitle = "May not tell you much and\nmay need to have stopwords removed",
       x = paste("Top", N, "Words"), y = "Frequency") + 
  theme(axis.text = element_text(size = 18, color = "black"), 
        axis.title = element_text(size = 18, color = "black"),
        title = element_text(size = 20))
#
# Now for the wordcloud
#
# Read the help file of wordcloud so that you can see what the arguments do
#
with(word_count_ordered_df, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 50,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2"), 
               scale = c(4.5, 0.5)))
#
## ------------------------------------------
#
#
# *** Sentiment analysis of comments ***
#
T_Roc_comments_existing_messages_with_sentiments <- T_Roc_comments_existing_messages %>% 
  rowwise() %>% # Needed because match_with_negation is not a vectorized function
  mutate(pos = match_with_negation(message_cleaned, pos), # Positive matches
         neg = match_with_negation(message_cleaned, neg), # Negative matches
         score = pos - neg) %>%
  ungroup() # Remove rowise grouping

#
# Tabulate the scores
#
T_Roc_comments_existing_messages_with_sentiments %>% count(score)
#
#
# Let's work out the mean score so that it can be added to the graph
#
# We'll include it as a line and as a numerical value
#
sentiment_means <- T_Roc_comments_existing_messages_with_sentiments %>% summarize(mean_score = mean(score)) 
sentiment_means
#
# Barplot
#
ggplot(T_Roc_comments_existing_messages_with_sentiments, aes(x = score)) + # Sentiment score on x-axis
  geom_bar() + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_means) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_mean above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), 
                vjust = 2, 
                data = sentiment_means) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -10:10, minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments towards comments give a mean of", signif(sentiment_means$mean_score, 3)),
       x = "Sentiment Score (positive minus negative matches)" , 
       y = "Number of comments")
#
#
#
# ---------------------------------------------------------------
#
# Which is the *** most negative comment ***?
#
most_negative_number <- with(T_Roc_comments_existing_messages_with_sentiments, which.min(score))
most_negative_number
#
# Look at it (Yvonne Hart, 30th August 2017, not too far from top of comments)
#
T_Roc_comments_existing_messages_with_sentiments$message_cleaned[most_negative_number]
#
# What is its id?
#
most_negative_id <- T_Roc_comments_existing_messages_with_sentiments$id[most_negative_number]
most_negative_id
#
# Get *** replies ***
#
replies_to_most_negative <- getCommentReplies(most_negative_id,
                                    token = token,
                                    n = 500,
                                    replies = TRUE,
                                    likes = TRUE, 
                                    api = "v2.9")
#
negative_messages <- replies_to_most_negative$replies$message
negative_messages
#
# --------------------------------------------
#
# Let's produce *** word counts and a wordcloud of these replies ***
#
negative_replies <- replies_to_most_negative$replies
#
names(negative_replies)
#
# Sort out the date, year and month
#
negative_replies <- negative_replies %>% 
  mutate(datetime = format.facebook.date(created_time))
#
# Record the year and month
#
negative_replies <- negative_replies %>% 
  mutate(month = format(datetime, "%Y-%m"))
#
#
# Cleaning
#
negative_replies_existing_messages <- negative_replies %>% filter(!is.na(message)) 
negative_replies_existing_messages <- negative_replies_existing_messages %>% mutate(message_cleaned = remove_http_www(message))
negative_replies_existing_messages <- negative_replies_existing_messages %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[^[:alnum:]]",  " ") )
negative_replies_existing_messages <- negative_replies_existing_messages %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[\u4E00-\u9FFF]+",  " ") )
negative_replies_existing_messages <- negative_replies_existing_messages %>% mutate(message_cleaned = str_replace_all(message_cleaned, "[\u3400-\u4DBF]+",  " ") )
negative_replies_existing_messages <- negative_replies_existing_messages %>% mutate(message_cleaned = remove.numbers(message_cleaned))
negative_replies_existing_messages <- negative_replies_existing_messages %>% mutate(message_cleaned = remove.spaces(message_cleaned))
negative_replies_existing_messages <- negative_replies_existing_messages %>% mutate(message_cleaned =  tolower(message_cleaned))
#
# Look at some results
#
negative_replies_existing_messages$message_cleaned[1:5]
#
# Put the posts into a Corpus
#
text_corpus <- Corpus(VectorSource(negative_replies_existing_messages$message_cleaned))
text_corpus
#
# Define stopwords that we don't want to include in our counts
#
new.stopwords <- c("http", "https", "youtube", "itunes", "vevo") 
#
# Produce a Document Term Matrix
# This has words as rows and posts as columns
#
# We remove standard English and the above stopwords
#
tdm <- TermDocumentMatrix(text_corpus, 
                          control = list(stopwords = c(new.stopwords, stopwords(kind = "en")))) 
#
# Turn it into an actual matrix
#
tdm_matrix <- as.matrix(tdm)
#
# The row sums give us the word counds
#
word_count <- rowSums(tdm_matrix)
head(word_count)
#
#
# Convert to a data frame
#
word_count_df <- data.frame(word = names(word_count), freq = word_count)
#
# Sort by decreasing order of frequency
#
word_count_ordered_df <- word_count_df %>% arrange(desc(freq))
#
## ------------------------------------------
#
# Graphical displays
#
# Specify N to limit the plot to the N most popular words
#
N <- 8
#
words_freq_limited <- word_count_ordered_df[1:N,]
words_freq_limited
#
# Define word as factor so that the levels can be specified
# Otherwise, alphabetical order would be used
#
words_freq_limited <- words_freq_limited %>% 
                       mutate(word = factor(word, levels = word))

#
words_freq_limited$word
#
#
# Now for the plot
#
# This uses the ggplot2 package, installed above
#
ggplot(words_freq_limited, 
       aes(x = word, y = freq)) +
  geom_col() + # This is the same as geom_bar(stat = "identity") + 
  # and plots the y values as they are; no tabulation is performed
  labs(title = "Most common words in replies", 
       subtitle = "May not tell you much and\nmay need to have stopwords removed",
       x = paste("Top", N, "Words"), y = "Frequency") + 
  theme(axis.text = element_text(size = 18, color = "black"), 
        axis.title = element_text(size = 18, color = "black"),
        title = element_text(size = 20))
#
# Now for the wordcloud
#
# Read the help file of wordcloud so that you can see what the arguments do
#
with(word_count_ordered_df, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 50,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2"), 
               scale = c(4.5, 0.5)))
#
## ------------------------------------------
#
#
# *** Sentiment analysis of replies ***
#
negative_replies_existing_messages_with_sentiments <- negative_replies_existing_messages %>% 
  rowwise() %>% # Needed because match_with_negation is not a vectorized function
  mutate(pos = match_with_negation(message_cleaned, pos), # Positive matches
         neg = match_with_negation(message_cleaned, neg), # Negative matches
         score = pos - neg) %>%
  ungroup() # Remove rowise grouping

#
# Tabulate the scores
#
negative_replies_existing_messages_with_sentiments %>% count(score)
#
#
# Let's work out the mean score so that it can be added to the graph
#
# We'll include it as a line and as a numerical value
#
sentiment_means <- negative_replies_existing_messages_with_sentiments %>% summarize(mean_score = mean(score)) 
sentiment_means
#
# Barplot
#
ggplot(negative_replies_existing_messages_with_sentiments, aes(x = score)) + # Sentiment score on x-axis
  geom_bar() + # geom_bar will do the tabulation for you :-)
  geom_vline(aes(xintercept = mean_score), data = sentiment_means) +
  # Add a vertical line at the mean score, calculated and stored in sentiment_mean above
  geom_text(aes(x = mean_score, 
                y = Inf, 
                label = signif(mean_score, 3)), 
            vjust = 2, 
            data = sentiment_means) + 
  # Add the mean as a number; vjust moves it down from the top of the plot
  scale_x_continuous(breaks = -15:10, minor_breaks = NULL) + # Show integers; set this to a suitably large range
  labs(title = paste("Sentiments in negative replies give a mean of", signif(sentiment_means$mean_score, 3)),
       x = "Sentiment Score (positive minus negative matches)" , 
       y = "Number of comments")
#
#
#########################################################
#########################################################
#
# **** A little additional general material for self-study ****
# about bar plots and automatic tabulation
#
# A aside on geom_bar
#
# An example of the default behaviour of geom_bar:
#
words_freq_limited # Each word occurs once
#
ggplot(words_freq_limited, 
       aes(x = word)) +
  geom_bar() + 
  # One example of each word 
  # I.e. by default geom_bar tabulates the number of words
  # Here's there's just one occurrence of each word
  # This is useful behaviour if you want to avoid a tabulation step
  labs(title = paste("Most common words in post to page", page_name),
       x = paste("Top", N, "Words"), 
       y = "Frequency") + 
  theme(axis.text = element_text(size = 18, color = "black"), 
        axis.title = element_text(size = 18, color = "black"),
        title = element_text(size = 20))

#
# An example of the use of geom_bar for tabulation
#
# Some words of greeting
#
df <- data.frame(words = c("Hello", "Hello",  # 2 of them
                           "Ciao", "Ciao", "Ciao", # 3 of them
                           "Good Day!", "Good Day!", "Good Day!", "Good Day!")) # 4 of them
#
df %>% count(words)
#
# A bar plot with tabulated word counts
#
ggplot(df, 
       aes(x = words)) +
  geom_bar() + 
  # By default geom_bar tabulates the number of words
  # This is useful behaviour if you want to avoid a tabulation step
  labs(title = "geom_bar will tabulate",
       x = "Words", 
       y = "Frequency") + 
  theme(axis.text = element_text(size = 18, color = "black"), 
        axis.title = element_text(size = 18, color = "black"),
        title = element_text(size = 20))
#
# It's also possible to order the bars by frequency (details not important)
#
ggplot(df, 
       aes(x = reorder(words, words, FUN = function(x){-length(x)}))) +
  # We reorder the words according to the number of times that they occur (the length of each word subset) in decreasing order (-)
  geom_bar() + 
  labs(title = "geom_bar will tabulate",
       x = "Words", 
       y = "Frequency") + 
  theme(axis.text = element_text(size = 18, color = "black"), 
        axis.title = element_text(size = 18, color = "black"),
        title = element_text(size = 20))
#
#########################################################
#########################################################

