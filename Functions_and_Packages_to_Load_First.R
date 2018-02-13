## ------------------------------------------
#
# This file contains some of the functions that 
# Julian Stander and Luciana Dalla Valle have written
# to work with text data coming from Facebook
#
# You do not need to understand all the details of these functions
#
# You do need to load these functions first, by running them in the usual way!
#
## ------------------------------------------
#
# Required packages
#
# Load the Rfacebook package
#
library(Rfacebook)
citation("Rfacebook")
#
# If this package is not available on your PC for example,
# you may have to install it:
#
# install.packages("Rfacebook", repos = "http://www.stats.bris.ac.uk/R/") 
#
# Other packages
# 
#
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
#
# Always give credit where it is due; use citation, as above
#
## ------------------------------------------
## ------------------------------------------
#
# Functions
#
## ------------------------------------------
#
# Formatting dates
#
format.facebook.date <- function(datestring) {
    date <- as.POSIXct(datestring, 
                       format = "%Y-%m-%dT%H:%M:%S+0000", 
                       tz = "GMT")
    date
}
#
## ------------------------------------------
#
# Cleaning text
#
# The stringr pacakge (loaded above) is needed as this provides many functions 
# for dealing with text strings
#
# Remove web addresses
#
remove_http_www <- function(some_txt){
    #
    some_txt <- str_replace_all(some_txt, 
                                "http\\:\\/\\/[A-Za-z0-9\\.\\/\\?\\=]+(\\s?)", "")
    #
    some_txt <- str_replace_all(some_txt, 
                                "https\\:\\/\\/[A-Za-z0-9\\.\\/\\?\\=]+(\\s?)", "")
    # 
    some_txt <- str_replace_all(some_txt, 
                                "www\\.[A-Za-z0-9\\.\\/\\?\\=]+(\\s?)", "")
    #
    return(some_txt)
}
#
## ------------------------------------------
#
# Remove numbers
#
remove.numbers <- function(some_txt){
    # Remove separate numbers
    some_txt <- str_replace_all(some_txt, "\\s\\d+", " ") #
    #
    # Remove numbers in brackets
    some_txt <- str_replace_all(some_txt, "\\(\\d+\\)", "") 
    #
    # Remove numbers at the beginning
    some_txt <- str_replace_all(some_txt, "^\\d+\\s", "")
    #
    # Remove numbers at the end
    some_txt <- str_replace_all(some_txt, "\\s\\d+$", "")
    #
    some_txt
}
#
## ------------------------------------------
#
# Remove spaces
#
remove.spaces <- function(some_txt){
    # If more than one space, replace by one space
    some_txt <- str_replace_all(some_txt, "\\s{2,}", " ") 
    #
    # Remove beginning and ending spaces
    some_txt <-  str_replace_all(some_txt, "^\\s+|\\s+$", "")
    #
    some_txt
}
#
## ------------------------------------------
#
# Word matching function for Sentiment Analysis
#
match_with_negation <- function(phrase, word_table, consider_negation = TRUE,
                                negation_dictionary = c("not", "isn't","aren't", "haven't","won't", "doesn't")){
    #
    # phrase is the phrase being considered
    # word_table is the dictionary of words in which we are looking for matches
    # consider_negation: if TRUE,
    # then the appearance of a negation word before a match is taken into account
    # negation_dictionary is a list of negation words (which we can add to)
    #
    # Obtain the individual words in phrase
    #
    individual_words <- unlist(str_split(phrase, '\\s+')) # Needs package stringr
    #
    # Remove the English articles as these can cause confusion when considering negation
    #
    individual_words <- individual_words[!individual_words %in% c("a", "the")]
    #
    # Where are the matches?
    # Position of the word in the word_table
    # If there is no match NA is reported
    #
    match_positions_in_table <- match(individual_words, word_table)
    #
    # So words that match are those that are not NA
    # Let's record which ones these are
    #
    match_positions_in_phrase <- which(!is.na(match_positions_in_table))
    #
    # How many matches?
    # This is the total number of elements that are not NA
    #
    number_of_matches <- sum(!is.na(match_positions_in_table))
    #
    # If we should take account of the appearance of a negation word before a match:
    #
    if(consider_negation){
        #
        # Position of preceding words
        #
        previous_words_position <- match_positions_in_phrase - 1
        #
        # Make sure that none of these are zero, as we can't index from zero
        #
        previous_words_position <- previous_words_position[previous_words_position > 0]
        #
        # What are those words?
        #
        previous_words <- individual_words[previous_words_position]
        #
        # Do they negate?
        #
        negation_words <- previous_words[previous_words %in% negation_dictionary]
        #
        # Remove the number of negation words from the number of matches
        #
        number_of_matches <- number_of_matches - length(negation_words)
        # So a phrase such as "not good" is recorded as neutral (score 0)
        # rather than 1 for the positive word "good"
    }
    #
    # Return the number of matches
    #
    number_of_matches
}
#
#
## ------------------------------------------
## ------------------------------------------