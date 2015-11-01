#####
# LIBRARIES
#####
library(plyr)
library(XML)
library(rvest)
library(dplyr)
library(RSelenium)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(ggthemes)
library(readr)

# URL for Iowa exchange
url <- 'https://iemweb.biz.uiowa.edu/quotes/Pres16_Quotes.html'

# Define function to collect and write data
get_and_write <- function(sleep_time = 15){
  
  # Gather data
  temp <- url %>%
    read_html() %>%
    html_nodes('table') %>%
    html_table()
  
  # Declare the table types
  events <- c('vote_share', 'winner_take_all')
  results_list <- list()
  for (i in 1:2){
    x <- temp[[i]]
    names(x) <- x[1,]
    x <- x[2:3,]
    x$event <- events[i]
    x$time <- Sys.time()
    results_list[[i]] <- x
  }
  results <- do.call('rbind', results_list)
  
  # Gather and clean up
  x <- gather(results, key, value, Bid:Last)
  
  # Write a csv
  file_name <- x$time[1]
  write_csv(x, paste0('iowa_data/x', file_name, '.csv'))
  
  # Sleep for sleep time
  Sys.sleep(sleep_time * 60)
}

# Run over 15 hour period
reps <- 15 * 4

for (j in 1:reps){
  message(paste0('On rep # ', j, '---------------\n'))
  try({
    get_and_write()
  })
}