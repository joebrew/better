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

# Create a list of poll pages
poll_pages <- data.frame(description = 
                           c('supreme_court_1415',
                             'republican_nomination_16',
                             'democratic_nomination_16',
                             'president_16'),
                         url = 
                           c('http://www.predictwise.com/node/4108',
                             'http://www.predictwise.com/politics/2016RepNomination',
                             'http://www.predictwise.com/politics/2016DemNomination',
                             'http://www.predictwise.com/politics/2016president'),
                         stringsAsFactors = FALSE)

# Loop through each poll page to get the direct html table links
poll_pages$url_direct <- NA
for (i in 1:nrow(poll_pages)){
  
  # REPLACE WITH GET_DIRECT_URL
  message(paste0('Working on row ', i, ' of ', nrow(poll_pages)))
  # Get landing URL
  poll_page <- poll_pages$url[i]
  
  # Read landing page
  poll_html <- read_html(poll_page)
  
  # Get links to html tables
  table_urls <- 
    poll_html %>%
    html_nodes('div iframe') %>%
    html_attr('src')
  
  # Go to link and read table
  table_url <- table_urls[length(table_urls)] # just get the last one, since that's usually raw
  
  # Stick into poll_pages
  poll_pages$url_direct[i] <- table_url
  message('---done!')
}

# Loop through each direct url, extracting the information into a dataframe
results_list <- list()
for (i in 1:nrow(poll_pages)){
  
  # REPLACE WITH MAKE_DF
  message(paste0('Working on row ', i, ' of ', nrow(poll_pages)))
  
  # Read the processed html (post js function) using RSelenium
  checkForServer()
  startServer() # requires java
  remDr <- remoteDriver(remoteServerAddr = "localhost", 
                        port = 4444, 
                        browserName = "firefox") # use phantom js instead
  Sys.sleep(5)
  
  url <- poll_pages$url_direct[i]
  Sys.sleep(5)
  
  remDr$open()
  Sys.sleep(5)
  
  remDr$navigate(url)
  message('Sleeping to let the browser load')
  Sys.sleep(5)
  url_source <- remDr$getPageSource()[[1]] # use phantom js instead to not have to render in browser
  
  # Get the time
  the_time <- Sys.time()
  
  # Column names
  column_names <- url_source %>% read_html() %>% html_nodes('thead th') %>% html_text() %>%
    tolower
  
  # Table content
  table_content <- url_source %>% read_html() %>% html_nodes('body table') %>% html_table() 
  
  # Combine
  the_table <- data.frame(table_content[[1]]); names(the_table) <- gsub(' ', '_', column_names)
  
  # Close connection
  remDr$close()
  
  
  # Gather
  temp <- gather(the_table, venue, value, 2:ncol(the_table))
  
  # Get type
  temp <- temp %>%
    mutate(type = ifelse(grepl('$', temp$value, fixed = TRUE),
                         'dollars',
                         ifelse(grep('%', temp$value, fixed = TRUE),
                                'percent',
                                'none'))) 
  # Make value numeric
  temp$value <- as.numeric(gsub('[$]| |%|,', '', temp$value))
  
  # Add a timestamp
  temp$time <- the_time
  
  # Stick into results
  results_list[[i]] <- temp
}

# Identify arbitrage opportunities
arbitrage_list <- list()
for (i in 1:length(results_list)){
  
  # REPLACE WITH CLEAN_DF (manually giving the "desription")
  
  temp <- results_list[[i]]
  
  # Descriptor
  descriptor <- names(temp)[1]
  
  # just looking at predictit vs betfair
  temp <- 
    temp %>%
    # filter(venue %in% c('predictit', 'betfair')) %>% # what about lay, back, buy, sell?
    group_by_(winner = descriptor) %>%
    summarise(#p_to_b = value[venue == 'predictit'] - value[venue == 'betfair'],
              p = value[venue == 'predictit'],
              b = value[venue == 'betfair'],
              the_time = first(time)) %>%
    mutate(p_to_b = p - b) %>%
    mutate(sell = ifelse(p_to_b > 0, 'p', ifelse(p_to_b < 0, 'b', NA))) %>%
    mutate(buy = ifelse(sell == 'b', 'p', 
                        ifelse(sell == 'p', 'b', NA))) %>%
    mutate(p_to_b = abs(p_to_b)) %>%
    arrange(desc(p_to_b)) 
  
  # Specify the event
  temp$event <- poll_pages$description[i] #descriptor
  
  # Stick into list
  arbitrage_list[[i]] <- temp
}

# Unlist arbitrage into a list of final
final <- do.call('rbind', arbitrage_list)
final <- final[!is.na(final$p_to_b),] %>% arrange(desc(p_to_b))

# Get the expected value of a two-way purchase
final$purchase_cost <-
  ifelse(final$sell == 'p',
         final$b + (1 - final$p),
         ifelse(final$sell == 'b',
                (1 - final$b) + final$p,
                NA))

# Get profit
final$profit <- 1 - final$purchase_cost

# ROI
final$roi <- ((1 / final$purchase_cost)-1) * 100

# Create a label
final$label <- paste(gsub('_', '', capitalize(final$event)), ':', final$winner) # why not working capitalize?
final$label <- factor(final$label, levels = final$label)

# Plot
ggplot(data = final, aes(x = label, y = roi)) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('ROI for arbitrage (pre-fees)') +
  xlab('Event : Winner') +
  ylab('Percentage return on investment')

# Write csv
file_name <- Sys.time()
write_csv(final, paste0('data/x', file_name, '.csv'))
