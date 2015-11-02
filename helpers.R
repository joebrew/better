#####
# GET THE DIRECT TABLE URL FROM A PREDICTIT URL
#####
get_direct_url <- function(url){

  # Read landing page
  temp_html <- read_html(url)
  
  # Get links to html tables
  table_urls <- 
    temp_html %>%
    html_nodes('div iframe') %>%
    html_attr('src')
  
  # Go to link and read table
  table_url <- table_urls[length(table_urls)] # just get the last one, since that's usually raw
  
  # Spit back
  return(table_url)
}

#####
# EXTRACT A PREDICTIT TABLE INTO A DATAFRAME
#####
make_df <- function(direct_url){
  # Read the processed html (post js function) using RSelenium
  checkForServer()
  startServer() # requires java
  remDr <- remoteDriver(remoteServerAddr = "localhost", 
                        port = 4444, 
                        browserName = "firefox") # use phantom js instead
  # Sys.sleep(5)
    # Sys.sleep(5)
  
  remDr$open()
  Sys.sleep(2)
  
  remDr$navigate(direct_url)
  # message('Sleeping to let the browser load')
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
  
  return(temp)
}

#####
# CLEAN A DATAFRAME FROM PREDICTIT ADDING ARBITRAGE POSSIBILITIES
#####
clean_df <- function(df, description = NULL){
  
  # Descriptor
  descriptor <- names(df)[1]
  
  # just looking at predictit vs betfair
  temp <- 
    df %>%
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
  temp$event <- description
  
}