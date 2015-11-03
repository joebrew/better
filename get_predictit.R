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

# Source helper functions
source('helpers.R')

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
pb <- txtProgressBar(min = 0, max = length(poll_pages$url_direct), style = 1)
cat(paste0('Getting direct URLs\n'))
for (i in 1:nrow(poll_pages)){
  poll_pages$url_direct[i] <- get_direct_url(url = poll_pages$url[i])
  setTxtProgressBar(pb, i)
}
close(pb)

# Loop through each direct url, extracting the information into a dataframe
results_list <- list()
cat(paste0('Getting dataframes\n'))
for (i in 1:nrow(poll_pages)){
  results_list[[i]] <- make_df(poll_pages$url_direct[i])
}

# Identify arbitrage opportunities
arbitrage_list <- list()
for (i in 1:length(results_list)){
  # Stick into list
  arbitrage_list[[i]] <- clean_df(df = results_list[[i]], 
                                  description = poll_pages$description[i])
}
  
  
# Unlist arbitrage into a list of final
final <- do.call('rbind', arbitrage_list) %>% arrange(desc(p_to_b))

# Plot

# Create a label
final$label <- paste(gsub('_', '', capitalize(final$event)), ':', final$winner) 
final$label <- factor(final$label, levels = final$label)

ggplot(data = final, aes(x = label, y = roi)) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('ROI for arbitrage (pre-fees)') +
  xlab('Event : Winner') +
  ylab('Percentage return on investment')

ggplot(data = final, aes(x = label, y = adjusted_roi)) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('ROI for arbitrage (post-fees)') +
  xlab('Event : Winner') +
  ylab('Percentage return on investment') +
  geom_pointrange(aes(ymax = adjusted_roi_upr, ymin = adjusted_roi_lwr), alpha = 0.6)

# Write csv
file_name <- Sys.time()
write_csv(final, paste0('data/x', file_name, '.csv'))
