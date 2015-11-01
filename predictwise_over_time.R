library(readr)
library(ggplot2)
library(ggthemes)

# Point to where all the csvs are saved from scraping predictwise
csvs <- dir('data')

# Loop through each file, reading it into memory
results_list <- list()
for (i in 1:length(csvs)){
  temp <- read_csv(paste0('data/', csvs[i]))
  results_list[[i]] <- temp
  message(paste0(i, ' of ', length(csvs)))
}
# Bind together all the results
ts <- do.call('rbind', results_list)
rm(temp, csvs, i, results_list)

# Make a clean label
ts$clean_label <- gsub('President16 : ', '', ts$label)

# Plot the predictit to betfair spreads for each competition
ggplot(data = ts[ts$event == 'president_16',], 
       aes(x = the_time, y = p)) +
  geom_line(alpha = 0.6) +
  geom_line(aes(x = the_time, y = b), color = 'red', alpha = 0.6) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y = element_text(size = 7)) +
  facet_wrap( ~ clean_label, ncol=4, scales = 'free') +
  xlab('Date-Time') +
  ylab('Black = PredictIt, Red = Betfair') +
  ggtitle('Cross-Market Arbitrage Opportunities')


# Chart each spread
labels <- unique(ts$label)
for (i in 1:length(labels)){
  temp <- ggplot(data = ts[ts$label == labels[i],],
         aes(x = the_time, y = p)) +
    geom_line() +
    geom_line(aes(x = the_time, y = b), color = 'red') +
    ggtitle(labels[i])
  print(temp)
}

