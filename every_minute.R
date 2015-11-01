for (z in 1:10000){
  try({
    source('get_data.R')
  })
  cat(paste0('Just finished # ', z, '---------------------\n'))
  }