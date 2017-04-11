x <- list.files(pattern='frame')
first.gif <- as.numeric(sub('.*?([0-9]+).*', '\\1', x, perl=TRUE)) <= 418
file.rename(x[first.gif], paste0('gif1-', x[first.gif]))
file.rename(x[!first.gif], paste0('gif2-', x[!first.gif]))

