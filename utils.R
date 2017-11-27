# props to this answer for the unicode block:
#   https://stackoverflow.com/a/41155368/3576984
#   only include one of the three blocks mentioned
#   there (the one containing everyday characters)
strip_nonzi = function(x) {
  gsub('[^\u4E00-\u9FCC]', '', x)
}

# unique cuts down on intentional duplication
#   (repeating article title, using text
#    in photo captions, etc)
blocks_to_zi = function(x) {
  x %>% html_text %>% strip_nonzi %>% unique %>%
    strsplit(NULL) %>% unlist
}

# almost a perfect case for hash table
#   -- but we want the names as well
zi_counts = function(zi) {
  data.table(zi = zi)[ , .N, by = zi]
}

# Anti-throttling measure;
#   only allowing for timeout errors, fail otherwise
read_html_or_sleep = function(x) {
  tryCatch(read_html(x), error = function(e) {
    if (grepl('Timeout', e$message)) {
      cat('\nTimed out; sleeping 90 seconds.\n')
      Sys.sleep(90)
      read_html(x)
    }
  })
}
