library(jsonlite)
library(magrittr)
library(data.table)
source('utils.R')
# attempt a larger number and you should get something back along
#   the lines of value invalid,expect (int[1~200])
MAX_COUNT = 200L
# just go to town until sample sized is reach (more methodical
#   would be to take a more temporally (or even spatiotemporally)
#   dispersed set of posts to avoid local concentrations (e.g. news
#   stories, or in the spatial case, local slang) and make this more random
DESIRED_SAMPLE = 1e6
# both to avoid breaking the service, and to help spread out
#   the content of the posts more substantially
SLEEP_INTERVAL = 360
# infinite gratitude to Yecheng Tan, who authored this post
#   that finally got me past an impasse in getting an access token:
#   https://gwu-libraries.github.io/sfm-ui/posts/2016-04-26-weibo-api-guide
token = readLines('secret/weibo.access_token', n = 1L)
base_url = 'https://api.weibo.com/2/statuses/public_timeline.json'
public_weibo_url = sprintf('%s?access_token=%s&count=%d', 
                           base_url, token, MAX_COUNT)

observed_characters = 0L
logprog = 1L
counts = data.table(zi = numeric(0L), N = integer(0L))
while (observed_characters < DESIRED_SAMPLE) {
  these_counts = read_json(public_weibo_url) %$% 
    statuses %>% sapply(extract2, 'text') %>%
    strip_nonzi %>% strsplit(NULL) %>% unlist %>% zi_counts
  observed_characters = observed_characters + these_counts[ , sum(N)]
  if (observed_characters > logprog) {
    message('Now seen at least ', logprog, ' characters')
    logprog = 2**ceiling(log2(observed_characters))
  }
  counts = rbind(counts, these_counts)[ , .(N = sum(N)), by = zi]
  Sys.sleep(SLEEP_INTERVAL)
}

fwrite(counts, 'weibo_counts.csv')
