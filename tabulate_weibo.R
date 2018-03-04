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

# as in Slack (and WeChat, for that matter), emoji tend to 
#   be represented as bracketed tags, e.g. [太阳] for sun
#   or [兔子] for rabbit; since the user likely doesn't type the
#   characters for these themselves, and since they display
#   as pictures (not characters), they represent a substantial
#   source of potential bias. Even though some legitimate text that
#   just happens to be between square brackets may be lost, the
#   prevalence of these "emoji" far outstrips anybody tweeting text
#   in brackets; strip them away. There's also a lot of text in 
#   between Chinese-font block brackets【】, but from a glance this
#   appears more likely to be legitimate user input. 
strip_emoji = function(x) gsub('\\[[^]]*\\]', '', x)

observed_characters = 0L
logprog = 1L
counts = data.table(zi = numeric(0L), N = integer(0L))
while (observed_characters < DESIRED_SAMPLE) {
  these_counts = read_json(public_weibo_url) %$% 
    statuses %>% sapply(`[[`, 'text') %>% strip_emoji %>% 
    strip_nonzi %>% strsplit(NULL) %>% unlist %>% zi_counts
  observed_characters = observed_characters + these_counts[ , sum(N)]
  if (observed_characters > logprog) {
    message('Now seen at least ', logprog, ' characters')
    logprog = 2**ceiling(log2(observed_characters))
  }
  counts = rbind(counts, these_counts)[ , .(N = sum(N)), by = zi]
  Sys.sleep(SLEEP_INTERVAL)
}

fwrite(counts, 'data/weibo_counts.csv')
