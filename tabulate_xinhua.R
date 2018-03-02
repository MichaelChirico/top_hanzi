#scrape the article titles and all article
#  pages from Xinhua's front page, then
#  tabulate the counts of each character
library(rvest)
library(data.table)
source('utils.R')
xinhua_main = 'http://www.xinhuanet.com/'

xinhua_top = read_html(xinhua_main)
all_link_zi = xinhua_top %>% html_nodes('a') %>% blocks_to_zi

counts = zi_counts(all_link_zi)

articles = xinhua_top %>% 
  html_nodes(xpath = '//a[contains(@href, "news.xinhuanet.com")]') %>% 
  html_attr('href') %>% unique

progress = txtProgressBar(style = 3)
for (article in articles) {
  setTxtProgressBar(progress, match(article, articles)/length(articles))
  article_counts = read_html_or_sleep(article) %>% 
    html_nodes(xpath = '//p|//div[@class="h-title"]') %>% 
    blocks_to_zi %>% zi_counts
  #concatenate and condense
  counts = rbind(counts, article_counts)[ , .(N = sum(N)), by = zi]
  Sys.sleep(1)
}

fwrite(counts, 'data/xinhua_counts.csv')
