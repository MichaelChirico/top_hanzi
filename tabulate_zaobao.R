library(rvest)
library(data.table)
source('utils.R')
zaobao_stem = 'http://www.zaobao.com.sg'
zaobao_main = paste0(zaobao_stem, '/znews')

zaobao_top = read_html(zaobao_main)
all_link_zi = zaobao_top %>% html_nodes('a') %>% blocks_to_zi

counts = zi_counts(all_link_zi)

toplevel = zaobao_top %>% 
  html_nodes('a') %>% html_attr('href') %>% unique %>% 
  # sections characterized by having /znews/no-following-slash
  grep('/znews/[^/]+$', ., value = TRUE)

progress = txtProgressBar(style = 3)
for (section in toplevel) {
  setTxtProgressBar(progress, match(section, toplevel)/length(toplevel))
  articles = read_html_or_sleep(paste0(zaobao_stem, section)) %>% 
    html_nodes(xpath = sprintf('//a[contains(@href, "%s/")]', section)) %>%
    html_attr('href') %>% unique %>% paste0(zaobao_stem, .)
  for (article in articles) {
    article_counts = read_html_or_sleep(article) %>% 
      html_nodes(xpath = '//p|//h1|//h3') %>% 
      blocks_to_zi %>% zi_counts
    #concatenate and condense
    counts = rbind(counts, article_counts)[ , .(N = sum(N)), by = zi]
    Sys.sleep(1)
  }
}

fwrite(counts, 'zaobao_counts.csv')
