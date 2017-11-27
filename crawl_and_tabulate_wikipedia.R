# recursively search the graph of Chinese
#   Wikipedia articles, pulling article
#   links from each page then randomly
#   visiting one of them, until a total
#   of 100,000 (unique) links are accumulated;
#   write out these links, then
#   sample 1,000 of the pages and 
#   tabulate the characters found therein
library(rvest)
library(data.table)
source('utils.R')

zh_stem = 'https://zh.wikipedia.org'
cur_link = link_list = explored = 
  '/wiki/%E7%BB%B4%E5%9F%BA%E7%99%BE%E7%A7%91'

search_size = 2500

#/wiki/ finds internal pages;
#  % aims to ensure some Chinese character in the URL
#  want to exclude:
#    external links [http/s]
#    internal special links (files, images) [:]
#    subheader/anchor links [#]
#    wikimedia links
link_xp = paste('//a[contains(@href, "/wiki/") and',
                'contains(@href, "%") and',
                'not(contains(@href, "http") or',
                'contains(@href, ":") or',
                'contains(@href, "#") or',
                'contains(@href, "wikimedia"))]')

while (length(link_list) < search_size) {
  links_here = read_html_or_sleep(paste0(zh_stem, cur_link)) %>% 
    html_nodes(xpath = link_xp) %>% 
    html_attr('href') %>% unique
  link_list = unique(c(link_list, links_here))
  # for diagnosing potential issues
  cat(sprintf('Now here (#%d): %s [%d links found; %d accumulated]\n',
              length(explored), cur_link,
              length(links_here), length(link_list)))
  cur_link = sample(setdiff(links_here, explored), 1L)
  explored = c(explored, cur_link)
  Sys.sleep(max(.5 + rnorm(1L, sd = .1), 0))
}
