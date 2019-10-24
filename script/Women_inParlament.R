library(rvest)
library(tidyverse)

wdr <- getwd()

start.page <- "http://archive.ipu.org/wmn-e/classif-arc.htm"

links <- start.page %>% 
  read_html() %>% 
  html_nodes("a") %>% 
  html_attr(.,"href")

df.links <- enframe(links, name=NULL) %>% 
  filter(str_detect(value,"[0-9]{6}.htm")) %>% 
  mutate(value=paste0("http://archive.ipu.org/",value))

class(df.links)

fn_scrap <- function(x) {
  
  pb$tick()$print()
  
  x %>% 
  read_html() %>% 
  html_node(".data") %>% 
  html_table(fill=T)}

df.links$value[1:2] 

x <- fn_scrap(df.links$value[1])
y <- fn_scrap(df.links$value[2])

u <- fn_scrap(df.links$value[1])

pb <- progress_estimated(nrow(df.links))


df.all <- df.links$value %>% 
  set_names() %>% 
  map_dfr(., possibly(fn_scrap, otherwise=NULL), .id="monthyear")


write_csv2(df.all, path=paste0(wdr, "/data/women_in_parlament.csv"))

#map_df(., fn_scrap, .id="monthyear")  


