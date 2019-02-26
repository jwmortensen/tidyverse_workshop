## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,warning=FALSE,message=FALSE)

## ------------------------------------------------------------------------
# install.packages(c("tidyverse","rvest","ggrepel")
library(tidyverse)
library(rvest)
library(ggrepel)

## ------------------------------------------------------------------------
x <- c(0.109, 0.359, 0.63)
# have to nest a lot of parentheses together
round(mean(log(x)), 1)
# pipe operator makes it cleaner!
x %>%
  log() %>%
  mean() %>%
  round(1)

## ------------------------------------------------------------------------
url = "https://www.basketball-reference.com/leagues/NBA_2019_games.html"
url %>%
  read_html() %>%
  html_nodes("#schedule") %>%
  html_text()

## ------------------------------------------------------------------------
bball = url %>%
  read_html() %>%
  html_nodes("#schedule") %>%
  html_table() %>%
  .[[1]]

names(bball) = c("date", "start_time", "visitor", "vistor_pts",
            "home", "home_pts", "col1", "col2", "attend", "Notes")
bball <- bball %>% dplyr::select(-col1, -col2, -Notes)
head(bball, 3)

## ------------------------------------------------------------------------
attendAvg = bball %>%
  mutate(attend = as.numeric(str_replace_all(attend,",",""))) %>%
  group_by(home) %>%
  summarise(no_of_games = n(),
            avg_attend = mean(attend)) %>%
  arrange(desc(avg_attend))
attendAvg

## ------------------------------------------------------------------------
ggplot(attendAvg, aes(x = reorder(home, avg_attend), y = avg_attend)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Average home attendence in NBA in October")

## ------------------------------------------------------------------------
url2 = "https://www.imdb.com/search/title?title_type=feature&release_date=2018-01-01,2018-12-31&sort=boxoffice_gross_us,desc"
url2 %>%
  read_html() %>%
  html_nodes("div.lister-item:nth-child(2) > div:nth-child(3)
             > div:nth-child(3) > div:nth-child(1)") %>%
  html_text() %>%
  as.numeric()

## ------------------------------------------------------------------------
url2 %>%
  read_html() %>%
  html_nodes("div.lister-item:nth-child(1) > div:nth-child(3)") %>%
  html_text()

## ----error = T-----------------------------------------------------------
url2 %>%
  read_html() %>%
  html_nodes("div.lister-item:nth-child(1) > div:nth-child(3)") %>%
  html_table()

## ------------------------------------------------------------------------
ranking = url2 %>% read_html() %>% html_nodes(".text-primary") %>%
  html_text()

rating = url2 %>% read_html() %>% html_nodes('.ratings-imdb-rating strong') %>%
  html_text()

title = url2 %>% read_html() %>% html_nodes('.lister-item-header a') %>%
  html_text()

## ------------------------------------------------------------------------
revenue = url2 %>% read_html() %>% html_nodes('.ghost~ .text-muted+ span') %>%
  html_text()

# movie length
runtime = url2 %>% read_html() %>% html_nodes('.text-muted .runtime') %>%
  html_text()

## ------------------------------------------------------------------------
movie_df = data.frame(ranking = ranking, title = title, rating = rating,
                      runtime = runtime, revenue = revenue, stringsAsFactors = F)
movie_df = movie_df %>% mutate(
                    ranking = str_remove_all(ranking,"\\.") %>% as.numeric(),
                    rating = as.numeric(rating),
                    runtime = str_remove_all(runtime," min") %>% as.numeric(),
                    revenue = str_remove_all(revenue, "M"),
                    revenue = str_replace_all(revenue, "\\$",""),
                    revenue = as.numeric(revenue))

## ------------------------------------------------------------------------
p = movie_df %>% ggplot( aes(x=rating, y=revenue)) +
  geom_point() + theme_bw() + xlim(c(3,10)) + ylim(c(-100,1000)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = mean(movie_df$revenue),
             size = 0.7, color = "red", alpha=0.5, linetype="dashed") +
  geom_vline(xintercept = mean(movie_df$rating),
             size = 0.7, color = "red", alpha=0.5, linetype="dashed") +
 geom_text_repel(movie_df %>% filter(revenue < 320, rating > 8),
                 mapping = aes(x=rating, y=revenue, label = title),
                 size = 3.5,
                 nudge_x = 1.5,
                 direction    = "y",
                 segment.size = 0.8) +
 geom_text_repel(movie_df %>% filter(rating < 5 | revenue > 320),
                 mapping = aes(x=rating, y=revenue, label = title),
                 size = 3.5,
                 nudge_x = -0.85,
                 direction    = "y",
                 segment.size = 0.8)

## ----echo = T------------------------------------------------------------
p

## ----echo = T------------------------------------------------------------
summary(lm(revenue ~ rating, data = movie_df))

## ------------------------------------------------------------------------
ggplot(movie_df, aes(x = rating, y = revenue)) +
  geom_point() + stat_smooth(method = "lm", col = "red")

