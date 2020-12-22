library(tidyverse)
library(httr)
library(lubridate)
library(jsonlite)
library(aws.s3)
library(udpipe)
library(translateR)
library(cld3)

c <- config::get('aws')
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = c$id,
  "AWS_SECRET_ACCESS_KEY" = c$password,
  "AWS_DEFAULT_REGION" = c$region
)

t <- config::get('token')



get_posts <- function(startdate, enddate, list_id, count, page) {
  
  offset <- page * 100
  temp <- GET(paste0(
    'https://api.crowdtangle.com/posts?token=',
    token,
    '&count=', count,
    '&offset=', offset,
    '&listIds=', list_id,
    '&startDate=', startdate))
  raise <- content(temp, as = 'text')
  
  new <- fromJSON(raise)
  
  df_temp <- new$result$posts
  
  return(df_temp)
}

lists <- GET(paste0('https://api.crowdtangle.com/lists?token=', t))

raise <- content(lists, as = 'text')
new <- fromJSON(raise)
df_temp <- new$result$lists

df_all <- data.frame()
for (i in c(0:50)) {
  token <- t$iri_token
  list_id <- '1473572'
  count <- '100'
  page <- i
  startdate <- today() - 2
  enddate <- today()
  df_temp <- get_posts(startdate, enddate, list_id, count, page) %>%
    select(-c('expandedLinks', 'media'))
  df_all <- bind_rows(df_all, df_temp)
  if (nrow(df_temp) != 100) {
    print('finished')
    break
  } 
}

other <- df_all %>% select(-starts_with('statistics')) %>%
  select(-c(starts_with('account'), 'platform', 'id', 'subscriberCount', 'platform'))

expected <-  df_all$statistics$expected %>%
  rename_all(
    function(x) paste0(x, '_expected')
  )

df_flat <- bind_cols(
  other,
  df_all$statistics$actual, 
  expected, 
  df_all$account
                    ) %>%
  mutate(
    date = as.character(date),
    date = ymd_hms(date),
    updated = as.character(updated),
    updated = ymd_hms(updated)
  ) %>%
  select(-ends_with('1'))

df_flat <- translate(
  dataset = df_flat,
  content.field = 'message',
  google.api.key = 'AIzaSyAMUp0wWw31PEPoZXihWUNuN_jAqkgFs1U',
  source.lang = 'ro',
  target.lang = 'en')

#applying natural language processing

#first extract word counts
udmodel <- udpipe_load_model(file = 'romanian-nonstandard-ud-2.5-191206.udpipe')
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')

all_corpus <- data.frame()
all_corpus_english <- data.frame()
for (i in (1:nrow(df_flat))) {
  df_anno <- udpipe_annotate(udmodel, x = df_flat[[i, 'message']]) %>% as.data.frame() %>%
    filter(upos %in% c('ADJ', 'NOUN', 'PROPN', 'VERB')) %>%
    mutate(
      name = df_flat[[i, 'name']],
      newId = df_flat[[i, 'newId']],
      date = df_flat[[i, 'date']]
    ) %>%
    select(lemma, name, date, token_id, upos, sentence_id)
  
  df_english <- udpipe_annotate(udmodel_english, x = df_flat[[i, 'translatedContent']]) %>% as.data.frame() %>%
    filter(upos %in% c('ADJ', 'NOUN', 'PROPN', 'VERB')) %>%
    mutate(
      name = df_flat[[i, 'name']],
      newId = df_flat[[i, 'newId']],
      date = df_flat[[i, 'date']]
    ) %>%
    select(lemma, name, date, token_id, upos, sentence)
  
  
  
  all_corpus <- bind_rows(all_corpus, df_anno)
  all_corpus_english <- bind_rows(all_corpus_english, df_english)
}

all_scores <- data.frame()
sentiment <- function(dataframe) {
  for (i in (1:nrow(dataframe))) {
    df_anno <- udpipe_annotate(udmodel_english, x = df_flat[[i, 'translatedContent']]) %>% as.data.frame()
    
    neg <- read_tsv('negative-words.txt') %>%
      mutate(sentiment = 'negative')
    
    pos <- read_tsv('positive-words.txt') %>%
      mutate(sentiment = 'positive')
    
    df_sentiment <- bind_rows(neg, pos) %>%
      mutate(
        polarity = ifelse(sentiment == 'positive', 1, -1)
      ) %>%
      rename(term = word)
    
    scores <- txt_sentiment(
      x = df_anno,
      term = 'lemma',
      polarity_terms = df_sentiment,
      polarity_negators = c('not', 'neither', "don't"),
      polarity_amplifiers = c('very', 'many', 'really', 'quite'),
      polarity_deamplifiers = c('slightly', 'somewhat')
    )$overall %>% select(terms, sentiment_polarity)
    
    all_scores <- bind_rows(scores, all_scores)
  }
  return(all_scores)
}

df_sentiment <- sentiment(df_flat)

df_final <- bind_cols(df_flat, df_sentiment)

#upload process for posts

df_posts_old <- s3read_using(read_csv, object = 'romania_post_archive.csv', bucket = 'iri-romania') %>%
  mutate(
    date = ymd_hms(date),
    updated = ymd_hms(updated),
    platformId = as.character(platformId)
  )


df_combined_new <- df_posts_old %>%
  bind_rows(df_final) %>%
  distinct(postUrl, .keep_all = TRUE)

s3write_using(df_combined_new, write_csv, object = 'romania_post_archive.csv', bucket = 'iri-romania')

#upload process for wordcounts

corpus_old <- s3read_using(read_csv, object = 'romania_word_archive.csv', bucket = 'iri-romania') %>%
  mutate(token_id = as.character(token_id))

corpus_new <- all_corpus %>%
  bind_rows(corpus_old) %>%
  distinct(token_id, lemma, newId, .keep_all = T)

s3write_using(corpus_new, write_csv, object ='romania_word_archive.csv', bucket = 'iri-romania')

en_corpus_old <- s3read_using(read_csv, object = 'romania_word_archive_en.csv', bucket = 'iri-romania') %>%
  mutate(token_id = as.character(token_id))

en_corpus_new <- all_corpus_english %>%
  bind_rows(en_corpus_old) %>%
  distinct(token_id, lemma, newId, .keep_all = T)

s3write_using(en_corpus_new, write_csv, object ='romania_word_archive_en.csv', bucket = 'iri-romania')
  


