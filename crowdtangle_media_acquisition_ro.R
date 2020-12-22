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

df_all <- data.frame()
for (i in c(0:50)) {
  token <- t$iri_token
  list_id <- '1484182'
  count <- '100'
  page <- i
  startdate <- today() - 2
  enddate <- today()
  df_temp <- get_posts(startdate, enddate, list_id, count, page)
  
  if (!is.null(df_temp)) {
    df_temp <- df_temp %>% select(-c('expandedLinks', 'media'))
    df_all <- bind_rows(df_all, df_temp)
  }
  
  if (is.null(df_temp)) {
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
  select(-matches('[0-9]$'))

# test_vector <- paste(posts_raw$name, collapse = '|')
# 
# 
# 
# test <- df_flat %>%
#   mutate(
#     media_mentions = str_extract_all(message, test_vector)
#   ) %>%
#   drop_na(
#     media_mentions
#   ) %>%
#   separate_rows(media_mentions, sep = ',')

df_old_media <- s3read_using(read_csv, object = 'romania_post_archive_media.csv', bucket = 'iri-romania')

df_new <- df_old_media %>%
  bind_rows(
    df_flat
  ) %>%
  distinct(postUrl, .keep_all = T)

s3write_using(df_flat, write_csv, object = 'romania_post_archive_media.csv', bucket = 'iri-romania')



