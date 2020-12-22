library(tidyverse)
library(httr)
library(aws.s3)


c <- config::get('aws')
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = c$id,
  "AWS_SECRET_ACCESS_KEY" = c$password,
  "AWS_DEFAULT_REGION" = c$region
)

base_url <- "https://filipvanlaenen.github.io/eopaod/"

r <-GET(paste0(base_url, 'ro.csv'))

df_polls <- content(r)

polls_long <- df_polls %>%
  pivot_longer(
    cols = c(10:length(colnames(.))),
    names_to = c('party'),
    values_to = c('result')
  ) %>%
  mutate(
    result = str_remove(result, '%'),
    result = as.numeric(result)
  ) %>%
  drop_na()

polls_old <- s3read_using(read_csv, object = 'polls_romania.csv', bucket = 'iri-romania')

polls_new <- polls_old %>%
  bind_rows(polls_long) %>%
  distinct()

s3write_using(polls_new, write_csv, object = 'polls_romania.csv', bucket = 'iri-romania')
