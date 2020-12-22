library(aws.s3)
library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(radarchart)
library(ggradar)
library(wordcloud2)
library(networkD3)
library(treemap)
library(d3treeR)

c <- config::get('aws')
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = c$id,
  "AWS_SECRET_ACCESS_KEY" = c$password,
  "AWS_DEFAULT_REGION" = c$region
)

party_names <- c(
  'RMDSZ',
  'Partidul Naţional Liberal',
  'Uniunea Salvați România - USR',
  "PLUS",
  "Alianța pentru Unirea Românilor - AUR",
  "Partidul Mișcarea Populară",
  "Partidul Social Democrat"
)

party_short_names <- c('RMDSZ', 
                       'PNL', 
                       'USR', 
                       'PLUS', 
                       'AUR', 
                       'PMP',
                       'PSD')
party_colors <- c('#15803C',
                  '#FFDD00',
                  '#00aae7',
                  '#ff4f00',
                  '#FCC224',
                  '#0084CA',
                  "#ED2128"
                  )
valid_colors <- c("green",
                  "yellow",
                  "blue",
                  "orange",
                  "yellow",
                  "green",
                  "red")

df_party_names <- data.frame(name = party_names, short_name = party_short_names, color = party_colors, valid_color = valid_colors)

posts_raw <- s3read_using(read_csv, object = 'romania_post_archive.csv', bucket = 'iri-romania') %>%
  mutate(
    date = ymd_hms(date),
    updated = ymd_hms(updated)
  ) %>%
  left_join(df_party_names) %>%
  mutate(
    short_name = coalesce(short_name, name)
  )

df_hashtags <- posts_raw %>%
  mutate(
    hashtags = str_extract_all(message, '#.* ')
  ) %>%
  select(name, hashtags) %>%
  drop_na(hashtags) %>%
  separate_rows(hashtags, sep = "#") %>%
  filter(hashtags != '') %>%
  group_by(hashtags) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

corpus <- s3read_using(read_csv, object = 'romania_word_archive.csv', bucket = 'iri-romania')

corpus_en <- s3read_using(read_csv, object = 'romania_word_archive_en.csv', bucket = 'iri-romania')

polls <- s3read_using(read_csv, object = 'polls_romania.csv', bucket = 'iri-romania') %>%
  mutate(result = result / 100)

media <- s3read_using(read_csv, object = "romania_post_archive_media.csv", bucket = "iri-romania")

wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                            browser.padding = 0, browser.fill = TRUE))
  chart
}

extract_vector <- paste(posts_raw$name, collapse = '|')
  




