library(Radlibrary)
library(aws.s3)
library(config)
library(readr)

c <- config::get('aws')

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = c$id,
  "AWS_SECRET_ACCESS_KEY" = c$password,
  "AWS_DEFAULT_REGION" = c$region
)

query <- adlib_build_query(
  ad_active_status = 'ALL',
  ad_reached_countries = 'RO',
  ad_type = c("POLITICAL_AND_ISSUE_ADS"),
  search_page_ids = c("181375448581167", 
                      '291177524360702', 
                      '102601499822802', 
                      '1058116737612843', 
                      '1903580739951981',
                      '581911828514783',
                      '621574984950400',
                      '2212856535602171'
                      ),
  ad_delivery_date_min = "2020-12-01"
  )


response <- adlib_get(params = query)
df_response <- as_tibble(response) %>%
  mutate(
    page_id = as.numeric(page_id),
    adlib_id = as.numeric(adlib_id)
  )

old_ads <- s3read_using(read_csv, object = 'romania_ads_archive.csv', bucket = 'iri-romania')

new_ads <- old_ads %>%
  bind_rows(df_response) %>%
  distinct()

s3write_using(new_ads, write_csv, object = 'romania_ads_archive.csv', bucket = 'iri-romania')


  
