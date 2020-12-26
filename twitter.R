library("rtweet")
library("tidyverse")
library("ggmap")
library("countrycode")
library("stringr")
library("DBI")

register_google(key = Sys.getenv("GOOGLE_KEY"))

twitter_token <- create_token(
  app = Sys.getenv("TWITTER_APPNAME"),
  consumer_key = Sys.getenv("TWITTER_KEY"),
  consumer_secret = Sys.getenv("TWITTER_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_SECRET"))

date <- Sys.Date()
date <- date - 1

#search tweets related to tourism in Italy
it_tweets <- search_tweets(q = "italy travel OR tourism OR trip OR vacation OR holiday",
                                n = 50000,
                                lang = "en",
                                include_rts = FALSE,
                                retryonratelimit = TRUE,
                                since = date)

#select variables
it_tweets <- it_tweets %>% 
  select(user_id, created_at, text, hashtags, location)

#clean text
clean_tweets <- function(x) {
  x %>%
    str_remove_all("@[[:alnum:]]+") %>%
    str_remove_all("#[[:alnum:]]+") %>%
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    str_replace_all("&amp;", "and") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("^RT:? ") %>%
    str_replace_all("\\\n", " ") %>%
    str_remove_all("[^\x01-\x7F]") %>%
    str_to_lower() %>%
    str_trim("both")
}

it_tweets$tweet <- it_tweets$text %>% 
  clean_tweets

#clean hashtags
it_tweets <- it_tweets %>%
  mutate(hashtags = sapply(hashtags, toString))

it_tweets$hashtags <- str_remove_all(it_tweets$hashtags, "[^A-Za-z0-9,;._-]+, ")

#remove tweets without location
it_tweets <- it_tweets[!(is.na(it_tweets$location) | it_tweets$location == ""), ]

#remove locations with special characters
it_tweets <- it_tweets %>% 
  filter(grepl("^[a-zA-Z0-9,:; ]+$", location)) %>%
  filter(!location == "Worldwide")

#geocode locations
it_coord <- ggmap::geocode(it_tweets$location)

it_tweets <- cbind(it_tweets, it_coord)

it_tweets  <- it_tweets %>% 
  drop_na(lon)

it_tweets$result <- do.call(rbind,
                     lapply(1:nrow(it_tweets),
                            function(i)revgeocode(as.numeric(it_tweets[i,7:8]))))

it_tweets$country <- sub('.*\\, ', '', it_tweets$result)

it_tweets$code <- countryname(it_tweets$country, 
                              destination = 'iso3c')

it_tweets$country <- countrycode(it_tweets$code,
                                 origin ='iso3c',
                                 destination = 'country.name')

it_tweets  <- it_tweets %>% 
  drop_na(code)

it_tweets$created_at <- as.Date(it_tweets$created_at, format = '%Y%m%d')

it_tweets <- it_tweets %>% 
  select(created_at, user_id, country, code, hashtags, tweet)

con <- DBI::dbConnect(odbc::odbc(), Driver = "ODBC Driver 17 for SQL Server", 
                       Server = Sys.getenv("SQLSERVER"), 
                       Database = Sys.getenv("SQLSERVER_DB"), 
                       UID =  Sys.getenv("SQLSERVER_USERNAME"), 
                       PWD =  Sys.getenv("SQLSERVER_PASSWORD"), 
                       Port = 1433)

dbWriteTable(con, "twitter", it_tweets, append = TRUE)

dbDisconnect(con)