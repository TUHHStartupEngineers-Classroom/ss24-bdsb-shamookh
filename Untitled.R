library(RSQLite)
library(glue)
library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(xml2)
library(tidyverse)
library(readr)
library(readxl)
library(xopen)     # Quickly opening URLs
library(RSelenium)
library(purrr)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "content/01_journal/Chinook_Sqlite.sqlite")
dbListTables(con)
tbl(con, "Album")
album_tbl <- tbl(con, "Album") %>% collect()
x<- dbGetQuery(con, 'SELECT * FROM Artist')
dbDisconnect(con)
con
## <SQLiteConnection>
##   DISCONNECTED
https://swapi.dev/api/
GET(url = https://chartmasters.org/most-monthly-listeners-on-spotify/)
https://swapi.dev
https://swapi.dev/api/people/?page=3

name <- "Fred"
glue('My name is {name}.')
## My name is Fred.

resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- GET('https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE')
resp
token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
response
userid = "username"
pwd = "password"
Sys.getenv(“userid”)
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
sp_500 <- url %>%
  read_html(url) %>%
  html_nodes(css = "#constituents") %>%
  html_table() %>% 
  .[[1]] %>%
  as_tibble()


#url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
url <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

print(url)
allTables <- url %>% html_table()
table1 <- album_tbl[[1]]


url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()
rank <- html |>  
  html_elements(css = ".cli-title .ipc-title__text") |>  
  html_text() |>  
  parse_number()

title <- html %>% 
  html_elements(css = ".cli-title .ipc-title__text") |> 
  html_text() |> 
  str_remove("^\\d*\\. ")


rating <- html %>% 
  html_elements(css = ".ratingGroup--imdb-rating") |> 
  html_text() |> 
  str_remove("\\(.*\\)") |> 
  str_squish() |> 
  as.numeric()

imdb_tbl <- tibble(rank, title, rating)

imdb_tbl



client_id = Sys.getenv("client_id")
client_secret = Sys.getenv("client_secret")

url <- "https://id.twitch.tv/oauth2/token"
query <- list(client_id, client_secret, "client_credentials")
names(query) <- c("client_id", "client_secret", "grant_type")


upload <- POST(url,
               query = query) %>% 
  content()


add_headers('Client-ID' = client_id,
            Authorization = paste0("Bearer ", upload$access_token)) %>%
  set_config()

url = 'https://api.twitch.tv/helix/streams'
query <- list(20,      #first
              NULL,    #after
              NULL,    #before
              NULL,    #community_id
              NULL,    #game_id
              NULL,    #language
              NULL,    #type
              NULL,    #user_id
              NULL)    #user_login
names(query) <- c('first',
                  'after',
                  'before',
                  'community_id',
                  'game_id',
                  'language',
                  'type',
                  'user_id',
                  'user_login')

streams <- GET(url,
               query) %>%
  content()
# Streams$Data is list of lists. We want to feed it to as_tibble, for that we need to make a list of
# vectors (when I used list of lists, the output was the type of all values, not the actual values themselves.
#To make vectors, we need lists that have elements of of only the same type. The same-index-elements of the
#inner lists correspond to the same feature (like game_id) so we want to transpose our matrix(list), so that we can
# get values of the same type in each list. Now we can make the lists into vectors
data <- streams$data %>% 
  #transpose() %>%         
  simplify_all() %>%
  as_tibble()
data %>%
  head(n=5) 


ur <- "https://www.skyscanner.de/transport/flights/fran/nyca/240427/240504/?adults=1&adultsv2=1&cabinclass=economy&children=0&childrenv2=&destinationentityid=27537542&inboundaltsenabled=false&infants=0&originentityid=27541706&outboundaltsenabled=false&preferdirects=false&ref=home&rtn=1"

httr::GET(ur,
          config = add_headers("user-agent" = "Mozilla/5.0"))

html <- ur %>% 
  read_html()

