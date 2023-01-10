library(tidyverse)
library(readr)
library(purrr)

key <- "3b8d3pffhfk20g96"

url_trove <- "https://api.trove.nla.gov.au/v2/gazette/titles?key=<INSERT KEY>" %>%
  str_replace(pattern = "<INSERT KEY>", replacement = key)

resp <-  httr::GET(url = url_trove)

converted <- jsonlite::fromJSON(rawToChar(resp$content))

dat_x <- converted$response$records$newspaper %>%
  dplyr::filter(str_detect(title, "Commonwealth of Australia Gazette.")) %>%
  dplyr::filter(str_detect(title, "Public Service"))

#---------------------------------------------------------

url_trove <- "https://api.trove.nla.gov.au/v2/gazette/1326?key=<INSERT KEY>" %>%
  str_replace(pattern = "<INSERT KEY>", replacement = key)

resp <-  httr::GET(url = url_trove)

converted <- jsonlite::fromJSON(rawToChar(resp$content))

dat <- converted$article$articleText

#----------------------------------------------------------------------------

#Extracts our the issue id's or the gazettes.
url_2 <- "https://api.trove.nla.gov.au/v2/newspaper/title/1326?key=<INSERT KEY>&include=years&range=20000101-20101230&reclevel=full&include=articletext"%>%
  str_replace(pattern = "<INSERT KEY>", replacement = key)

resp <-  httr::GET(url = url_2)

converted <- jsonlite::fromJSON(rawToChar(resp$content))


dat_x <- converted$newspaper$year$issue[[24]]

#-------------------------------------------------------------------------------
"244707345" # Page 11
"244707336" # Page 12


url_3 <- "https://api.trove.nla.gov.au/v2/newspaper/244707345?key=<INSERT KEY>&include=articletext"%>%
  str_replace(pattern = "<INSERT KEY>", replacement = key)

resp <-  httr::GET(url = url_3)

converted <- jsonlite::fromJSON(rawToChar(resp$content))

converted$article$articleText


# TRy searching

url_3 <- "https://api.trove.nla.gov.au/v2/result?key=<INSERT KEY>&zone=all&q=Commonwealth of Australia Gazette. Public Service (National : 1977 - 2007)"%>%
  str_replace(pattern = "<INSERT KEY>", replacement = key) %>%
  str_replace_all(pattern = " ", replacement = "%20")

resp <-  httr::GET(url = url_3)

converted <- jsonlite::fromJSON(rawToChar(resp$content))
