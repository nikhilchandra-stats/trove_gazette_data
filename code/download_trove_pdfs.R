library(cah)
library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)

#----------------------Detects if you are on pipeline or VM
local_testing <- TRUE


# If on VM run local selenium driver using firefox
if(local_testing){

  driver <- rsDriver(browser = c("firefox"),
                     port = 4580L, chromever = "108.0.5359.71")

  remote_driver <- driver[["client"]]

  remote_driver$open()


}

# If on Pipeline run run the following to engage selenium
if(local_testing == FALSE){

  server_address = "selenium"

  server_port = 4444L

  remote_driver <- RSelenium::remoteDriver(remoteServerAddr = server_address,
                                           port = server_port,
                                           browserName = "chrome")
  remote_driver$open()

}


html_url <- "https://webarchive.nla.gov.au/tep/75984"

xpaths_all <- generate_xpaths_for_pdf_links()


remote_driver$navigate(html_url)

html_read_in <- remote_driver$getPageSource() %>%
                  pluck(1) %>%
                  xml2::read_html()


extract_pdf_links <- function(html_read_in,
                              xpath_of_pdf = xpaths_all[1]) {

  html_raw <- html_read_in%>%
    rvest::html_element(xpath = xpath_of_pdf) %>%
    rvest::html_elements("a") %>%
    html_attrs()

  links_pdf <- html_raw[[1]] %>%
    pluck("data-thumbnailurl")

  return(links_pdf)

}

safely_get_pdf_links <- safely(extract_pdf_links, otherwise = NULL)

all_pdf_links <- xpaths_all %>%
  map( ~ safely_get_pdf_links(html_read_in, .x) %>%
         pluck('result')) %>%
  keep(~!is.null(.x)) %>%
  unlist()

pdf_links_tibble <-
  tibble(pdf_links_trove = all_pdf_links)

write.csv(x = pdf_links_tibble, file = "data/pdf_links_trove.csv", row.names = F)

try(remote_driver$quit())
