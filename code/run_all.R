library(tidyverse)
library(purrr)
library(pdftools)
library(rvest)

helpeR::load_custom_functions()

pdf_urls_local <- read_csv("data/pdf_links_trove.csv", trim_ws = T) %>%
  pull(pdf_links_trove)


dat <- list()

safely_extract_all_pdf <- safely(.f = extract_details_from_tove_pdf,
                                 otherwise = NULL)

for (i in 1:length(pdf_urls_local)) {

  rand_wait <- runif(n = 1, min = 1, max = 2) %>%
    round(digits = 2)
  
  date_from_url <- pdf_urls_local[i] %>% 
    str_extract(pattern = "[0-9]+mp_") %>% 
    str_extract("[0-9]+")
  
  date_from_url <- substr(date_from_url, 1, 8) %>% 
    lubridate::as_date()

  Sys.sleep(rand_wait)

  dat_x <- safely_extract_all_pdf(pdf_urls_local[i]) %>%
    pluck('result')

  if(!is.null(dat_x)) {
    dat_x <- dat_x %>% 
      mutate(date = date_from_url)
  }
  
  dat[[i]] <- dat_x
  
}

trove_pdf_data <- dat %>%
  keep(~!is.null(.x)) %>%
  map_dfr(bind_rows)

write.csv(trove_pdf_data, file = "latest_trove_gazette_2007_2019.csv", row.names = F)

