#' Finds the Xpaths from "https://webarchive.nla.gov.au/tep/75984" where the
#' pdf file links are stored.
#'
#' @return
#' @export
#'
#' @examples
generate_xpaths_for_pdf_links <- function() {

  c = 0
  x_paths_returned <- c()
  index_ol <- 0

  for (i in 1:15) {

    index_ol <- i
    index_li <- 1

    for (j in 1:60) {

      index_li <- j
      c = c + 1
      x_paths_returned[c] <-
        glue::glue("/html/body/div[2]/div[2]/div/div[2]/div[1]/div[1]/div[1]/ol[{index_ol}]/li[{index_li}]")

    }
  }

  return(x_paths_returned)

}

#' This function will chunk your PDF into the sections you need to extract out the
#' details.
#'
#' @param links_pdf (character, URL) This is the URL where the pdf is stored
#'
#' @return
#' @export
#'
#' @examples
break_up_pdf_trove_gazette <- function(links_pdf) {

  pdf_file_link_read <- pdftools::pdf_text(pdf = links_pdf) %>%
    keep(~str_detect(.x, "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9]")) %>%
    keep(~str_detect(.x, "Engagement N|Engagement Details"))

  return(pdf_file_link_read)

}


#' This function takes in a a string object and extracts out the AGS number,
#' name, salary and level.
#'
#' @param text_to_strip (character) Use the following:
#'
#' pdf_file <- "PDF URL"
#'
#' dat <- pdftools::pdf_text(pdf = pdf_file) %>%
#'  keep(~str_detect(.x, "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9]"))
#'  keep(~str_detect(.x, "Engagement N|Engagement Details"))
#'
#'  This will return a character vector/list that you can run the function on
#'  and this will extract out all the relevant details.
#'
#' @return
#' @export
#'
#' @examples
extract_relavent_details_pdf <- function(text_to_strip) {

  test <- text_to_strip %>%
    str_split(pattern = "\n\n\n", n = Inf) %>%
    pluck(1)


  ags_num_detect <- "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9]"
  ags_name_detect <- "[A-Z][a-z]+ [A-Z][a-z]+\n[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9]"
  salary_detect <- "\\$[0-9]+,[0-9]+"
  department_name <- "Engagement N.N. [0-9]+\n\n(.*?)\n"

  ags_num_name <- test %>% str_extract(pattern = ags_name_detect)
  applicant_name <- ags_num_name %>%
    str_remove_all("[0-9]+|-|\n")
  ags_number <- test %>% str_extract(pattern = ags_num_detect)
  salary_value <- test %>% str_extract(pattern = salary_detect)
  level_value <- test %>%
    str_extract("Classification:(.*?),|Classification:(.*?)\n|Broadband:(.*?)\n|Broadband:(.*?),") %>%
    str_remove_all(",|\n") %>%
    str_remove_all("Classification:|Broadband:") %>%
    str_trim()
  department <- test %>% str_extract(pattern = department_name) %>%
    str_remove_all(pattern = "Engagement N.N. [0-9]+\n\n") %>%
    str_remove_all(pattern = "\n")

  returned_value <-
    tibble(
      ags_number = ags_number,
      applicant_name = applicant_name,
      salary = salary_value,
      level = level_value,
      department = department,
      raw_text_row = test
    )

  return(returned_value)

}

#' This function wraps extract_relavent_details_pdf across pdf url links to
#' extract out the data you need.
#'
#' @param pdf_link (character) You can either scrape these again check the file
#' code/download_trove_pdfs.R. Or you can read in data/pdf_links_trove.csv
#' which has all the links scraped and ready for data extraction.
#'
#' @return
#' @export
#'
#' @examples
extract_details_from_tove_pdf <- function(pdf_link = "data/gazette_2007_08_09.pdf") {

  dat <- pdftools::pdf_text(pdf = pdf_link) %>%
    keep(~str_detect(.x, "[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9]")) %>%
    keep(~str_detect(.x, "Engagement N|Engagement Details"))

  safely_extract_relavent_details <- purrr::safely(extract_relavent_details_pdf,
                                                   otherwise = NULL)

  all_data <-
    dat %>%
    map(
      ~ .x %>%
        safely_extract_relavent_details() %>%
        pluck('result')
    ) %>%
    keep(~!is.null(.x)) %>%
    map_dfr(bind_rows) %>%
    filter(!is.na(ags_number))

  return(all_data)


}
