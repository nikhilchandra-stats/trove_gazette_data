dates_x <-
c(
"jan_08_2004",
"feb_13_2004",
"mar_13_2004",
"apr_01_2004",
"may_06_2004",
"jun_03_2004",
"jul_01_2004",
"aug_04_2004",
"sep_02_2004",
"oct_07_2004",
"nov_04_2004",
"dec_02_2004",

"jan_12_2006",
"feb_02_2006",
"mar_02_2006",
"apr_06_2006",
"may_04_2006",
"jun_01_2006",
"jul_06_2006",
"aug_03_2006",
"sep_07_2006",
"oct_05_2006",
"nov_02_2006",
"dec_07_2006",

"jan_11_2007",
"feb_01_2007",
"mar_01_2007",
"apr_05_2007",
"may_03_2007",
"jun_07_2007",
"jul_05_2007",
"aug_02_2007"
)

article_dates <-
  c(
    248308392,
    248308790,
    248309133,
    248309431,
    244487069,
    244487470,
    244487806,
    244668892,
    244669280,
    244561736,
    244562118,
    244562493,

    244953694,
    244954070,
    244996505,
    244996984,
    244886931,
    244887358,
    244887883,
    244888307,
    244888844,
    244997612,
    244998066,
    245022483,

    245022810,
    245023133,
    245023570,
    244889375,
    245023980,
    245024520,
    245024934,
    245025391
  )

pages <- c(
  162,
  690 -555,
  1232 - 1095,
  1732 - 1601,
  2324- 2191,
  2914 - 2765,
  3452 - 3319,
  4198 - 4055,
  4786 - 4661,
  5512 - 5381,
  6156 - 5999,
  6842 - 6683,

  344,
  964 - 797,
  1858 - 1629,
  2870 - 2637,
  3580 - 3417,
  4432 - 4225,
  5280 - 5061,
  6150 - 5961,
  7287 - 7035,
  8165 - 7977,
  9098 - 8870,
  10270 - 10015,

  256,
  922 - 714,
  1789 - 1581,
  2828 - 2627,
  3626 - 3414,
  4772 - 4557,
  5708 - 5475,
  6653 - 6423
)


x_tibble <-
  tibble(
  dates_x = dates_x,
  url_replace = article_dates,
  pages = pages
  )

url_base <- "https://trove.nla.gov.au/newspaper/rendition/nla.news-articlexxxxxxxxx.txt"
replace_val <- "xxxxxxxxx"

extract_func <- function(url_to_use) {


  dat <- xml2::read_html(url_to_use) %>%
    rvest::html_text2() %>%
    as.character() %>%
    str_split(pattern = "\n\nN.N", n = Inf) %>%
    pluck(1) %>%
    as_tibble() %>%
    mutate(
      value = str_remove_all(value, "\\!")
    ) %>%
    mutate(applicant_name =
             str_extract(value, "[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+: [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+; [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+. [0-9]+-[0-9]+")) %>%
    mutate(applicant_name =
             case_when(
               is.na(applicant_name) ~ str_extract(value, "[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+: [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+; [0-9]+-[0-9]+|[A-Z][a-zA-Z]+ [A-Z][a-zA-Z]+. [0-9]+-[0-9]+"),
               TRUE ~ applicant_name
             )) %>%
    mutate(
      ags_number =
        str_extract(applicant_name, ": [0-9]+-[0-9]+|; [0-9]+-[0-9]+|. [0-9]+-[0-9]+"),
      applicant_name = str_remove_all(applicant_name, ": [0-9]+-[0-9]+|; [0-9]+-[0-9]+|. [0-9]+-[0-9]+"),
    ) %>%
    mutate(across(c(ags_number,applicant_name), .fns = ~ str_remove_all(., ":|;"))) %>%
    mutate(salary = str_extract(value, "\\$[0-9]+-[0-9]+,|5[0-9]+-[0-9]+,|S[0-9]+-[0-9]+,") %>%
             str_remove_all("[a-zA-Z]|$|,")) %>%
    mutate(
      salary =
        case_when(
          str_detect(salary, "5[0-9]+-[0-9]+") & (str_detect(salary, "[a-zA-Z]+ 5")|str_detect(salary, "[0-9] 5")) ~ str_remove(salary,"5"),
          TRUE ~ salary
        )
    ) %>%
    mutate(
      level = str_extract(value, "APS[0-9]|APS Level [0-9]|APS level [0-9]")
    ) %>%
    mutate(value =
             case_when(
               str_detect(value, "[a-zA-Z]\\^") ~ str_replace(value, pattern = "\\^", replacement = "e"),
               TRUE ~ value
             )) %>%
    mutate(
      level =
        case_when(
          is.na(level) & str_detect(value, "EL2|Director|director|Executive (?i)Level II|Executive (?i)Level 2") ~ "EL2",
          is.na(level) & str_detect(value, "EL1|Assistant Director|Assistant director|assistant director|Executive (?i)Level I|Executive (?i)Level 1|Executive level 1") ~ "EL1",
          TRUE ~ level
        )
    ) %>%
    mutate(department =
             case_when(
               str_detect(value, "Finance") & !str_detect(value, "ELECTORAL|Electoral|electoral") ~ "Department of Finance",
               str_detect(value, "ELECTORAL|Electoral|electoral") ~ "AEC",
               str_detect(value, "Treasury") ~ "Treasury",
               str_detect(value, "Prime Minister|PMC|PM&C") ~ "PMC",
             )) %>%
    fill(department, .direction = "down") %>%
    rename(raw_text_row = value)

  return(dat)


}

produce_article_ids <-
  x_tibble %>%
  split(.$dates_x) %>%
  map(
    ~ tibble(
      dates_x = rep(.x[1,1] %>% as.character(), .x[1,3] %>% as.numeric()),
      url_replace  = rep(.x[1,2] %>% as.numeric(), .x[1,3] %>% as.numeric()),
      pages = seq(.x[1,2] %>% as.numeric(), .x[1,2] %>% as.numeric() + .x[1,3] %>% as.numeric() - 1, 1)
  )
  )

dat_comp_list <- list()

extract_safely <- safely(extract_func, otherwise = NULL)

c = 0

for (j in 1:dim(x_tibble)[1]) {

  pages_x <- 1:x_tibble$pages[j]
  url_num <- x_tibble$url_replace[j]
  dat_current <- x_tibble$dates_x[j]

  for (i in pages_x) {

    c = c + 1
    Sys.sleep(0.5)

    article_num <- (url_num + i) %>% as.character()
    url_replacement <- url_base %>%
      str_replace(pattern = "xxxxxxxxx", replacement = article_num)

    dat_x <- extract_safely(url_to_use = url_replacement) %>%
      pluck("result")

    if(any(class(dat_x) == "tbl_df")) {

      dat_x <- dat_x %>%
        mutate(date = dat_current)

    }

    dat_comp_list[[c]] <- dat_x

  }

}


