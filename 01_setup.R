pkgs <- c("tidyverse","readr","data.table","janitor","lubridate",
          "stringr","scales","broom","performance","here","ggrepel")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install, dependencies = TRUE)

library(tidyverse); library(readr); library(data.table); library(janitor)
library(lubridate); library(stringr); library(scales); library(broom)
library(performance); library(here); library(ggrepel)

default_locale <- locale(encoding = "CP949")
p <- function(fname) here("data", fname)

extract_gu <- function(x){
  x <- as.character(x)
  out <- str_extract(x, "([가-힣]+구)")
  out <- str_replace_all(out, "\\s", "")
  out
}

message("환경 준비 완료됨")

# 자동 인코딩 감지 + 실패 시 재시도
read_kor_csv <- function(path, guess_max = 100000) {
  try_cp <- try(
    readr::read_csv(path, locale = readr::locale(encoding = "CP949"),
                    guess_max = guess_max),
    silent = TRUE
  )
  if (!inherits(try_cp, "try-error")) {
    df <- try_cp
  } else {
    df <- readr::read_csv(path, locale = readr::locale(encoding = "UTF-8"),
                          guess_max = guess_max)
  }
  df <- df %>% dplyr::mutate(dplyr::across(where(is.character),
                                           stringi::stri_enc_toutf8))
  return(df)
}

safe_clean <- function(df){
  df %>%
    janitor::clean_names() %>%
    dplyr::rename_with(.fn = ~ stringi::stri_replace_all_regex(.x, "[^a-z0-9_]", "_"))
}