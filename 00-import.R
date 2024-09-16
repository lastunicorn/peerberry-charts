library(tidyverse)

pb <- read_csv2(
  "peerberry-exports/investments - finished - 2024 09 02.csv",
  col_types = list(
    col_character(),
    col_character(),
    col_character(),
    col_character(),
    col_date(),
    col_factor(c("SHORT", "BUSINESS")),
    col_double(),
    col_double(),
    col_date(),
    col_date(),
    col_integer(),
    col_double(),
    col_factor(c("FINISHED"))
    )) |>
  janitor::clean_names()

problems()

pb
