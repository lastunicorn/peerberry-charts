library(tidyverse)


# ------------------------------------------------------------------------------
# Import Finished

pb <- read_csv2(
  "data/pb-exports/investments - finished.csv",
  col_types = list(
    col_character(), # Loan ID (1)
    col_character(), # Country (2)
    col_character(), # Originator (3)
    col_character(), # Brand name (4)
    col_date(), # Date of purchase (5)
    col_factor(c("SHORT", "BUSINESS")), # Loan type (6)
    col_double(), # Interest rate (7)
    col_double(), # Invested amount (8)
    col_date(), # Final payment date according to the schedule (9)
    col_date(), # Date of received payment (10)
    col_integer(), # Term (11)
    col_double(), # Received payments (12)
    col_factor(c("FINISHED")) # Status (13)
  )) |>
  rename("last_received_payment_date" = "Date of received payment") |>  # rename col 10 to match col 14 from "in-progress" data frame
  select(c(1, 2, 4, 5, 6, 8, 10, 12, 13)) |> 
  janitor::clean_names() |> 
  relocate(last_received_payment_date, .after = received_payments)

problems()


# ------------------------------------------------------------------------------
# Import In-Progress

pb <- read_csv2(
  "data/pb-exports/investments - in progress.csv",
  col_types = list(
    col_character(), # Loan ID (1 = 1)
    col_character(), # Country (2 = 2)
    col_character(), # Originator (3 = 3)
    col_character(), # Brand name (4 = 4)
    col_date(), # Date of purchase (5 = 5)
    col_factor(c("SHORT", "BUSINESS")), # Loan type (6 = 6)
    col_double(), # Interest rate (7 = 7)
    col_double(), # Invested amount (8 = 8)
    col_date(), # Estimated final payment date (9 = 9)
    col_date(), # Estimated next payment date (10 = x)
    col_double(), # Estimated next payment (principal) (11 = x)
    col_double(), # Estimated next payment (interest) (12 = x)
    col_integer(), # Left term till estimated payment date (13 = 11)
    col_double(), # Received payments (14 = 12)
    col_date(), # Last received payment date (15 = 10)
    col_double(), # Remaining principal (16 = x)
    col_factor(c("FINISHED", "CURRENT", "LATE")) # Status (17)
  )) |>
  select(c(1, 2, 4, 5, 6, 8, 14, 15, 17)) |> 
  janitor::clean_names() |> 
  rbind(pb)

problems()
