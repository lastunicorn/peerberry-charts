library(readxl)
library(tidyverse)


# ------------------------------------------------------------------------------
# Import Investments (Finished)

# col_character(), # Loan ID (1)
# col_character(), # Country (2)
# col_character(), # Originator (3)
# col_character(), # Brand name (4)
# col_date(), # Date of purchase (5)
# col_factor(c("SHORT", "BUSINESS")), # Loan type (6)
# col_double(), # Interest rate (7)
# col_double(), # Invested amount (8)
# col_date(), # Final payment date according to the schedule (9)
# col_date(), # Date of received payment (10)
# col_integer(), # Term (11)
# col_double(), # Received payments (12)
# col_factor(c("FINISHED")) # Status (13)


pb <- read_xlsx("data-raw/investments - finished.xlsx") |>
  select(c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13)) |> 
  janitor::clean_names() |>  
  rename("last_payment_date" = "date_of_received_payment") |>
  rename("estimated_final_payment_date" = "final_payment_date_according_to_the_schedule") |>
  relocate(last_payment_date, .after = received_payments) |> 
  mutate(
    loan_id = as.character(loan_id),
    date_of_purchase = as.Date(date_of_purchase),
    loan_type = as.factor(loan_type),
    estimated_final_payment_date = as.Date(estimated_final_payment_date),
    last_payment_date = as.Date(last_payment_date),
    status = as.factor(status)
  )


# ------------------------------------------------------------------------------
# Import Investments (In-Progress)

# col_character(), # Loan ID (1 = 1)
# col_character(), # Country (2 = 2)
# col_character(), # Originator (3 = 3)
# col_character(), # Brand name (4 = 4)
# col_date(), # Date of purchase (5 = 5)
# col_factor(c("SHORT", "BUSINESS")), # Loan type (6 = 6)
# col_double(), # Interest rate (7 = 7)
# col_double(), # Invested amount (8 = 8)
# col_date(), # Estimated final payment date (9 = 9)
# col_date(), # Estimated next payment date (10 = x)
# col_double(), # Estimated next payment (principal) (11 = x)
# col_double(), # Estimated next payment (interest) (12 = x)
# col_integer(), # Left term till estimated payment date (13 = 11)
# col_double(), # Received payments (14 = 12)
# col_date(), # Last received payment date (15 = 10)
# col_double(), # Remaining principal (16 = x)
# col_factor(c("FINISHED", "CURRENT", "LATE")) # Status (17)

pb <- read_xlsx("data-raw/investments - current.xlsx") |>
  select(c(1, 2, 4, 5, 6, 8, 9, 14, 15, 17)) |> 
  janitor::clean_names() |>  
  rename("last_payment_date" = "last_received_payment_date") |>
  mutate(
    loan_id = as.character(loan_id),
    date_of_purchase = as.Date(date_of_purchase),
    loan_type = as.factor(loan_type),
    estimated_final_payment_date = as.Date(estimated_final_payment_date),
    status = as.factor(status)
  ) |> 
  rbind(pb)
