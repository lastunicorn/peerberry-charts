library(tidyverse)
library(lubridate)

# ------------------------------------------------------------------------------
# Total amount with principal returned by date (ever)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    #amount_if_principal = ifelse(is.element(type, c("BUYBACK_PRINCIPAL", "REPAYMENT_PRINCIPAL", "DEPOSIT")), amount, 0),
    amount_if_principal = ifelse(is.element(type, c("BUYBACK_PRINCIPAL", "REPAYMENT_PRINCIPAL")), amount, 0),
    amount_if_added = ifelse(is.element(type, c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_amount = map_dbl(date, ~ sum(amount_if_added[date <= .x])),
    .keep = "used"
  ) |>
  uncount(2) |> 
  mutate(
    total_amount = lag(total_amount),
    amount_if_principal = if_else(row_number() %% 2 == 0, amount_if_principal, 0)
  ) |> 
  slice_tail(n = -1) |> 
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_amount), fill = "#D4E79E") +
  geom_step(aes(y = total_amount), direction = "hv", color = "#7E8C40") +
  geom_col(aes(y = amount_if_principal)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount with principal returned by date (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("total-amount-with-principal-by-date.png")
