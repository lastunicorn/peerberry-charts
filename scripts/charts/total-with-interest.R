library(tidyverse)
library(lubridate)

# ------------------------------------------------------------------------------
# Total amount with total interest by date (ever)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount2 = ifelse(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_interest = map_dbl(date, ~ sum(amount2[date <= .x])),
    amount3 = ifelse(is.element(type, c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_amount = map_dbl(date, ~ sum(amount3[date <= .x])),
  ) |>
  uncount(2) |> 
  mutate(
    total_interest = lag(total_interest),
    total_amount = lag(total_amount)
  ) |> 
  filter(!is.na(total_interest) & !is.na(total_amount)) |> 
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_amount), fill = "#D4E79E") +
  geom_step(aes(y = total_amount), direction = "hv", color = "#7E8C40") +
  geom_area(aes(y = total_interest), fill = "#38D44B") +
  geom_step(aes(y = total_interest), direction = "hv", color = "#009412") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount with total interest by date (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("total-amount-with-total-interest-by-date-ever.png")
