library(tidyverse)
library(lubridate)

# ------------------------------------------------------------------------------
# Total amount with all by date (ever)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount_interest = if_else(type %in% c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0), # uninvested + invested + interest
    amount_invested = if_else(type == "DEPOSIT", amount, 0), # uninvested + invested
    amount_uninvested = if_else(type == "INVESTMENT", -amount, amount), # uninvested
    total_interest = map_dbl(date, ~ sum(amount_interest[date <= .x])),
    total_invested = map_dbl(date, ~ sum(amount_invested[date <= .x])),
    total_uninvested = map_dbl(date, ~ sum(amount_uninvested[date <= .x]))
  ) |>
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_interest), fill = "#38D44B") +
  geom_step(aes(y = total_interest), direction = "hv", color = "#009412") +
  geom_area(aes(y = total_invested), fill = "#D4E79E") +
  geom_step(aes(y = total_invested), direction = "hv", color = "#7E8C40") +
  geom_area(aes(y = total_uninvested), fill = "gray60") +
  geom_step(aes(y = total_uninvested), direction = "hv", color = "gray20") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount with all by date (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("total-amount-with-all-by-date-ever.png")