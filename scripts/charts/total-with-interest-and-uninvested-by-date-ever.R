library(tidyverse)
library(lubridate)

# ------------------------------------------------------------------------------
# Total amount with interest and uninvested by date (ever)

pb_transactions |> 
  group_by(date) |> 
  summarize(
    amount_deposit_and_interest = sum(if_else(type %in% c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0)), # uninvested + invested + interest
    amount_deposit = sum(if_else(type %in% c("DEPOSIT"), amount, 0)), # uninvested + invested
    amount_uninvested = sum(if_else(type == "INVESTMENT", -amount, amount)) # uninvested
  ) |> 
  arrange(date) |> 
  mutate(
    amount_total = cumsum(amount_deposit_and_interest),
    amount_total_without_interest = cumsum(amount_deposit),
    amount_total_uninvested = cumsum(amount_uninvested)
  ) |> 
  uncount(2) |> 
  mutate(
    amount_total = lag(amount_total),
    amount_total_without_interest = lag(amount_total_without_interest),
    amount_total_uninvested = lag(amount_total_uninvested)
  ) |> 
  slice_tail(n = -1) |> 
  ggplot(aes(x = date)) +
  geom_area(aes(y = amount_total), fill = "#38D44B") +
  geom_step(aes(y = amount_total), direction = "hv", color = "#009412") +
  geom_area(aes(y = amount_total_without_interest), fill = "#D4E79E") +
  geom_step(aes(y = amount_total_without_interest), direction = "hv", color = "#7E8C40") +
  geom_area(aes(y = amount_total_uninvested), fill = "gray60") +
  geom_step(aes(y = amount_total_uninvested), direction = "hv", color = "gray20") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount with interest and uninvested by date (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("total-with-interest-and-uninvested-by-date-ever.png")
