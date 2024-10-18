library(tidyverse)
library(lubridate)

# ------------------------------------------------------------------------------
# Total amount (ever)

pb_transactions |> 
  group_by(date) |> 
  summarize(
    total_amount = sum(if_else(type %in% c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0))
  ) |> 
  arrange(date) |> 
  mutate(
    total_amount = cumsum(total_amount)
  ) |> 
  uncount(2) |> 
  mutate(
    total_amount = lag(total_amount)
  ) |> 
  slice_tail(n = -1) |> 
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_amount), fill = "#D4E79E") +
  geom_step(aes(y = total_amount), direction = "hv", color = "#7E8C40") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("total-amount-ever.png")
