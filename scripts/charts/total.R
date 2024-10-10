library(tidyverse)
library(lubridate)

# ------------------------------------------------------------------------------
# Total amount (ever)

pb_transactions |> 
  group_by(date) |> 
  summarize(
    amount_added = sum(if_else(type %in% c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0))
  ) |> 
  arrange(date) |> 
  mutate(
    amount_total = cumsum(amount_added)
  ) |> 
  ggplot(aes(x = date)) +
  geom_area(aes(y = amount_total), fill = "#D4E79E") +
  geom_step(aes(y = amount_total), direction = "hv", color = "#7E8C40") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("total-amount-ever.png")
