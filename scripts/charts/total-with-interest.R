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
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_amount), alpha = .5, fill = "#bada55") +
  geom_step(aes(y = total_amount), direction = "hv", color = "darkgreen") +
  geom_area(aes(y = total_interest), alpha = .5) +
  geom_step(aes(y = total_interest), direction = "hv", alpha = .5, color = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount with total interest by date (ever)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/total-amount-with-total-interest-by-date-ever.png", width=30, height=20, units="cm", dpi=300)
