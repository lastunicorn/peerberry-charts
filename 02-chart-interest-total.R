library(tidyverse)
library(lubridate)


# ------------------------------------------------------------------------------
# Total interest by date

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount3 = ifelse(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_interest = map_dbl(date, ~ sum(amount3[date <= .x])),
  ) |>
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_interest), alpha = .5) +
  geom_step(aes(y = total_interest), direction = "hv", alpha = .5, color = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total interest (ever)",
    x = "Date",
    y = "Amount"
  )

# Save
ggsave("charts/interest-total-over-time.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# Total interest and total amount by date

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
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total interest vs. total amount (ever)",
    x = "Date",
    y = "Amount"
  )

# Save
ggsave("charts/interest-total-and-amount-total-over-time.png", width=30, height=20, units="cm", dpi=300)
