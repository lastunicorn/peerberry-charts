library(tidyverse)
library(lubridate)


# ------------------------------------------------------------------------------
# Interest cumulative amount daily (per year)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  add_row(
    date = as.Date("2023-01-01"),
    amount = 0
  ) |>
  add_row(
    date = as.Date("2024-12-31"),
    amount = 0
  ) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(date)
  ) |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = interest_amount)) +
  geom_smooth(aes(y = interest_amount), method = 'loess', formula = 'y ~ x') +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(
    title = "Interest cumulative amount by day (per year)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/interest-cumulative-amount-by-day-per-year.png", width=30, height=20, units="cm", dpi=300)
