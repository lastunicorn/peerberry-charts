library(tidyverse)
library(lubridate)


# ------------------------------------------------------------------------------
# Interest daily (ever)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = interest_amount)) +
  geom_smooth(aes(y = interest_amount)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Interest daily (ever)",
    x = "Date",
    y = "Amount"
  )

# Save
ggsave("charts/interest-over-time.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# One chart per month:
#   - Interest (daily)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(date),
    month = format_ISO8601(date, precision = "ym"),
    day = day(date)
  ) |>
  ggplot(aes(x = day)) +
  geom_col(aes(y = interest_amount)) +
  facet_wrap(~ month) +
  labs(
    title = "Interest daily (per month)",
    x = "Date",
    y = "Amount"
  )

# Save
ggsave("charts/interest-over-time-per-month.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# One chart per year:
#   - Interest daily (per year)

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
  geom_smooth(aes(y = interest_amount)) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Interest daily (per year)",
    x = "Date",
    y = "Amount"
  )

# Save
ggsave("charts/interest-over-time-per-year.png", width=30, height=20, units="cm", dpi=300)

