library(dplyr)


# ------------------------------------------------------------------------------
# Uninvested amount by month (ever)

pb_transactions |>
  arrange(date) |>
  mutate(
    amount_with_sign = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount_with_sign[date < .x])),
    month_and_year = as.Date(ISOdate(year(date), month(date), 1)),
    days_in_month = lubridate::days_in_month(date)
  ) |>
  group_by(month_and_year) |>
  summarize(uninvested_amount = sum(amount) / mean(days_in_month)) |>
  ggplot(aes(x = month_and_year, y = uninvested_amount)) +
  geom_col() +
  guides(x = guide_axis(angle = 60)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Cash drag (uninvested amount) average by month (ever)",
    x = "Month",
    y = "Amount (€)"
  )

# Save
ggsave("charts/uninvested-amount-by-month-ever.png", width=30, height=20, units="cm", dpi=300)
