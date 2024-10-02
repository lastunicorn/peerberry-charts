library(tidyverse)


# ------------------------------------------------------------------------------
# Interest daily average by month (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    date_as_month = as.Date(ISOdate(year(date), month(date), 1)),
    days_in_month = lubridate::days_in_month(date)
  ) |>
  group_by(date_as_month) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount) / first(days_in_month)) |>
  ggplot(aes(x = date_as_month, y = interest_amount)) +
  geom_col() +
  guides(x = guide_axis(angle = 60)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(
    title = "Interest daily average by month (ever)",
    x = "Month",
    y = "Amount (€)"
  )

# Save
ggsave("charts/interest-daily-average-by-month-ever.png", width=30, height=20, units="cm", dpi=300)
