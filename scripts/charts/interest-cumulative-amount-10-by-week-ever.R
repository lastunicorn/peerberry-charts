library(tidyverse)


# ------------------------------------------------------------------------------
# Interest cumulative amount by week (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    week_as_date = floor_date(date, "week")
  ) |>
  group_by(week_as_date) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount)) |>
  ggplot(aes(x = week_as_date, y = interest_amount)) +
  geom_col() +
  guides(x = guide_axis(angle = 60)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(
    title = "Interest cumulative amount by week (ever)",
    x = "Week",
    y = "Amount (€)"
  )

save_plot("interest-cumulative-amount-10-by-week-ever.png")
