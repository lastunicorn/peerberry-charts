library(tidyverse)


# ------------------------------------------------------------------------------
# Interest cumulative amount by week (per year)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    week_as_date = floor_date(date, "week")
  ) |>
  add_row(
    week_as_date = generate_weekly_dates(pb_transactions.first_year_as_date, pb_transactions.last_year_as_date |>  last_week_of_year()),
    amount = 0
  ) |>
  group_by(week_as_date) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(week_as_date)
  ) |> 
  ggplot(aes(x = week_as_date, y = interest_amount)) +
  geom_col() +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    minor_breaks = NULL
  ) +
  scale_y_continuous(n.breaks = 15, minor_breaks = F) +
  labs(
    title = "Interest cumulative amount by week (per year)",
    x = "Week (date)",
    y = "Amount (€)"
  )

save_plot("interest-cumulative-amount-11-by-week-per-year.png")
