library(tidyverse)


# ------------------------------------------------------------------------------
# Interest daily average by month (per year)

pb_transactions |>
  filter(type %in% c("BUYBACK_INTEREST", "REPAYMENT_INTEREST")) |>
  mutate(
    month_as_date = floor_date(date, "month")
  ) |>
  add_row(
    month_as_date = generate_monthly_dates(pb_transactions.first_year_as_date, pb_transactions.last_year_as_date |> last_month_of_year()),
    amount = 0
  ) |>
  group_by(month_as_date) |>
  arrange(date) |>
  summarize(
    amount = sum(amount)
  ) |>
  mutate(
    year = as.factor(year(month_as_date)),
    days_in_month = pb_transactions.days_in_month(month_as_date),
    interest_amount = amount / days_in_month
  ) |> 
  ggplot(aes(x = month_as_date, y = interest_amount)) +
  geom_col() +
  geom_text(
    aes(label = if_else(interest_amount == 0, NA, format(round(interest_amount, 2), nsmall = 2))),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    minor_breaks = NULL
  ) +
  labs(
    title = "Interest daily average by month (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Month",
    y = "Amount (€)"
  )

save_plot("interest-daily-average-02-by-month-per-year.png")
