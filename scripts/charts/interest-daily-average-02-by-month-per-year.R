library(tidyverse)


# ------------------------------------------------------------------------------
# Interest daily average by month (per year)

pb_transactions |>
  mutate(
    start_date = min(date),
    end_date = max(date) + days(1)
  ) |> 
  filter(type %in% c("BUYBACK_INTEREST", "REPAYMENT_INTEREST")) |>
  mutate(
    month_as_date = floor_date(date, "month")
  ) |>
  add_row(
    month_as_date = generate_monthly_dates(floor_date(start_date, "year"), ceiling_date(end_date, "year") - months(1)),
    amount = 0
  ) |>
  group_by(month_as_date) |>
  arrange(date) |>
  mutate(
    month_start_date = floor_date(date, "month"),
    month_start_date = if_else(month_start_date < start_date, start_date, month_start_date),
    month_end_date = ceiling_date(date, "month"),
    month_end_date = if_else(month_end_date > end_date, end_date, month_end_date),
    days_in_month = (month_start_date %--% month_end_date) / days(1)
  ) |> 
  summarize(
    interest_amount = sum(amount) / first(days_in_month),
    year = as.factor(first(year(month_as_date)))
  ) |>
  ggplot(aes(x = month_as_date, y = interest_amount)) +
  geom_col() +
  geom_text(aes(label = format(round(interest_amount, 2), nsmall = 2)), vjust = -0.5, size = 3) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(
    title = "Interest daily average by month (per year)",
    x = "Month",
    y = "Amount (€)"
  )

save_plot("interest-daily-average-01-by-month-per-year.png")
