library(tidyverse)


# ------------------------------------------------------------------------------
# Interest cumulative amount by week days (per month)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    month_as_date = floor_date(date, "month"),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(month_as_date, week_day) |> 
  summarize(
    interest_amount = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = week_day, y = interest_amount)) +
  geom_col(width = .75) +
  geom_text(aes(label = interest_amount), vjust = -0.5, size = 3, color = "#666") +
  facet_wrap(~ month_as_date, labeller = as_labeller(month_year_labeller)) +
  labs(
    title = "Interest cumulative amount by week days (per month)",
    x = "Week day",
    y = "Interest count"
  )

save_plot("interest-cumulative-amount-40-by-week-day-per-month.png")
