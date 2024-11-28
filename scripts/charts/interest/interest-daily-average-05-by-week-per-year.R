library(tidyverse)


# ------------------------------------------------------------------------------
# Interest daily average amount by week days (per year)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |> 
  summarize(
    amount = sum(amount)
  ) |> 
  mutate(
    year = year(date),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(year, week_day) |> 
  summarize(
    interest_average = mean(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = week_day, y = interest_average)) +
  geom_col(width = .75) +
  geom_text(
    aes(label = format(round(interest_average, 2), nsmall = 2)),
    vjust = -0.5,
    size = 3,
    color = "#666"
  ) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Interest daily average amount by week days (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Week day",
    y = "Amount (€)"
  )

save_plot("interest-daily-average-05-by-week-day-per-year.png")
