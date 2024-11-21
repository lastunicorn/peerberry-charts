library(tidyverse)


# ------------------------------------------------------------------------------
# Interest cumulative amount by week days (per year)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(year, week_day) |> 
  summarize(
    interest_amount = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = week_day, y = interest_amount)) +
  geom_col(width = .75) +
  geom_text(
    aes(label = interest_amount),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Interest cumulative amount by week days (per year)",
    x = "Week day",
    y = "Amount (€)"
  )

save_plot("interest-cumulative-amount-41-by-week-day-per-year.png")
