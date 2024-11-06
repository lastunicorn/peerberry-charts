library(tidyverse)


# ------------------------------------------------------------------------------
# Interest count by week days (per year)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(year, week_day) |> 
  summarize(
    count = n(),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = week_day, y = count)) +
  geom_col(width = .75) +
  geom_text(
    aes(label = count),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Interest count by week days (per year)",
    x = "Week day",
    y = "Interest count"
  )

save_plot("interest-count-02-by-week-day-per-year.png")
