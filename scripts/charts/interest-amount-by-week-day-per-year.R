library(tidyverse)


# ------------------------------------------------------------------------------
# Interest amount by week days (per year)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(year, week_day) |> 
  summarize(interest_amount = sum(amount)) |> 
  ggplot(aes(x = week_day, y = interest_amount)) +
  geom_col(width = .75) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Interest amount by week days (per year)",
    x = "Week day",
    y = "Interest count"
  )

# Save
ggsave("charts/interest-amount-by-week-day-per-year.png", width=30, height=20, units="cm", dpi=300)
