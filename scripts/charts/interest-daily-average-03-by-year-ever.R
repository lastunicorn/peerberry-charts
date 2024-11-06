library(tidyverse)


# ------------------------------------------------------------------------------
# Interest daily average by year (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    year = as.factor(year),
    days_in_year = (make_date(year, 01, 01) %--% make_date(as.numeric(year) + 1, 01, 01)) / days(1)
  ) |>
  group_by(year) |>
  arrange(year) |>
  summarize(interest_amount = sum(amount) / first(days_in_year)) |>
  ggplot(aes(x = year, y = interest_amount)) +
  geom_col(width = .5) +
  geom_text(
    aes(label = format(round(interest_amount, 2), nsmall = 2)),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Interest daily average by year (ever)",
    x = "Year",
    y = "Amount (€)"
  )

save_plot("interest-daily-average-03-by-year-ever.png")
