library(tidyverse)
library(lubridate)


# ------------------------------------------------------------------------------
# Interest cumulative amount daily (per month)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(date),
    month = floor_date(date, "month"),
    day = day(date)
  ) |>
  ggplot(aes(x = day)) +
  geom_col(aes(y = interest_amount)) +
  facet_wrap(~ month, labeller = as_labeller(month_year_labeller)) +
  labs(
    title = "Interest cumulative amount by day (per month)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("interest-cumulative-amount-by-day-per-month.png")
