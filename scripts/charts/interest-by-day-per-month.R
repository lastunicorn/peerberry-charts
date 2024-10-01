library(tidyverse)
library(lubridate)


# ------------------------------------------------------------------------------
# One chart per month:
#   - Interest (daily)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(date),
    month = format_ISO8601(date, precision = "ym"),
    day = day(date)
  ) |>
  ggplot(aes(x = day)) +
  geom_col(aes(y = interest_amount)) +
  facet_wrap(~ month) +
  labs(
    title = "Interest daily (per month)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/interest-by-day-per-month.png", width=30, height=20, units="cm", dpi=300)
