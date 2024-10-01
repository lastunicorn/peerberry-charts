library(tidyverse)


# ------------------------------------------------------------------------------
# Interest by month (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    month_and_year = format_ISO8601(date, precision = "ym")
  ) |>
  group_by(month_and_year) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount)) |>
  ggplot(aes(x = month_and_year, y = interest_amount)) +
  geom_col() +
  geom_smooth(aes(x = month_and_year, y = interest_amount), formula = y ~ x, method = "loess") +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Interest by month (ever)",
    x = "Month",
    y = "Amount (€)"
  )

# Save
ggsave("charts/interest-by-month-ever.png", width=30, height=20, units="cm", dpi=300)
